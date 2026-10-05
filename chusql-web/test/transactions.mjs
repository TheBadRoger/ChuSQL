import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import { beforeEach, test } from 'node:test';
import * as api from '../static/api.js';
import { createEmptyChangeSet } from '../static/core.js';

// 验证页面编辑状态、原子请求、断线重试与关闭清理。

const listeners = new Map();
globalThis.window = { addEventListener: (name, callback) => listeners.set(name, callback), dispatchEvent: () => {} };
globalThis.document = { getElementById: (id) => id === 'root' ? { addEventListener() {} } : null };
let source = await readFile(new URL('../static/app.js', import.meta.url), 'utf8');
source = source.replaceAll("'./api.js'", JSON.stringify(new URL('../static/api.js', import.meta.url).href))
  .replaceAll("'./core.js'", JSON.stringify(new URL('../static/core.js', import.meta.url).href))
  .replace('void boot();', 'export { state, commitAll, rollbackAll, stageUpdate, removeStaged, ensureEdit, runSql };');
const app = await import(`data:text/javascript;base64,${Buffer.from(source).toString('base64')}`);
let calls;
let commitResponse;

// 生成 JSON HTTP 响应。
function response(body, status = 200) {
  return new Response(JSON.stringify(body), { status, headers: { 'Content-Type': 'application/json' } });
}

// 重置页面与模拟服务端。
beforeEach(() => {
  calls = [];
  commitResponse = () => response({ ok: true, applied: 1, state: 'committed' });
  api.forgetEdit();
  Object.assign(app.state, { database: 'test', databases: ['test'], tabs: [], activeId: undefined,
    view: 'workbench', session: { user: 'admin' }, edit: { id: 'page', database: 'test' },
    editBusy: false, editStarting: null, editAttempt: null, editNeedsReload: false, editUncertain: null,
    changes: createEmptyChangeSet(), messages: [] });
  globalThis.fetch = async (url, init) => {
    calls.push({ url, init, body: init.body ? JSON.parse(init.body) : undefined });
    if (url === '/api/edit/commit') return commitResponse();
    if (url === '/api/databases') return response(['test']);
    if (url === '/api/tables') return response([]);
    return response({ state: 'active' });
  };
});

// 检查整批修改只发送一次提交请求。
test('all table edits use one atomic request and clear only after success', async () => {
  app.stageUpdate('first', 'test', 1, 'value', 10, 20);
  app.stageUpdate('second', 'test', 2, 'value', 20, 30);
  await app.commitAll();
  const committed = calls.filter((call) => call.url === '/api/edit/commit');
  assert.equal(committed.length, 1);
  assert.equal(committed[0].body.changes.length, 2);
  assert.equal(app.state.changes.updates.size, 0);
  assert.equal(app.state.edit, null);
  assert.equal(calls.some((call) => call.init.method === 'PATCH'), false);
});

// 检查失败保留可修正的完整编辑集。
test('constraint failure retains all edits and permits local undo before replay', async () => {
  app.stageUpdate('first', 'test', 1, 'value', 10, 20);
  app.stageUpdate('second', 'test', 2, 'value', 20, 30);
  commitResponse = () => response({ ok: false, applied: 0, state: 'active', message: 'constraint', reloadRequired: false }, 400);
  await app.commitAll();
  assert.equal(app.state.changes.updates.size, 2);
  assert.equal(app.state.editNeedsReload, false);
  app.removeStaged([...app.state.changes.updates.keys()][0]);
  assert.equal(app.state.changes.updates.size, 1);
  assert.equal(calls.some((call) => call.init.method === 'PATCH'), false);
});

// 检查冲突禁止再次提交直到回滚。
test('serialization conflict freezes edits and requires rollback and reload', async () => {
  app.stageUpdate('first', 'test', 1, 'value', 10, 20);
  commitResponse = () => response({ ok: false, state: 'rolled_back', reloadRequired: true, message: 'conflict' }, 409);
  await app.commitAll();
  await app.commitAll();
  assert.equal(calls.filter((call) => call.url === '/api/edit/commit').length, 1);
  assert.equal(app.state.changes.updates.size, 1);
  app.stageUpdate('first', 'test', 1, 'value', 10, 40);
  assert.equal([...app.state.changes.updates.values()][0].newValue, 20);
  await app.rollbackAll();
  assert.equal(app.state.changes.updates.size, 0);
  assert.equal(app.state.editNeedsReload, false);
  assert.equal(calls.some((call) => call.url === '/api/edit/rollback'), true);
});

// 检查断线后冻结载荷并使用同一提交标识。
test('an uncertain response retries the same payload without allowing edits', async () => {
  app.stageUpdate('first', 'test', 1, 'value', 10, 20);
  commitResponse = () => { throw new Error('connection lost'); };
  await app.commitAll();
  assert.ok(app.state.editUncertain);
  app.stageUpdate('first', 'test', 1, 'value', 10, 99);
  commitResponse = () => response({ ok: true, applied: 1, state: 'committed' });
  await app.commitAll();
  const committed = calls.filter((call) => call.url === '/api/edit/commit');
  assert.deepEqual(committed[0].body, committed[1].body);
  assert.equal(app.state.editUncertain, null);
});

// 检查并发点击只有一个正在提交的请求。
test('duplicate clicks cannot submit concurrently', async () => {
  app.stageUpdate('first', 'test', 1, 'value', 10, 20);
  await Promise.all([app.commitAll(), app.commitAll()]);
  assert.equal(calls.filter((call) => call.url === '/api/edit/commit').length, 1);
});

// 检查快照读取带页面标识且普通 SQL 无标识。
test('snapshot headers are attached only to table reads', async () => {
  await api.beginEdit('page', 'test');
  await api.listTables('test');
  globalThis.fetch = async (url, init) => {
    calls.push({ url, init });
    return response({ columns: [], rows: [], rowCount: 0 });
  };
  await api.query('SELECT 1', 'test');
  assert.equal(calls.find((call) => call.url === '/api/tables').init.headers.get('X-ChuSQL-Edit'), 'page');
  assert.equal(calls.find((call) => call.url === '/api/query').init.headers.has('X-ChuSQL-Edit'), false);
});

// 检查页面离开时发送回滚请求。
test('page close sends a rollback beacon for its own transaction', async () => {
  let beacon;
  Object.defineProperty(globalThis, 'navigator', { configurable: true, value: { sendBeacon: (url, body) => { beacon = { url, body }; } } });
  listeners.get('pagehide')();
  assert.equal(beacon.url, '/api/edit/rollback');
  assert.deepEqual(JSON.parse(await beacon.body.text()), { id: 'page', database: 'test' });
});

// 检查 BEGIN 回包断线后仍使用同一标识。
test('a lost BEGIN response keeps its identity for an idempotent retry', async () => {
  app.state.edit = null;
  globalThis.fetch = async (url, init) => {
    calls.push({ url, init, body: JSON.parse(init.body) });
    if (calls.length === 1) throw new Error('begin response lost');
    return response({ state: 'active' });
  };
  await assert.rejects(app.ensureEdit('test'));
  assert.ok(app.state.editAttempt);
  await app.ensureEdit('test');
  assert.deepEqual(calls[0].body, calls[1].body);
  assert.equal(app.state.edit.id, calls[0].body.id);
});

// 检查每个 SQL 标签使用独立所有者标识。
test('console transaction cleanup only applies to its owning tab', async () => {
  globalThis.fetch = async (url, init) => {
    calls.push({ url, init, body: JSON.parse(init.body) });
    return response({ columns: [], rows: [], rowCount: 0, transaction: true });
  };
  await api.query('BEGIN', 'test', 'first');
  const owner = calls[0].init.headers.get('X-ChuSQL-Console');
  assert.ok(owner.endsWith(':first'));
  await api.rollbackConsole('second');
  assert.equal(calls.length, 1);
  await api.rollbackConsole('first');
  assert.equal(calls[1].url, '/api/query/rollback');
  assert.equal(calls[1].body.console, owner);
});

// 检查 SQL 回滚等待期间不能继续编辑旧快照。
test('edits are locked while SQL releases the previous snapshot', async () => {
  let release;
  globalThis.fetch = async (url, init) => {
    calls.push({ url, init });
    if (url === '/api/edit/rollback') return new Promise((resolve) => { release = () => resolve(response({ state: 'rolled_back' })); });
    return response({ columns: ['value'], rows: [[1]], rowCount: 1 });
  };
  const job = app.runSql({ id: 'query', database: 'test', sql: 'SELECT 1' });
  assert.equal(app.state.editBusy, true);
  app.stageUpdate('first', 'test', 1, 'value', 10, 20);
  assert.equal(app.state.changes.updates.size, 0);
  release();
  await job;
  assert.equal(app.state.editBusy, false);
  assert.equal(app.state.edit, null);
});
