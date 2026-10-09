// REST 数据源：与 chusql-web 的 /api/* 一一对应。
// 请求都带 X-ChuSQL-Database 头；401 时广播登录事件。

export const AUTH_EVENT = 'chusql-auth-required';
let editing = null;
let consoleTransaction = null;
const pageOwner = [...crypto.getRandomValues(new Uint8Array(16))].map((byte) => byte.toString(16).padStart(2, '0')).join('');

// 是否是普通对象。
function isObject(value) {
  return value !== null && typeof value === 'object' && !Array.isArray(value);
}

// 校验结果列名及每行的列数。
function hasValidRows(body) {
  return isObject(body) && Array.isArray(body.columns) && body.columns.every((column) => typeof column === 'string')
    && Array.isArray(body.rows) && body.rows.every((row) => Array.isArray(row) && row.length === body.columns.length);
}

// 发一次请求并处理错误。
async function request(url, init, database) {
  const headers = new Headers(init && init.headers ? init.headers : undefined);
  if (database) headers.set('X-ChuSQL-Database', database);
  if (editing && editing.database === database && url.startsWith('/api/tables') && (!init || init.method === 'GET')) {
    headers.set('X-ChuSQL-Edit', editing.id);
  }
  let response;
  try {
    response = await fetch(url, { credentials: 'same-origin', ...(init ?? {}), headers });
  } catch (error) {
    throw new Error(`无法连接服务器：${error && error.message ? error.message : String(error)}`);
  }
  let body;
  try {
    body = await response.json();
  } catch {
    body = undefined;
  }
  if (!response.ok) {
    if (response.status === 401 || (isObject(body) && body.error === 'password_expired')) {
      window.dispatchEvent(new Event(AUTH_EVENT));
    }
    const message = isObject(body) && typeof body.message === 'string'
      ? body.message
      : `请求失败（HTTP ${response.status}）。`;
    const error = new Error(message);
    error.result = body;
    error.code = body?.error;
    throw error;
  }
  return body;
}

// 组装 JSON 请求参数。
function jsonInit(method, body) {
  return {
    method,
    headers: { 'Content-Type': 'application/json' },
    body: body === undefined ? undefined : JSON.stringify(body),
  };
}

const identifier = /^[A-Za-z_][A-Za-z0-9_]{0,63}$/;

// 校验数据库名格式。
function checkDatabaseName(name) {
  if (!identifier.test(String(name))) throw new Error('数据库名只能是字母、数字与下划线，且以字母或下划线开头。');
}

// ---------------------------------------------------------------- 会话

// 读取当前会话。
export async function currentSession() {
  const body = await request('/api/session');
  if (!isObject(body) || typeof body.user !== 'string') throw new Error('服务器返回的会话信息无法识别。');
  return {
    user: body.user,
    administrator: body.administrator === true,
    policy: isObject(body.policy) ? body.policy : { minLength: 0, classes: 0 },
  };
}

// 提交登录。
export async function login(user, password) {
  await request('/api/login', jsonInit('POST', { user, password }));
}

// 退出登录。
export async function logout() {
  await request('/api/logout', jsonInit('POST', {}));
}

// ---------------------------------------------------------------- 数据库

// 列出数据库。
export async function listDatabases() {
  const body = await request('/api/databases');
  if (!Array.isArray(body) || !body.every((name) => typeof name === 'string')) {
    throw new Error('服务器返回的数据库列表无法识别。');
  }
  return body;
}

// 新建数据库。
export async function createDatabase(name, database) {
  checkDatabaseName(name);
  await request('/api/databases', jsonInit('POST', { name }), database);
}

// 删除数据库。
export async function dropDatabase(name, database) {
  await request(`/api/databases/${encodeURIComponent(name)}`, jsonInit('DELETE'), database);
}

// 切换当前库（发 USE 语句）。
export async function useDatabase(name, database) {
  checkDatabaseName(name);
  return query(`USE ${name}`, database);
}

// ---------------------------------------------------------------- 表结构

// 校验并规整列描述。
function asColumns(value) {
  if (!Array.isArray(value)) throw new Error('服务器返回的列清单无法识别。');
  return value.map((entry) => {
    if (!isObject(entry) || typeof entry.name !== 'string' || typeof entry.type !== 'string' || !entry.type.trim()) {
      throw new Error('服务器返回的列描述无法识别。');
    }
    return { ...entry, name: entry.name, type: entry.type };
  });
}

// 校验并规整表描述。
function asTable(value) {
  if (!isObject(value) || typeof value.table !== 'string' || typeof value.rowCount !== 'number') {
    throw new Error('服务器返回的表描述无法识别。');
  }
  return {
    name: value.table,
    rowCount: value.rowCount,
    columns: asColumns(value.columns),
    indexes: Array.isArray(value.indexes) ? value.indexes : [],
    kind: value.kind === 'system' ? 'system' : 'table',
  };
}

// 列出表。
export async function listTables(database) {
  const body = await request('/api/tables', undefined, database);
  if (!Array.isArray(body)) throw new Error('服务器返回的表清单无法识别。');
  return body.map(asTable);
}

// 取单表结构。
export async function getTable(database, table) {
  return asTable(await request(`/api/tables/${encodeURIComponent(table)}`, undefined, database));
}

// 新建表。
export async function createTable(database, name, columns) {
  await request('/api/tables', jsonInit('POST', { name, columns }), database);
}

// 删除表。
export async function dropTable(database, table) {
  await request(`/api/tables/${encodeURIComponent(table)}`, jsonInit('DELETE'), database);
}

// 建索引。
export async function createIndex(database, table, column) {
  await request(`/api/tables/${encodeURIComponent(table)}/indexes`, jsonInit('POST', { column }), database);
}

// 删索引。
export async function dropIndex(database, table, column) {
  await request(`/api/tables/${encodeURIComponent(table)}/indexes/${encodeURIComponent(column)}`, jsonInit('DELETE'), database);
}

// 删列。
export async function dropColumn(database, table, column) {
  await request(`/api/tables/${encodeURIComponent(table)}/columns/${encodeURIComponent(column)}`, jsonInit('DELETE'), database);
}

// ---------------------------------------------------------------- 行

// 分页取表数据。
export async function fetchRows({ database, table, limit, offset, sort, dir, filters }) {
  const params = new URLSearchParams({ limit: String(limit), offset: String(offset) });
  if (sort) {
    params.set('sort', sort);
    params.set('dir', dir === 'desc' ? 'desc' : 'asc');
  }
  if (filters && filters.length) {
    params.set('filter', JSON.stringify(filters.map((f) => ({ column: f.column, value: f.expression }))));
  }
  const body = await request(`/api/tables/${encodeURIComponent(table)}/rows?${params}`, undefined, database);
  if (!hasValidRows(body) || !Number.isSafeInteger(body.total) || body.total < 0) {
    throw new Error('服务器返回的分页数据无法识别。');
  }
  const columns = body.columns;
  const rows = body.rows.map((raw) => {
    return Object.fromEntries(columns.map((column, index) => [column, raw[index]]));
  });
  return { columns, rows, total: body.total };
}

// 插入一行。
export async function insertRow(database, table, values) {
  await request(`/api/tables/${encodeURIComponent(table)}/rows`, jsonInit('POST', { values }), database);
}

// 更新一行。
export async function updateRow(database, table, id, values) {
  await request(`/api/tables/${encodeURIComponent(table)}/rows/${encodeURIComponent(id)}`, jsonInit('PATCH', { values }), database);
}

// 删除一行。
export async function deleteRow(database, table, id) {
  await request(`/api/tables/${encodeURIComponent(table)}/rows/${encodeURIComponent(id)}`, jsonInit('DELETE'), database);
}

// 为页面开启固定数据库的编辑快照。
export async function beginEdit(id, database) {
  await request('/api/edit/begin', jsonInit('POST', { id, database }));
  editing = { id, database };
}

// 在服务端事务中原子提交编辑集。
export async function commitChanges(id, database, changes) {
  try {
    const result = await request('/api/edit/commit', jsonInit('POST', { id, database, changes }));
    if (!isObject(result) || result.ok !== true || result.state !== 'committed') throw new Error('服务器未返回可确认的提交结果。');
    editing = null;
    return result;
  } catch (error) {
    if (isObject(error.result) && typeof error.result.state === 'string') return error.result;
    return { ok: false, applied: 0, state: error.result ? 'lost' : 'unknown',
      reloadRequired: Boolean(error.result), message: error.message };
  }
}

// 回滚编辑快照并清除本地事务标识。
export async function rollbackEdit(id, database) {
  const result = await request('/api/edit/rollback', jsonInit('POST', { id, database }));
  editing = null;
  return result;
}

// 忘记已失效的登录会话编辑标识。
export function forgetEdit() {
  editing = null;
  consoleTransaction = null;
}

// 标签关闭时回滚它持有的控制台事务。
export async function rollbackConsole(tabId) {
  if (!consoleTransaction || consoleTransaction.console !== `${pageOwner}:${tabId ?? 'default'}`) return;
  await request('/api/query/rollback', jsonInit('POST', { console: consoleTransaction.console }));
  consoleTransaction = null;
}

// 页面离开时请求清理控制台事务。
export function closeConsoleOnPagehide() {
  if (consoleTransaction) {
    navigator.sendBeacon('/api/query/rollback', new Blob([JSON.stringify({ console: consoleTransaction.console })], { type: 'application/json' }));
  }
}

// ---------------------------------------------------------------- 查询

// 执行 SQL 并规整结果。
export async function query(sql, database, tabId) {
  const started = performance.now();
  try {
    const console = `${pageOwner}:${tabId ?? 'default'}`;
    const init = jsonInit('POST', { sql });
    init.headers['X-ChuSQL-Console'] = console;
    const body = await request('/api/query', init, database);
    if (!hasValidRows(body)) {
      throw new Error('服务器返回的查询结果无法识别。');
    }
    const durationMs = performance.now() - started;
    consoleTransaction = body.transaction === true ? { console, database } : null;
    return {
      type: body.columns.length ? 'rows' : 'affected',
      columns: body.columns,
      rows: body.rows,
      affected: typeof body.rowCount === 'number' ? body.rowCount : 0,
      truncated: body.truncated === true,
      database: typeof body.database === 'string' ? body.database : undefined,
      transactionActive: body.transaction === true,
      durationMs,
    };
  } catch (error) {
    return { type: 'error', durationMs: performance.now() - started, message: error instanceof Error ? error.message : String(error) };
  }
}

// 灌演示数据。
export async function seedDemoData(database) {
  return request('/api/demo-data', jsonInit('POST', {}), database);
}

// ---------------------------------------------------------------- 设置

// 读取服务器设置。
export async function loadServerSettings() {
  const body = await request('/api/settings');
  if (!isObject(body) || !Array.isArray(body.items)) throw new Error('服务器返回的设置清单无法识别。');
  return body;
}

// 保存服务器设置。
export async function saveServerSettings(values) {
  const body = await request('/api/settings', jsonInit('PUT', { values }));
  return isObject(body) ? body : { ok: true };
}

// 读取 IDE 设置。
export async function loadUiSettings() {
  return request('/api/ui-settings');
}

// 保存 IDE 设置。
export async function saveUiSettings(settings) {
  await request('/api/ui-settings', jsonInit('PUT', settings));
}
