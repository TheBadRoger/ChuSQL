import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

// 检查复合值、SQL 生成、类型元数据和编码错误。

const source = await readFile(new URL('../static/core.js', import.meta.url), 'utf8');
const { cellText, sqlLiteral, typeLabel, editText } = await import(`data:text/javascript;base64,${Buffer.from(source).toString('base64')}`);
const typeInt = [1, []];
const typeString = [4, []];
const nested = { __chusql_runtime_v1: [1, [5, [[6, [typeInt]]]], [[], [42]]] };
assert.equal(cellText(nested, 'NULL'), '[Nothing, Just (42)]');
assert.equal(sqlLiteral(nested), 'LIST<Maybe Int>(MAYBE<Int>(), MAYBE<Int>(42))');
assert.equal(sqlLiteral({ __chusql_runtime_v1: [1, [7, [typeInt, typeString]], [1, "a'b"]] }), "TUPLE<Int, String>(1, 'a''b')");
assert.equal(sqlLiteral({ __chusql_runtime_v1: [1, [5, [[2, []]]], [1]] }), 'LIST<Double>(1.0)');
assert.throws(() => cellText({ __chusql_runtime_v1: [2, typeInt, 1] }, 'NULL'), /encoding/);
assert.throws(() => sqlLiteral({ __chusql_runtime_v1: [1, [7, [typeInt]], [1, 2]] }), /mismatch/);
assert.equal(typeLabel('runtime([1,[5,[[1,[]]]]])'), '[Int]');
assert.equal(editText({ __chusql_runtime_v1: [1, typeString, 'abc'] }), 'abc');
assert.throws(() => sqlLiteral({ __chusql_runtime_v1: [1, typeInt, 1.5] }), /integer/);
const apiSource = await readFile(new URL('../static/api.js', import.meta.url), 'utf8');
const api = await import(`data:text/javascript;base64,${Buffer.from(apiSource).toString('base64')}`);
const columns = ['int', 'runtime([1,[5,[[1,[]]]]])', 'runtime([1,[6,[[4,[]]]]])', 'runtime([1,[7,[[1,[]],[4,[]]]]])', 'domain(age,int)']
  .map((type, index) => ({ name: `column_${index}`, type }));
const table = { table: 'typed', rowCount: 0, columns };
const originalFetch = globalThis.fetch;
try {
  globalThis.fetch = async (url) => new Response(JSON.stringify(url === '/api/tables' ? [table] : table));
  assert.deepEqual((await api.listTables('test'))[0].columns, columns);
  assert.deepEqual((await api.getTable('test', 'typed')).columns, columns);
  for (const type of [undefined, 42, '   ']) {
    globalThis.fetch = async () => new Response(JSON.stringify([{ ...table, columns: [{ name: 'broken', type }] }]));
    await assert.rejects(api.listTables('test'), /列描述/);
  }
  const page = { columns: ['id', 'value'], rows: [[1, null]], total: 1 };
  globalThis.fetch = async () => new Response(JSON.stringify(page));
  assert.deepEqual((await api.fetchRows({ database: 'test', table: 'typed', limit: 10, offset: 0 })).rows,
    [{ id: 1, value: null }]);
  assert.deepEqual((await api.query('SELECT id, value FROM typed', 'test')).rows, page.rows);
  for (const invalid of [
    { ...page, rows: [[1]] }, { ...page, rows: [[1, null, 3]] },
    { ...page, rows: [null] }, { ...page, columns: ['id', 42] },
  ]) {
    globalThis.fetch = async () => new Response(JSON.stringify(invalid));
    await assert.rejects(api.fetchRows({ database: 'test', table: 'typed', limit: 10, offset: 0 }), /分页数据/);
    assert.equal((await api.query('SELECT id, value FROM typed', 'test')).type, 'error');
  }
  for (const total of [-1, 1.5, '1']) {
    globalThis.fetch = async () => new Response(JSON.stringify({ ...page, total }));
    await assert.rejects(api.fetchRows({ database: 'test', table: 'typed', limit: 10, offset: 0 }), /分页数据/);
  }
} finally {
  globalThis.fetch = originalFetch;
}
console.log('runtime values and schemas: 27 assertions passed');
