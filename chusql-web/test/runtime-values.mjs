import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

// 检查前端复合值显示、SQL 生成和编码错误。

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
console.log('runtime values: 9 assertions passed');
