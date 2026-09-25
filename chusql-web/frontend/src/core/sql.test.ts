import { describe, expect, it } from 'vitest';
import { splitSqlStatements } from './sql';

// 覆盖语句切分的分号、注释与行号计算。

describe('splitSqlStatements', () => {
  it('splits a script into executable statements', () => {
    expect(splitSqlStatements('SELECT 1; SELECT 2;').map((item) => item.sql)).toEqual([
      'SELECT 1',
      'SELECT 2',
    ]);
  });

  it('keeps semicolons inside strings and comments', () => {
    const sql = "SELECT 'a;b'; -- keep ; here\nSELECT 2 /* and ; here */;";
    expect(splitSqlStatements(sql).map((item) => item.sql)).toEqual([
      "SELECT 'a;b'",
      '-- keep ; here\nSELECT 2 /* and ; here */',
    ]);
  });

  it('reports one-based source lines for gutter decorations', () => {
    const statements = splitSqlStatements('\nSELECT 1;\n\nSELECT 2');
    expect(statements.map(({ startLine, endLine }) => [startLine, endLine])).toEqual([
      [2, 2],
      [4, 4],
    ]);
  });
});
