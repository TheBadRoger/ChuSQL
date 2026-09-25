import { describe, expect, it } from 'vitest';
import { SQL_PLAIN_COLOR, SQL_TOKEN_COLORS, tokenizeSql } from './sqlHighlight';

// 覆盖 SQL 高亮切分与执行器配色常量。

describe('tokenizeSql', () => {
  it('marks keywords, strings and numbers while keeping the text intact', () => {
    const sql = "UPDATE users SET name = 'A''B' WHERE id = 12;";
    const tokens = tokenizeSql(sql);
    expect(tokens.map((token) => token.text).join('')).toBe(sql);
    expect(tokens.filter((token) => token.kind === 'keyword').map((token) => token.text)).toEqual(['UPDATE', 'SET', 'WHERE']);
    expect(tokens.filter((token) => token.kind === 'string').map((token) => token.text)).toEqual(["'A''B'"]);
    expect(tokens.filter((token) => token.kind === 'number').map((token) => token.text)).toEqual(['12']);
  });

  it('recognises line comments and leaves unknown words plain', () => {
    expect(tokenizeSql('SELECT * FROM users -- note').at(-1)).toEqual({ text: '-- note', kind: 'comment' });
    expect(tokenizeSql('select users').map((token) => token.kind)).toEqual(['keyword', 'plain']);
  });

  it('splits schema statements used by the pending list', () => {
    const tokens = tokenizeSql('CREATE TABLE items (id int, title str);');
    expect(tokens.filter((token) => token.kind === 'keyword').map((token) => token.text)).toEqual(['CREATE', 'TABLE', 'int', 'str']);
    expect(tokens.map((token) => token.text).join('')).toBe('CREATE TABLE items (id int, title str);');
  });
});

describe('SQL_TOKEN_COLORS', () => {
  it('matches the colours the SQL console theme uses', () => {
    expect(SQL_TOKEN_COLORS).toEqual({ keyword: '#569CD6', string: '#CE9178', number: '#B5CEA8', comment: '#6A9955' });
  });

  it('paints everything that is not a keyword in the plain grey', () => {
    expect(SQL_PLAIN_COLOR).toBe('rgb(200, 200, 200)');
  });
});
