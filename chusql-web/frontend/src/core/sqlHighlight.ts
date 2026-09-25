export const SQL_TOKEN_COLORS = {
  keyword: '#569CD6',
  string: '#CE9178',
  number: '#B5CEA8',
  comment: '#6A9955',
} as const;

export const SQL_PLAIN_COLOR = 'rgb(200, 200, 200)';

export type SqlTokenKind = 'plain' | keyof typeof SQL_TOKEN_COLORS;

export interface SqlToken { text: string; kind: SqlTokenKind }

// SQL 词法高亮：与执行器共用同一套 token 配色。

const keywords = new Set([
  'select', 'from', 'where', 'insert', 'into', 'values', 'update', 'set', 'delete',
  'create', 'table', 'index', 'on', 'drop', 'alter', 'and', 'or', 'not', 'null',
  'order', 'by', 'group', 'having', 'limit', 'offset', 'join', 'left', 'right',
  'inner', 'outer', 'as', 'distinct', 'primary', 'key', 'int', 'integer', 'str',
  'text', 'bool', 'boolean', 'true', 'false',
]);

const wordChar = /[A-Za-z0-9_]/;

// 切分 SQL 为关键字、字符串、数字、注释与普通片段。
export function tokenizeSql(sql: string): SqlToken[] {
  const tokens: SqlToken[] = [];
  let plain = '';
  let index = 0;
  const flush = () => { if (plain) { tokens.push({ text: plain, kind: 'plain' }); plain = ''; } };
  while (index < sql.length) {
    const char = sql[index];
    if (char === "'") {
      flush();
      let end = index + 1;
      while (end < sql.length) {
        if (sql[end] !== "'") { end += 1; continue; }
        if (sql[end + 1] === "'") { end += 2; continue; }
        end += 1;
        break;
      }
      tokens.push({ text: sql.slice(index, end), kind: 'string' });
      index = end;
      continue;
    }
    if (char === '-' && sql[index + 1] === '-') {
      flush();
      const end = sql.indexOf('\n', index);
      tokens.push({ text: end < 0 ? sql.slice(index) : sql.slice(index, end), kind: 'comment' });
      index = end < 0 ? sql.length : end;
      continue;
    }
    if (char >= '0' && char <= '9') {
      flush();
      let end = index;
      while (end < sql.length && (wordChar.test(sql[end]) || sql[end] === '.')) end += 1;
      tokens.push({ text: sql.slice(index, end), kind: 'number' });
      index = end;
      continue;
    }
    if (wordChar.test(char)) {
      let end = index;
      while (end < sql.length && wordChar.test(sql[end])) end += 1;
      const word = sql.slice(index, end);
      if (keywords.has(word.toLowerCase())) { flush(); tokens.push({ text: word, kind: 'keyword' }); }
      else plain += word;
      index = end;
      continue;
    }
    plain += char;
    index += 1;
  }
  flush();
  return tokens;
}
