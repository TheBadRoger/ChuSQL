// SQL 脚本切分：识别字符串、注释并按分号拆分语句。

export interface SqlStatement {
  id: string;
  sql: string;
  startLine: number;
  endLine: number;
}

function lineAt(source: string, index: number): number {
  let line = 1;
  for (let cursor = 0; cursor < index; cursor += 1) {
    if (source[cursor] === '\n') line += 1;
  }
  return line;
}

export function splitSqlStatements(source: string): SqlStatement[] {
  const spans: Array<[number, number]> = [];
  let start = 0;
  let mode: 'normal' | 'single' | 'double' | 'line-comment' | 'block-comment' = 'normal';

  for (let index = 0; index < source.length; index += 1) {
    const char = source[index];
    const next = source[index + 1];
    if (mode === 'single') {
      if (char === "'" && next === "'") index += 1;
      else if (char === "'") mode = 'normal';
      continue;
    }
    if (mode === 'double') {
      if (char === '"' && next === '"') index += 1;
      else if (char === '"') mode = 'normal';
      continue;
    }
    if (mode === 'line-comment') {
      if (char === '\n') mode = 'normal';
      continue;
    }
    if (mode === 'block-comment') {
      if (char === '*' && next === '/') {
        mode = 'normal';
        index += 1;
      }
      continue;
    }
    if (char === "'") mode = 'single';
    else if (char === '"') mode = 'double';
    else if (char === '-' && next === '-') {
      mode = 'line-comment';
      index += 1;
    } else if (char === '/' && next === '*') {
      mode = 'block-comment';
      index += 1;
    } else if (char === ';') {
      spans.push([start, index]);
      start = index + 1;
    }
  }
  spans.push([start, source.length]);

  return spans.flatMap(([from, to], ordinal) => {
    const raw = source.slice(from, to);
    const leading = raw.search(/\S/);
    if (leading < 0) return [];
    const sql = raw.trim();
    const absoluteStart = from + leading;
    const startLine = lineAt(source, absoluteStart);
    return [{
      id: `statement-${ordinal + 1}`,
      sql,
      startLine,
      endLine: startLine + (sql.match(/\n/g)?.length ?? 0),
    }];
  });
}
