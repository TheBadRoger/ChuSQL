// 纯逻辑层：SQL 生成、变更集、筛选、类型、标签页与 IDE 设置。
// 不碰 DOM、不发请求。

// ---------------------------------------------------------------- 转义

// 转义 HTML 特殊字符。
export function escapeHtml(value) {
  return String(value ?? '')
    .replaceAll('&', '&amp;')
    .replaceAll('<', '&lt;')
    .replaceAll('>', '&gt;')
    .replaceAll('"', '&quot;')
    .replaceAll("'", '&#39;');
}

// ---------------------------------------------------------------- SQL 字面量

const identifierPattern = /^[A-Za-z_][A-Za-z0-9_]*$/;

// 账号表：在 system 库里像普通表一样浏览，写入会翻译成账号命令。
export const accountsTable = '__system_users';

// 值转 SQL 字面量（字符串加引号转义）。
export function sqlLiteral(value) {
  if (value === null || value === undefined) return 'NULL';
  if (typeof value === 'number' || typeof value === 'boolean') return String(value);
  return `'${String(value).replaceAll("'", "''")}'`;
}

// 标识符：白名单外才加双引号。
export function sqlIdentifier(name) {
  const text = String(name);
  return identifierPattern.test(text) ? text : `"${text.replaceAll('"', '""')}"`;
}

// 单元格修改语句。
export function updateSql(table, pk, column, value) {
  if (table === accountsTable) return `ALTER USER ${sqlIdentifier(pk)} IDENTIFIED BY ${sqlLiteral(value)};`;
  return `UPDATE ${sqlIdentifier(table)} SET ${sqlIdentifier(column)} = ${sqlLiteral(value)} WHERE id = ${sqlLiteral(pk)};`;
}

// 插入语句。
export function insertSql(table, values) {
  if (table === accountsTable) {
    return `CREATE USER ${sqlIdentifier(values.user ?? '')} IDENTIFIED BY ${sqlLiteral(values.password)};`;
  }
  const columns = Object.keys(values);
  const literals = columns.map((column) => sqlLiteral(values[column]));
  return `INSERT INTO ${sqlIdentifier(table)} (${columns.map(sqlIdentifier).join(', ')}) VALUES (${literals.join(', ')});`;
}

// 删除语句。
export function deleteSql(table, pk) {
  if (table === accountsTable) return `DROP USER ${sqlIdentifier(pk)};`;
  return `DELETE FROM ${sqlIdentifier(table)} WHERE id = ${sqlLiteral(pk)};`;
}

// 一条待提交变更对应的展示用 SQL。
export function changeSql(change) {
  if (change.op === 'insert') return insertSql(change.table, change.values);
  if (change.op === 'delete') return deleteSql(change.table, change.pk);
  if (change.table === accountsTable) {
    return updateSql(change.table, change.pk, 'password', change.set.password);
  }
  const assignments = Object.entries(change.set)
    .map(([column, value]) => `${sqlIdentifier(column)} = ${sqlLiteral(value)}`)
    .join(', ');
  return `UPDATE ${sqlIdentifier(change.table)} SET ${assignments} WHERE id = ${sqlLiteral(change.pk)};`;
}

// ---------------------------------------------------------------- 变更集

// 建一个空变更集。
export function createEmptyChangeSet() {
  return { updates: new Map(), inserts: new Map(), deletes: new Map() };
}

// 复制变更集。
export function cloneChangeSet(changes) {
  return {
    updates: new Map(changes.updates),
    inserts: new Map(changes.inserts),
    deletes: new Map(changes.deletes),
  };
}

// 变更总条数。
export function changeCount(changes) {
  return changes.updates.size + changes.inserts.size + changes.deletes.size;
}

// 单元格变更的键。
export function changeKey(table, pk, column) {
  return `${table}:${JSON.stringify(pk)}:${column}`;
}

// 插入行的键。
export function insertKey(table, tempId) {
  return `${table}:${tempId}`;
}

// 删除行的键。
export function deleteKey(table, pk) {
  return `${table}:${JSON.stringify(pk)}`;
}

// 组装 DML 提交载荷：同一行的多列修改并成一条。
export function toCommitPayload(changes) {
  const updates = new Map();
  for (const update of changes.updates.values()) {
    const key = `${update.table}:${JSON.stringify(update.pk)}`;
    const existing = updates.get(key);
    if (existing) {
      existing.set[update.column] = update.newValue;
    } else {
      updates.set(key, { op: 'update', table: update.table, pk: update.pk, set: { [update.column]: update.newValue } });
    }
  }
  return [
    ...updates.values(),
    ...[...changes.inserts.values()].map((insert) => ({ op: 'insert', table: insert.table, values: { ...insert.values } })),
    ...[...changes.deletes.values()].map((deleted) => ({ op: 'delete', table: deleted.table, pk: deleted.pk })),
  ];
}

// 面板展示用的变更条目列表。
export function changeEntries(changes) {
  const updates = [...changes.updates.entries()].map(([key, update]) => ({
    key,
    kind: 'update',
    table: update.table,
    pk: update.pk,
    column: update.column,
    sql: changeSql({ op: 'update', table: update.table, pk: update.pk, set: { [update.column]: update.newValue } }),
  }));
  const inserts = [...changes.inserts.entries()].map(([key, insert]) => ({
    key,
    kind: 'insert',
    table: insert.table,
    tempId: insert.tempId,
    sql: changeSql({ op: 'insert', table: insert.table, values: insert.values }),
  }));
  const deletes = [...changes.deletes.entries()].map(([key, deleted]) => ({
    key,
    kind: 'delete',
    table: deleted.table,
    pk: deleted.pk,
    sql: changeSql({ op: 'delete', table: deleted.table, pk: deleted.pk }),
  }));
  return [...updates, ...inserts, ...deletes];
}

// 全部变更的 SQL 预览。
export function changesSql(changes) {
  return toCommitPayload(changes).map(changeSql);
}

// ---------------------------------------------------------------- 筛选

// 转义正则元字符。
function escapeRegExp(value) {
  return value.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

// like 通配转正则。
function likePattern(raw) {
  const escaped = escapeRegExp(raw).replaceAll('%', '.*').replaceAll('_', '.');
  return new RegExp(`^${escaped}$`, 'i');
}

// 判断单元格值是否命中筛选表达式。
export function matchesFilter(value, rawExpression) {
  const expression = String(rawExpression ?? '').trim();
  if (!expression) return true;
  const normalized = expression.toLowerCase();

  if (normalized === 'null') return value === null || value === undefined;
  if (normalized === 'true' || normalized === 'false') return value === (normalized === 'true');

  const like = expression.match(/^like\s+(.+)$/i);
  if (like) return likePattern(like[1]).test(String(value ?? ''));

  const comparison = expression.match(/^(>=|<=|>|<|=|!=)\s*(-?\d+(?:\.\d+)?)$/);
  if (comparison) {
    const left = Number(value);
    const right = Number(comparison[2]);
    if (!Number.isFinite(left)) return false;
    switch (comparison[1]) {
      case '>': return left > right;
      case '<': return left < right;
      case '>=': return left >= right;
      case '<=': return left <= right;
      case '!=': return left !== right;
      default: return left === right;
    }
  }

  return String(value ?? '').toLocaleLowerCase().includes(normalized);
}

// ---------------------------------------------------------------- 列类型

const typeLabels = {
  int: 'INT', bigint: 'BIGINT', smallint: 'SMALLINT', str: 'STR', text: 'TEXT',
  varchar: 'VARCHAR', char: 'CHAR', bool: 'BOOL', boolean: 'BOOL', float: 'FLOAT',
  real: 'REAL', double: 'DOUBLE', decimal: 'DECIMAL', numeric: 'NUMERIC',
  date: 'DATE', timestamp: 'TIMESTAMP', blob: 'BLOB',
};

// 建表对话框里可选的类型。
export const creatableTypes = [
  'int', 'bigint', 'smallint', 'str', 'varchar(255)', 'char(1)', 'bool',
  'float', 'double', 'decimal(10,2)', 'date', 'timestamp', 'blob',
];

// 取类型基名（去掉参数）。
export function columnKind(type) {
  return String(type).split('(')[0].trim().toLowerCase();
}

// 是否整数类型。
export function isIntegerType(type) {
  return ['int', 'bigint', 'smallint'].includes(columnKind(type));
}

// 是否数值类型。
export function isNumericType(type) {
  return ['int', 'bigint', 'smallint', 'float', 'double', 'decimal', 'numeric'].includes(columnKind(type));
}

// 是否布尔类型。
export function isBooleanType(type) {
  return ['bool', 'boolean'].includes(columnKind(type));
}

// 类型的显示名。
export function typeLabel(type) {
  const kind = columnKind(type);
  const label = typeLabels[kind];
  const args = String(type).includes('(') ? String(type).slice(String(type).indexOf('(')) : '';
  if (label) return `${label}${args}`;
  return String(type);
}

// ---------------------------------------------------------------- 值渲染

// 单元格显示文本（空值用占位文本）。
export function cellText(value, nullText) {
  if (value === null || value === undefined) return nullText;
  if (typeof value === 'boolean') return value ? 'true' : 'false';
  return String(value);
}

// 编辑框里回填的文本（null 给空串）。
export function editText(value) {
  if (value === null || value === undefined) return '';
  return String(value);
}

// 编辑框文本转提交值（空串当 null）。
export function parseEdited(raw, type) {
  const text = String(raw ?? '').trim();
  if (text === '') return null;
  if (isIntegerType(type)) {
    const n = Number(text);
    return Number.isFinite(n) ? Math.trunc(n) : text;
  }
  if (isNumericType(type)) {
    const n = Number(text);
    return Number.isFinite(n) ? n : text;
  }
  if (isBooleanType(type)) {
    const lower = text.toLowerCase();
    if (['1', 'true', 'yes', 'on'].includes(lower)) return true;
    if (['0', 'false', 'no', 'off'].includes(lower)) return false;
    return text;
  }
  return text;
}

// ---------------------------------------------------------------- SQL 高亮

export const SQL_TOKEN_COLORS = {
  keyword: '#569CD6',
  string: '#CE9178',
  number: '#B5CEA8',
  comment: '#6A9955',
};

const keywords = new Set([
  'select', 'from', 'where', 'insert', 'into', 'values', 'update', 'set', 'delete',
  'create', 'table', 'index', 'on', 'drop', 'alter', 'and', 'or', 'not', 'null',
  'order', 'by', 'group', 'having', 'limit', 'offset', 'join', 'left', 'right',
  'inner', 'outer', 'as', 'distinct', 'primary', 'key', 'int', 'integer', 'str',
  'text', 'bool', 'boolean', 'true', 'false', 'use', 'show', 'database', 'databases',
  'user', 'identified', 'add', 'column', 'if', 'exists', 'begin', 'commit', 'rollback',
]);

const wordChar = /[A-Za-z0-9_]/;

// 切分 SQL 为关键字、字符串、数字、注释与普通片段。
export function tokenizeSql(sql) {
  const tokens = [];
  let plain = '';
  let index = 0;
  const text = String(sql ?? '');
  // 收尾当前普通片段。
  const flush = () => {
    if (plain) {
      tokens.push({ text: plain, kind: 'plain' });
      plain = '';
    }
  };
  while (index < text.length) {
    const char = text[index];
    if (char === "'") {
      flush();
      let end = index + 1;
      while (end < text.length) {
        if (text[end] !== "'") { end += 1; continue; }
        if (text[end + 1] === "'") { end += 2; continue; }
        end += 1;
        break;
      }
      tokens.push({ text: text.slice(index, end), kind: 'string' });
      index = end;
      continue;
    }
    if (char === '-' && text[index + 1] === '-') {
      flush();
      const end = text.indexOf('\n', index);
      tokens.push({ text: end < 0 ? text.slice(index) : text.slice(index, end), kind: 'comment' });
      index = end < 0 ? text.length : end;
      continue;
    }
    if (char >= '0' && char <= '9') {
      flush();
      let end = index;
      while (end < text.length && (wordChar.test(text[end]) || text[end] === '.')) end += 1;
      tokens.push({ text: text.slice(index, end), kind: 'number' });
      index = end;
      continue;
    }
    if (wordChar.test(char)) {
      let end = index;
      while (end < text.length && wordChar.test(text[end])) end += 1;
      const word = text.slice(index, end);
      if (keywords.has(word.toLowerCase())) {
        flush();
        tokens.push({ text: word, kind: 'keyword' });
      } else {
        plain += word;
      }
      index = end;
      continue;
    }
    plain += char;
    index += 1;
  }
  flush();
  return tokens;
}

// 高亮 SQL 片段转 HTML。
export function highlightHtml(sql) {
  return tokenizeSql(sql)
    .map((token) => (token.kind === 'plain' ? escapeHtml(token.text) : `<span class="tok-${token.kind}">${escapeHtml(token.text)}</span>`))
    .join('');
}

// ---------------------------------------------------------------- 标签页

// 打开表标签：同库同表已开则激活，否则追加。
export function openTableTab(tabs, database, name, id) {
  const existing = tabs.find((tab) => tab.kind === 'table' && tab.table === name && tab.database === database);
  if (existing) return { tabs, activeId: existing.id };
  return { tabs: [...tabs, { id, kind: 'table', title: name, table: name, database }], activeId: id };
}

// 打开查询标签：每次追加。
export function openQueryTab(tabs, title, id, database, sql) {
  return { tabs: [...tabs, { id, kind: 'sql', title, sql, database }], activeId: id };
}

// 打开设置标签：只保留一个。
export function openSettingsTab(tabs, title, id) {
  const existing = tabs.find((tab) => tab.kind === 'settings');
  if (existing) return { tabs, activeId: existing.id };
  return { tabs: [...tabs, { id, kind: 'settings', title }], activeId: id };
}

// 关闭标签：关当前标签时激活右邻，无则左邻。
export function closeTab(tabs, id, activeId) {
  const index = tabs.findIndex((tab) => tab.id === id);
  if (index < 0) return { tabs, activeId };
  const remaining = tabs.filter((tab) => tab.id !== id);
  if (activeId !== id) return { tabs: remaining, activeId };
  const next = remaining[index] ?? remaining[index - 1];
  return { tabs: remaining, activeId: next ? next.id : undefined };
}

// ---------------------------------------------------------------- IDE 设置

// 默认 IDE 设置（与后端 /api/ui-settings 的键一一对应）。
export const DEFAULT_SETTINGS = Object.freeze({
  uiFonts: ['JetBrains Mono', 'Consolas', 'Cascadia Code'],
  uiFontSize: 13,
  gridFonts: ['JetBrains Mono', 'Consolas'],
  gridFontSize: 12,
  gridRowHeight: 24,
  sqlFonts: ['JetBrains Mono', 'Consolas'],
  sqlFontSize: 13,
  sqlLineHeight: 20,
  sqlTabSize: 2,
  sqlLineNumbers: true,
  pageSize: 200,
  nullText: 'NULL',
  autocomplete: true,
  minimap: false,
});

export const fontKeys = ['uiFonts', 'gridFonts', 'sqlFonts'];
export const booleanKeys = ['sqlLineNumbers', 'autocomplete', 'minimap'];
export const numberKeys = ['uiFontSize', 'gridFontSize', 'gridRowHeight', 'sqlFontSize', 'sqlLineHeight', 'sqlTabSize', 'pageSize'];

export const settingRanges = {
  uiFontSize: [11, 16],
  gridFontSize: [10, 18],
  gridRowHeight: [18, 40],
  sqlFontSize: [10, 20],
  sqlLineHeight: [16, 40],
  sqlTabSize: [1, 8],
  pageSize: [10, 500],
};

export const settingLabels = {
  uiFonts: '界面字体链',
  uiFontSize: '界面字号',
  gridFonts: '表格字体链',
  gridFontSize: '表格字号',
  gridRowHeight: '表格行高',
  sqlFonts: 'SQL 字体链',
  sqlFontSize: 'SQL 字号',
  sqlLineHeight: 'SQL 行高',
  sqlTabSize: 'SQL Tab 宽度',
  sqlLineNumbers: '显示行号',
  pageSize: '每页行数',
  nullText: 'NULL 显示文本',
  autocomplete: '自动补全',
  minimap: '缩略图',
};

export const IDE_SETTINGS_KEY = 'chusql.ide.settings.v1';

// 数值收敛到区间并取整。
export function clampSetting(key, value) {
  const range = settingRanges[key];
  const numeric = Number(value);
  if (!Number.isFinite(numeric)) return DEFAULT_SETTINGS[key];
  const rounded = Math.round(numeric);
  return range ? Math.min(range[1], Math.max(range[0], rounded)) : rounded;
}

// 字体链 → CSS font-family 值。
export function fontFamilyValue(chain) {
  const fonts = chain
    .map((font) => String(font).trim())
    .filter(Boolean)
    .map((font) => `"${font.replaceAll('\\', '\\\\').replaceAll('"', '\\"')}"`);
  return [...fonts, 'monospace'].join(', ');
}

// 只取已知键、按类型收敛；坏值丢弃。
export function parseUiSettings(body) {
  const parsed = {};
  if (!body || typeof body !== 'object' || Array.isArray(body)) return parsed;
  for (const key of fontKeys) {
    const value = body[key];
    if (!Array.isArray(value)) continue;
    const fonts = value.filter((font) => typeof font === 'string' && font.trim().length > 0).slice(0, 10);
    if (fonts.length) parsed[key] = fonts;
  }
  for (const key of numberKeys) {
    const value = body[key];
    if (typeof value !== 'number' || !Number.isFinite(value)) continue;
    parsed[key] = clampSetting(key, value);
  }
  for (const key of booleanKeys) {
    if (typeof body[key] === 'boolean') parsed[key] = body[key];
  }
  if (typeof body.nullText === 'string') parsed.nullText = body.nullText;
  return parsed;
}

// 逗号分隔的字体链 ↔ 数组。
export function fontsFromText(text) {
  return String(text ?? '')
    .split(',')
    .map((font) => font.trim())
    .filter(Boolean)
    .slice(0, 10);
}

// 字体链转逗号分隔文本。
export function fontsToText(chain) {
  return chain.join(', ');
}
