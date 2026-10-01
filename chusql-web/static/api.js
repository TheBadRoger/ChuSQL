// REST 数据源：与 chusql-web 的 /api/* 一一对应。
// 所有请求带 X-ChuSQL-Database 头（服务端不保存 USE 状态）；401 时广播登录事件。

export const AUTH_EVENT = 'chusql-auth-required';

function isObject(value) {
  return value !== null && typeof value === 'object' && !Array.isArray(value);
}

async function request(url, init, database) {
  const headers = new Headers(init && init.headers ? init.headers : undefined);
  if (database) headers.set('X-ChuSQL-Database', database);
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
    throw new Error(message);
  }
  return body;
}

function jsonInit(method, body) {
  return {
    method,
    headers: { 'Content-Type': 'application/json' },
    body: body === undefined ? undefined : JSON.stringify(body),
  };
}

const identifier = /^[A-Za-z_][A-Za-z0-9_]{0,63}$/;

function checkDatabaseName(name) {
  if (!identifier.test(String(name))) throw new Error('数据库名只能是字母、数字与下划线，且以字母或下划线开头。');
}

// ---------------------------------------------------------------- 会话

export async function currentSession() {
  const body = await request('/api/session');
  if (!isObject(body) || typeof body.user !== 'string') throw new Error('服务器返回的会话信息无法识别。');
  return {
    user: body.user,
    administrator: body.administrator === true,
    policy: isObject(body.policy) ? body.policy : { minLength: 0, classes: 0 },
  };
}

export async function login(user, password) {
  await request('/api/login', jsonInit('POST', { user, password }));
}

export async function logout() {
  await request('/api/logout', jsonInit('POST', {}));
}

// ---------------------------------------------------------------- 数据库

export async function listDatabases() {
  const body = await request('/api/databases');
  if (!Array.isArray(body) || !body.every((name) => typeof name === 'string')) {
    throw new Error('服务器返回的数据库列表无法识别。');
  }
  return body;
}

export async function createDatabase(name, database) {
  checkDatabaseName(name);
  await request('/api/databases', jsonInit('POST', { name }), database);
}

export async function dropDatabase(name, database) {
  await request(`/api/databases/${encodeURIComponent(name)}`, jsonInit('DELETE'), database);
}

// 切换当前库：走一条 USE 语句，服务端在响应里回带新的库名。
export async function useDatabase(name, database) {
  checkDatabaseName(name);
  return query(`USE ${name}`, database);
}

// ---------------------------------------------------------------- 表结构

function columnKind(type) {
  return String(type).split('(')[0].trim().toLowerCase();
}

const knownTypes = ['int', 'bigint', 'smallint', 'str', 'text', 'varchar', 'char', 'float', 'double', 'decimal', 'numeric', 'bool', 'date', 'timestamp', 'blob'];

function asColumns(value) {
  if (!Array.isArray(value)) throw new Error('服务器返回的列清单无法识别。');
  return value.map((entry) => {
    if (!isObject(entry) || typeof entry.name !== 'string' || typeof entry.type !== 'string') {
      throw new Error('服务器返回的列描述无法识别。');
    }
    if (!knownTypes.includes(columnKind(entry.type))) throw new Error(`不支持的列类型：${entry.type}`);
    return { ...entry, name: entry.name, type: entry.type };
  });
}

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

export async function listTables(database) {
  const body = await request('/api/tables', undefined, database);
  if (!Array.isArray(body)) throw new Error('服务器返回的表清单无法识别。');
  return body.map(asTable);
}

export async function getTable(database, table) {
  return asTable(await request(`/api/tables/${encodeURIComponent(table)}`, undefined, database));
}

export async function createTable(database, name, columns) {
  await request('/api/tables', jsonInit('POST', { name, columns }), database);
}

export async function dropTable(database, table) {
  await request(`/api/tables/${encodeURIComponent(table)}`, jsonInit('DELETE'), database);
}

export async function createIndex(database, table, column) {
  await request(`/api/tables/${encodeURIComponent(table)}/indexes`, jsonInit('POST', { column }), database);
}

export async function dropIndex(database, table, column) {
  await request(`/api/tables/${encodeURIComponent(table)}/indexes/${encodeURIComponent(column)}`, jsonInit('DELETE'), database);
}

export async function dropColumn(database, table, column) {
  await request(`/api/tables/${encodeURIComponent(table)}/columns/${encodeURIComponent(column)}`, jsonInit('DELETE'), database);
}

// ---------------------------------------------------------------- 行

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
  if (!isObject(body) || !Array.isArray(body.columns) || !body.columns.every((c) => typeof c === 'string')
      || !Array.isArray(body.rows) || typeof body.total !== 'number') {
    throw new Error('服务器返回的分页数据无法识别。');
  }
  const columns = body.columns;
  const rows = body.rows.map((raw) => {
    if (!Array.isArray(raw)) throw new Error('服务器返回的行数据无法识别。');
    return Object.fromEntries(columns.map((column, index) => [column, raw[index]]));
  });
  return { columns, rows, total: body.total };
}

export async function insertRow(database, table, values) {
  await request(`/api/tables/${encodeURIComponent(table)}/rows`, jsonInit('POST', { values }), database);
}

export async function updateRow(database, table, id, values) {
  await request(`/api/tables/${encodeURIComponent(table)}/rows/${encodeURIComponent(id)}`, jsonInit('PATCH', { values }), database);
}

export async function deleteRow(database, table, id) {
  await request(`/api/tables/${encodeURIComponent(table)}/rows/${encodeURIComponent(id)}`, jsonInit('DELETE'), database);
}

// 逐条提交，遇到第一条失败就停，返回已经成功了几条。
export async function commitChanges(database, changes) {
  let applied = 0;
  for (const [index, change] of changes.entries()) {
    try {
      if (change.op === 'insert') await insertRow(database, change.table, change.values);
      else if (change.op === 'delete') await deleteRow(database, change.table, change.pk);
      else await updateRow(database, change.table, change.pk, change.set);
      applied += 1;
    } catch (error) {
      return { ok: false, applied, errors: [{ index, message: error instanceof Error ? error.message : String(error) }] };
    }
  }
  return { ok: true, applied };
}

// ---------------------------------------------------------------- 查询

export async function query(sql, database) {
  const started = performance.now();
  try {
    const body = await request('/api/query', jsonInit('POST', { sql }), database);
    if (!isObject(body) || !Array.isArray(body.columns) || !Array.isArray(body.rows)) {
      throw new Error('服务器返回的查询结果无法识别。');
    }
    const durationMs = performance.now() - started;
    return {
      type: body.columns.length ? 'rows' : 'affected',
      columns: body.columns,
      rows: body.rows,
      affected: typeof body.rowCount === 'number' ? body.rowCount : 0,
      truncated: body.truncated === true,
      database: typeof body.database === 'string' ? body.database : undefined,
      durationMs,
    };
  } catch (error) {
    return { type: 'error', durationMs: performance.now() - started, message: error instanceof Error ? error.message : String(error) };
  }
}

export async function seedDemoData(database) {
  return request('/api/demo-data', jsonInit('POST', {}), database);
}

// ---------------------------------------------------------------- 设置

export async function loadServerSettings() {
  const body = await request('/api/settings');
  if (!isObject(body) || !Array.isArray(body.items)) throw new Error('服务器返回的设置清单无法识别。');
  return body;
}

export async function saveServerSettings(values) {
  const body = await request('/api/settings', jsonInit('PUT', { values }));
  return isObject(body) ? body : { ok: true };
}

export async function loadUiSettings() {
  return request('/api/ui-settings');
}

export async function saveUiSettings(settings) {
  await request('/api/ui-settings', jsonInit('PUT', settings));
}
