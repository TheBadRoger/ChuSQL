import type {
  ColumnMeta, CommitChange, DataSourceAdapter, FetchRowsParams, StatementResult, TableMeta,
} from '../types';

// REST 数据源：调用后端 JSON 接口并校验返回结构。

type JsonObject = Record<string, unknown>;

function isObject(value: unknown): value is JsonObject {
  return value !== null && typeof value === 'object' && !Array.isArray(value);
}

function asColumns(value: unknown): ColumnMeta[] {
  if (!Array.isArray(value)) throw new Error('The server returned an invalid column list.');
  return value.map((entry) => {
    if (!isObject(entry) || typeof entry.name !== 'string' || typeof entry.type !== 'string') {
      throw new Error('The server returned an invalid column description.');
    }
    if (!['int', 'str', 'bool', 'date'].includes(entry.type)) throw new Error(`Unsupported column type: ${entry.type}`);
    return {
      name: entry.name,
      type: entry.type as ColumnMeta['type'],
      ...(typeof entry.primaryKey === 'boolean' ? { primaryKey: entry.primaryKey } : {}),
      ...(typeof entry.indexed === 'boolean' ? { indexed: entry.indexed } : {}),
      ...(typeof entry.prime === 'boolean' ? { prime: entry.prime } : {}),
      ...(typeof entry.distinct === 'number' ? { distinct: entry.distinct } : {}),
      ...(typeof entry.statsCapped === 'boolean' ? { statsCapped: entry.statsCapped } : {}),
    };
  });
}

function asTable(value: unknown): TableMeta {
  if (!isObject(value) || typeof value.table !== 'string' || typeof value.rowCount !== 'number') {
    throw new Error('The server returned an invalid table description.');
  }
  return { name: value.table, rowCount: value.rowCount, columns: asColumns(value.columns) };
}

async function requestJson(url: string, init?: RequestInit): Promise<unknown> {
  const response = await fetch(url, { credentials: 'same-origin', ...init });
  let body: unknown;
  try {
    body = await response.json();
  } catch {
    body = undefined;
  }
  if (!response.ok) {
    const message = isObject(body) && typeof body.message === 'string'
      ? body.message
      : `Request failed with status ${response.status}.`;
    throw new Error(message);
  }
  return body;
}

function jsonInit(method: string, body?: unknown): RequestInit {
  return {
    method,
    headers: { 'Content-Type': 'application/json' },
    body: body === undefined ? undefined : JSON.stringify(body),
  };
}

function changeRequest(change: CommitChange): [string, RequestInit] {
  const table = encodeURIComponent(change.table);
  if (change.op === 'insert') return [`/api/tables/${table}/rows`, jsonInit('POST', { values: change.values })];
  const pk = encodeURIComponent(String(change.pk));
  if (change.op === 'delete') return [`/api/tables/${table}/rows/${pk}`, jsonInit('DELETE')];
  return [`/api/tables/${table}/rows/${pk}`, jsonInit('PATCH', { values: change.set })];
}

export class RestAdapter implements DataSourceAdapter {
  async createTable(name: string, columns: Array<Pick<ColumnMeta, 'name' | 'type'>>): Promise<void> {
    await requestJson('/api/tables', jsonInit('POST', { name, columns }));
  }
  async dropTable(table: string): Promise<void> {
    await requestJson(`/api/tables/${encodeURIComponent(table)}`, jsonInit('DELETE'));
  }
  async createIndex(table: string, column: string): Promise<void> {
    await requestJson(`/api/tables/${encodeURIComponent(table)}/indexes`, jsonInit('POST', { column }));
  }
  async dropIndex(table: string, column: string): Promise<void> {
    await requestJson(`/api/tables/${encodeURIComponent(table)}/indexes/${encodeURIComponent(column)}`, jsonInit('DELETE'));
  }
  async listTables(_db: string): Promise<TableMeta[]> {
    const body = await requestJson('/api/tables');
    if (!Array.isArray(body)) throw new Error('The server returned an invalid table list.');
    return body.map(asTable);
  }

  async getColumns(_db: string, table: string): Promise<ColumnMeta[]> {
    return asTable(await requestJson(`/api/tables/${encodeURIComponent(table)}`)).columns;
  }

  async fetchRows(params: FetchRowsParams): Promise<{ rows: Record<string, unknown>[]; total: number }> {
    const query = new URLSearchParams({
      limit: String(params.pageSize),
      offset: String(params.page * params.pageSize),
    });
    const firstSort = params.sort[0];
    if (firstSort) {
      query.set('sort', firstSort.column);
      query.set('dir', firstSort.dir);
    }
    if (params.filters.length) {
      query.set('filter', JSON.stringify(params.filters.map((filter) => ({ column: filter.column, value: filter.expression }))));
    }
    const body = await requestJson(`/api/tables/${encodeURIComponent(params.table)}/rows?${query}`);
    if (!isObject(body) || !Array.isArray(body.columns) || !body.columns.every((column) => typeof column === 'string') || !Array.isArray(body.rows) || typeof body.total !== 'number') {
      throw new Error('The server returned an invalid row page.');
    }
    const columns = body.columns as string[];
    const rows = body.rows.map((raw) => {
      if (!Array.isArray(raw)) throw new Error('The server returned an invalid row.');
      return Object.fromEntries(columns.map((column, index) => [column, raw[index]]));
    });
    return { rows, total: body.total };
  }

  async commitChanges(payload: { db: string; changes: CommitChange[] }): Promise<{ ok: boolean; applied: number; errors?: Array<{ index: number; message: string }> }> {
    let applied = 0;
    for (const [index, change] of payload.changes.entries()) {
      const [url, init] = changeRequest(change);
      try {
        await requestJson(url, init);
        applied += 1;
      } catch (error) {
        return {
          ok: false,
          applied,
          errors: [{ index, message: error instanceof Error ? error.message : String(error) }],
        };
      }
    }
    return { ok: true, applied };
  }

  async execStatement({ sql }: { db: string; sql: string }): Promise<StatementResult> {
    const started = performance.now();
    try {
      const body = await requestJson('/api/query', jsonInit('POST', { sql }));
      if (!isObject(body) || !Array.isArray(body.columns) || !Array.isArray(body.rows)) {
        throw new Error('The server returned an invalid query result.');
      }
      const durationMs = performance.now() - started;
      return body.columns.length
        ? { type: 'rows', columns: body.columns as string[], rows: body.rows as unknown[][], durationMs }
        : { type: 'affected', affected: typeof body.rowCount === 'number' ? body.rowCount : 0, durationMs };
    } catch (error) {
      return { type: 'error', durationMs: performance.now() - started, message: error instanceof Error ? error.message : String(error) };
    }
  }
}
