import { matchesFilter } from '../core/filters';
import type {
  ColumnMeta, CommitChange, DataSourceAdapter, FetchRowsParams, StatementResult, TableMeta,
} from '../types';

// 内存数据源：内置 mock 表，供无后端环境演示与测试。

type TableData = { columns: ColumnMeta[]; rows: Array<Record<string, unknown>> };

function makeData(): Map<string, TableData> {
  const users = Array.from({ length: 500 }, (_, index) => ({
    id: index + 1,
    name: `User_${String(index + 1).padStart(3, '0')}`,
    age: 18 + (index % 55),
    active: index % 4 !== 0,
    joined_at: `202${index % 5}-${String((index % 12) + 1).padStart(2, '0')}-${String((index % 28) + 1).padStart(2, '0')}`,
  }));
  const orders = Array.from({ length: 500 }, (_, index) => ({
    id: index + 1,
    user_id: (index % 500) + 1,
    product: ['Keyboard', 'Monitor', 'Database Course', 'Hard Drive'][index % 4],
    amount: 49 + ((index * 37) % 2400),
    paid: index % 3 !== 0,
  }));
  return new Map([
    ['users', {
      columns: [
        { name: 'id', type: 'int', primaryKey: true, indexed: true },
        { name: 'name', type: 'str' },
        { name: 'age', type: 'int' },
        { name: 'active', type: 'bool' },
        { name: 'joined_at', type: 'date' },
      ], rows: users,
    }],
    ['orders', {
      columns: [
        { name: 'id', type: 'int', primaryKey: true, indexed: true },
        { name: 'user_id', type: 'int', indexed: true },
        { name: 'product', type: 'str' },
        { name: 'amount', type: 'int' },
        { name: 'paid', type: 'bool' },
      ], rows: orders,
    }],
  ]);
}

function compareValues(left: unknown, right: unknown): number {
  if (left === right) return 0;
  if (left === null || left === undefined) return -1;
  if (right === null || right === undefined) return 1;
  if (typeof left === 'number' && typeof right === 'number') return left - right;
  return String(left).localeCompare(String(right), 'zh-CN', { numeric: true });
}

export class MockAdapter implements DataSourceAdapter {
  async createTable(name: string, columns: Array<Pick<ColumnMeta, 'name' | 'type'>>): Promise<void> {
    await this.delay();
    if (!/^[A-Za-z_][A-Za-z0-9_]*$/.test(name) || this.tables.has(name)) throw new Error('Invalid or already existing table name');
    if (!columns.length || new Set(columns.map((column) => column.name)).size !== columns.length) throw new Error('Column names must be non-empty and unique');
    this.tables.set(name, { columns: columns.map((column) => ({ ...column, primaryKey: column.name === 'id', indexed: column.name === 'id' })), rows: [] });
  }
  async dropTable(table: string): Promise<void> {
    await this.delay();
    if (!this.tables.delete(table)) throw new Error('Table does not exist');
  }
  async createIndex(table: string, column: string): Promise<void> {
    await this.delay();
    const found = this.tables.get(table);
    const meta = found?.columns.find((item) => item.name === column);
    if (!found || !meta || meta.type !== 'int' || meta.indexed) throw new Error('Only unindexed integer columns can be indexed');
    if (new Set(found.rows.map((row) => row[column])).size !== found.rows.length) throw new Error('Index column contains duplicate values and must be unique');
    meta.indexed = true;
  }
  async dropIndex(table: string, column: string): Promise<void> {
    await this.delay();
    const meta = this.tables.get(table)?.columns.find((item) => item.name === column);
    if (!meta?.indexed || column === 'id') throw new Error('Index does not exist or is a built-in index that cannot be dropped');
    meta.indexed = false;
  }
  private readonly tables = makeData();
  private readonly latencyMs: number | [number, number];

  constructor(options: { latencyMs?: number | [number, number] } = {}) {
    this.latencyMs = options.latencyMs ?? [80, 200];
  }

  private async delay(): Promise<void> {
    const milliseconds = Array.isArray(this.latencyMs)
      ? this.latencyMs[0] + Math.random() * (this.latencyMs[1] - this.latencyMs[0])
      : this.latencyMs;
    if (milliseconds > 0) await new Promise((resolve) => window.setTimeout(resolve, milliseconds));
  }

  async listTables(_db: string): Promise<TableMeta[]> {
    await this.delay();
    return Array.from(this.tables, ([name, table]) => ({ name, rowCount: table.rows.length, columns: table.columns }));
  }

  async getColumns(_db: string, table: string): Promise<ColumnMeta[]> {
    await this.delay();
    const found = this.tables.get(table);
    if (!found) throw new Error(`Unknown table: ${table}`);
    return found.columns.map((column) => ({ ...column }));
  }

  async fetchRows(params: FetchRowsParams): Promise<{ rows: Record<string, unknown>[]; total: number }> {
    await this.delay();
    const found = this.tables.get(params.table);
    if (!found) throw new Error(`Unknown table: ${params.table}`);
    let rows = found.rows.filter((row) => params.filters.every((filter) => matchesFilter(row[filter.column], filter.expression)));
    if (params.search?.trim()) {
      const term = params.search.toLocaleLowerCase();
      rows = rows.filter((row) => Object.values(row).some((value) => String(value ?? '').toLocaleLowerCase().includes(term)));
    }
    if (params.sort.length) {
      rows = [...rows].sort((left, right) => {
        for (const sort of params.sort) {
          const compared = compareValues(left[sort.column], right[sort.column]);
          if (compared !== 0) return sort.dir === 'asc' ? compared : -compared;
        }
        return 0;
      });
    }
    const start = params.page * params.pageSize;
    return { rows: rows.slice(start, start + params.pageSize).map((row) => ({ ...row })), total: rows.length };
  }

  async commitChanges(payload: { db: string; changes: CommitChange[] }): Promise<{ ok: boolean; applied: number; errors?: Array<{ index: number; message: string }> }> {
    await this.delay();
    const working = new Map(Array.from(this.tables, ([name, table]) => [name, { columns: table.columns, rows: table.rows.map((row) => ({ ...row })) }]));
    const errors: Array<{ index: number; message: string }> = [];
    payload.changes.forEach((change, index) => {
      const table = working.get(change.table);
      if (!table) {
        errors.push({ index, message: `Unknown table: ${change.table}` });
        return;
      }
      if (change.op === 'insert') {
        if (table.rows.some((row) => row.id === change.values.id)) errors.push({ index, message: `Duplicate id: ${String(change.values.id)}` });
        else table.rows.push({ ...change.values });
        return;
      }
      const rowIndex = table.rows.findIndex((row) => row.id === change.pk);
      if (rowIndex < 0) {
        errors.push({ index, message: `Row not found: ${String(change.pk)}` });
      } else if (change.op === 'delete') {
        table.rows.splice(rowIndex, 1);
      } else {
        table.rows[rowIndex] = { ...table.rows[rowIndex], ...change.set };
      }
    });
    if (errors.length) return { ok: false, applied: 0, errors };
    this.tables.clear();
    for (const [name, table] of working) this.tables.set(name, table);
    return { ok: true, applied: payload.changes.length };
  }

  async execStatement({ sql }: { db: string; sql: string }): Promise<StatementResult> {
    const started = performance.now();
    await this.delay();
    const normalized = sql.trim().replace(/;$/, '');
    const select = normalized.match(/^select\s+(.+?)\s+from\s+([A-Za-z_][A-Za-z0-9_]*)/is);
    if (select) {
      const table = this.tables.get(select[2]);
      if (!table) return { type: 'error', durationMs: performance.now() - started, message: `Unknown table: ${select[2]}` };
      const columns = select[1].trim() === '*' ? table.columns.map((column) => column.name) : select[1].split(',').map((item) => item.trim());
      return {
        type: 'rows', columns,
        rows: table.rows.slice(0, 200).map((row) => columns.map((column) => row[column])),
        durationMs: performance.now() - started,
      };
    }
    if (/^(insert|update|delete|create|drop)\b/i.test(normalized)) {
      return { type: 'affected', affected: 1, durationMs: performance.now() - started };
    }
    return { type: 'error', durationMs: performance.now() - started, message: 'MockAdapter cannot execute this statement' };
  }
}
