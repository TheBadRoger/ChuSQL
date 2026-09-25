// 共享类型定义：数据源适配器接口、元数据与查询结果结构。

export type DataValue = string | number | boolean | null;

export interface ColumnMeta {
  name: string;
  type: 'int' | 'str' | 'bool' | 'date';
  primaryKey?: boolean;
  indexed?: boolean;
  prime?: boolean;
  distinct?: number;
  statsCapped?: boolean;
}

export interface TableMeta {
  name: string;
  rowCount: number;
  columns: ColumnMeta[];
}

export interface FilterExpr {
  column: string;
  expression: string;
}

export interface FetchRowsParams {
  db: string;
  table: string;
  page: number;
  pageSize: number;
  sort: Array<{ column: string; dir: 'asc' | 'desc' }>;
  filters: FilterExpr[];
  search?: string;
}

export type SchemaChange =
  | { op: 'createTable'; table: string; columns: Array<Pick<ColumnMeta, 'name' | 'type'>> }
  | { op: 'dropTable'; table: string }
  | { op: 'createIndex'; table: string; column: string }
  | { op: 'dropIndex'; table: string; column: string };

export type CommitChange =
  | { op: 'update'; table: string; pk: unknown; set: Record<string, unknown> }
  | { op: 'insert'; table: string; values: Record<string, unknown> }
  | { op: 'delete'; table: string; pk: unknown };

export type PendingChange = CommitChange | SchemaChange;

export interface StatementResult {
  type: 'rows' | 'affected' | 'error';
  columns?: string[];
  rows?: unknown[][];
  affected?: number;
  durationMs: number;
  message?: string;
}

export interface DataSourceAdapter {
  createTable(name: string, columns: Array<Pick<ColumnMeta, 'name' | 'type'>>): Promise<void>;
  dropTable(table: string): Promise<void>;
  createIndex(table: string, column: string): Promise<void>;
  dropIndex(table: string, column: string): Promise<void>;
  listTables(db: string): Promise<TableMeta[]>;
  getColumns(db: string, table: string): Promise<ColumnMeta[]>;
  fetchRows(params: FetchRowsParams): Promise<{ rows: Record<string, unknown>[]; total: number }>;
  commitChanges(payload: {
    db: string;
    changes: CommitChange[];
  }): Promise<{ ok: boolean; applied: number; errors?: Array<{ index: number; message: string }> }>;
  execStatement(params: { db: string; sql: string }): Promise<StatementResult>;
}

export interface ChangeSet {
  updates: Map<string, {
    table: string;
    pk: unknown;
    column: string;
    oldValue: unknown;
    newValue: unknown;
  }>;
  inserts: Map<string, {
    table: string;
    tempId: string;
    values: Record<string, unknown>;
  }>;
  deletes: Map<string, {
    table: string;
    pk: unknown;
    row: Record<string, unknown>;
  }>;
  schemas: Map<string, SchemaChange>;
}
