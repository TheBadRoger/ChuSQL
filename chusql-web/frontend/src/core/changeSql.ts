import type { ChangeSet, PendingChange, SchemaChange } from '../types';
import { pendingChanges } from './changes';

// 展示用 SQL 生成：标识符走白名单，字符串单引号翻倍。

const identifier = /^[A-Za-z_][A-Za-z0-9_]*$/;

// 字面量：字符串加引号并翻倍单引号。
export function sqlLiteral(value: unknown): string {
  if (value === null || value === undefined) return 'NULL';
  if (typeof value === 'number' || typeof value === 'boolean') return String(value);
  return `'${String(value).replaceAll("'", "''")}'`;
}

// 标识符：白名单外才加双引号。
export function sqlIdentifier(name: string): string {
  return identifier.test(name) ? name : `"${name.replaceAll('"', '""')}"`;
}

// 单元格修改语句。
export function updateSql(table: string, pk: unknown, column: string, value: unknown): string {
  return `UPDATE ${sqlIdentifier(table)} SET ${sqlIdentifier(column)} = ${sqlLiteral(value)} WHERE id = ${sqlLiteral(pk)};`;
}

// 插入语句。
export function insertSql(table: string, values: Record<string, unknown>): string {
  const columns = Object.keys(values);
  return `INSERT INTO ${sqlIdentifier(table)} (${columns.map(sqlIdentifier).join(', ')}) VALUES (${columns.map((column) => sqlLiteral(values[column])).join(', ')});`;
}

// 删除语句。
export function deleteSql(table: string, pk: unknown): string {
  return `DELETE FROM ${sqlIdentifier(table)} WHERE id = ${sqlLiteral(pk)};`;
}

// 结构变更语句，与 Actions 拼法一致。
export function schemaSql(change: SchemaChange): string {
  if (change.op === 'createTable') {
    const columns = change.columns.map((column) => `${sqlIdentifier(column.name)} ${column.type}`).join(', ');
    return `CREATE TABLE ${sqlIdentifier(change.table)} (${columns});`;
  }
  if (change.op === 'dropTable') return `DROP TABLE ${sqlIdentifier(change.table)};`;
  if (change.op === 'createIndex') return `CREATE INDEX ON ${sqlIdentifier(change.table)} (${sqlIdentifier(change.column)});`;
  return `DROP INDEX ON ${sqlIdentifier(change.table)} (${sqlIdentifier(change.column)});`;
}

// 渲染一条提交项为展示用 SQL。
export function changeSql(change: PendingChange): string {
  if (change.op === 'insert') return insertSql(change.table, change.values);
  if (change.op === 'delete') return deleteSql(change.table, change.pk);
  if (change.op === 'update') {
    const assignments = Object.entries(change.set).map(([column, value]) => `${sqlIdentifier(column)} = ${sqlLiteral(value)}`).join(', ');
    return `UPDATE ${sqlIdentifier(change.table)} SET ${assignments} WHERE id = ${sqlLiteral(change.pk)};`;
  }
  return schemaSql(change);
}

// 按提交顺序渲染整套变更。
export function changesSql(changes: ChangeSet): string[] {
  return pendingChanges(changes).map(changeSql);
}
