import { useState } from 'react';
import { schemaSql } from '../../core/changeSql';
import { columnTypeLabels, zhCN } from '../../i18n/zh-CN';
import { useChangeStore } from '../../store/changeStore';
import type { ColumnMeta, SchemaChange, TableMeta } from '../../types';
import { Modal } from '../common/Modal';
import styles from '../grid/grid.module.css';

// 建表、删表与索引对话框：结构变更一律暂存，提交时才下发。
export type SchemaAction = { kind: 'create' } | { kind: 'drop' | 'indexes'; table: TableMeta };
const identifier = /^[A-Za-z_][A-Za-z0-9_]*$/;

// 列类型的中文名，未知类型原样显示。
function typeLabel(type: string): string {
  return columnTypeLabels[type] ?? type;
}

export function SchemaDialog({ action, onClose, onChanged }: {
  action: SchemaAction; onClose: () => void; onChanged: (message: string) => void;
}) {
  const [name, setName] = useState('');
  const [columns, setColumns] = useState<Array<Pick<ColumnMeta, 'name' | 'type'>>>([{ name: 'id', type: 'int' }, { name: '', type: 'str' }]);
  const [error, setError] = useState('');
  const stageSchema = useChangeStore((state) => state.stageSchema);
  const schemas = useChangeStore((state) => state.changes.schemas);
  const target = action.kind === 'create' ? undefined : action.table;
  const stagedIndex = new Map<string, boolean>();
  for (const change of schemas.values()) {
    if (change.table !== target?.name) continue;
    if (change.op === 'createIndex') stagedIndex.set(change.column, true);
    else if (change.op === 'dropIndex') stagedIndex.set(change.column, false);
  }
  const stage = (change: SchemaChange) => { stageSchema(change); onChanged(zhCN.stagedMessage(schemaSql(change))); };
  const create = () => {
    if (!identifier.test(name) || columns.some((column) => !identifier.test(column.name))) return setError(zhCN.invalidIdentifier);
    if (new Set(columns.map((column) => column.name)).size !== columns.length) return setError(zhCN.duplicateColumns);
    stage({ op: 'createTable', table: name, columns });
    onClose();
  };
  const drop = () => {
    if (!target) return;
    stage({ op: 'dropTable', table: target.name });
    onClose();
  };
  const changeIndex = (column: string, indexed: boolean) => {
    if (!target) return;
    stage(indexed
      ? { op: 'dropIndex', table: target.name, column }
      : { op: 'createIndex', table: target.name, column });
  };
  const title = action.kind === 'create' ? zhCN.newTableTitle : `${action.kind === 'drop' ? zhCN.dropTableTitle : zhCN.indexManagerTitle} · ${target?.name}`;
  return <Modal title={title} onClose={onClose} footer={<><button onClick={onClose}>{action.kind === 'indexes' ? zhCN.done : zhCN.cancel}</button>{action.kind === 'create' && <button className={styles.primaryButton} onClick={create}>{zhCN.createTable}</button>}{action.kind === 'drop' && <button className={styles.dangerButton} onClick={drop}>{zhCN.dropTablePermanently}</button>}</>}>
    {action.kind === 'create' ? <div className={styles.formFields}>
      <label>{zhCN.tableName}<input autoFocus aria-label={zhCN.tableName} value={name} onChange={(event) => setName(event.target.value)} /></label>
      <p>{zhCN.createTableHint}</p>
      {columns.map((column, index) => <div className={styles.columnDefinition} key={index}><input aria-label={zhCN.columnName(index + 1)} value={column.name} disabled={index === 0} onChange={(event) => setColumns(columns.map((item, i) => i === index ? { ...item, name: event.target.value } : item))} /><select aria-label={zhCN.columnType(index + 1)} value={column.type} disabled={index === 0} onChange={(event) => setColumns(columns.map((item, i) => i === index ? { ...item, type: event.target.value as ColumnMeta['type'] } : item))}><option value="int">{zhCN.typeInt}</option><option value="str">{zhCN.typeStr}</option><option value="bool">{zhCN.typeBool}</option></select><button disabled={index === 0} aria-label={zhCN.removeColumn(index + 1)} onClick={() => setColumns(columns.filter((_, i) => i !== index))}>{zhCN.remove}</button></div>)}
      <button onClick={() => setColumns([...columns, { name: '', type: 'str' }])}>{zhCN.addColumn}</button>
    </div> : action.kind === 'drop' ? <>
      <p>{zhCN.dropTableHint(target?.name ?? '', target?.rowCount ?? 0)}</p>
    </> : <>
      <p>{zhCN.indexHint}</p>
      <ul className={styles.indexList}>{target?.columns.map((column) => {
        const indexed = stagedIndex.get(column.name) ?? Boolean(column.indexed);
        return <li key={column.name}><span>{column.name} <small>{typeLabel(column.type)}</small></span><span>{column.primaryKey || column.name === 'id' ? zhCN.builtinIndex : indexed ? zhCN.uniqueIndex : zhCN.noIndex}</span><button disabled={column.name === 'id' || (!indexed && column.type !== 'int')} onClick={() => changeIndex(column.name, indexed)}>{indexed ? zhCN.deleteIndex : zhCN.createIndex}</button></li>;
      })}</ul>
    </>}
    {error && <p role="alert">{error}</p>}
  </Modal>;
}
