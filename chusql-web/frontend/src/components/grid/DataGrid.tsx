import { useMemo, useState, type ClipboardEvent, type KeyboardEvent, type PointerEvent } from 'react';
import type { ChangeSet, ColumnMeta } from '../../types';
import { changeKey } from '../../core/changes';
import { zhCN } from '../../i18n/zh-CN';
import styles from './grid.module.css';

// 数据网格：列排序、筛选、列宽拖拽、行选中与单元格编辑。
export interface GridRow extends Record<string, unknown> { _tempId?: string }

export type SelectionMode = 'single' | 'toggle' | 'range';

interface DataGridProps {
  table: string;
  columns: ColumnMeta[];
  rows: GridRow[];
  changes: ChangeSet;
  nullText: string;
  sort?: { column: string; dir: 'asc' | 'desc' };
  filters: Record<string, string>;
  widths: Record<string, number>;
  selectedRows: Set<string>;
  onSort: (column: string) => void;
  onFilter: (column: string, expression: string) => void;
  onWidth: (column: string, width: number) => void;
  onSelectRow: (index: number, mode: SelectionMode) => void;
  onEdit: (row: GridRow, column: string, oldValue: unknown, newValue: unknown) => void;
  onDelete: (row: GridRow) => void;
}

interface EditingCell { rowIndex: number; column: string; value: unknown }

// 行键：临时行用 tempId，其余用主键。
export function rowKey(row: GridRow, index: number): string { return row._tempId ?? String(row.id ?? index); }

function valueFor(row: GridRow, table: string, column: string, changes: ChangeSet): unknown {
  if (row._tempId) return changes.inserts.get(row._tempId)?.values[column] ?? row[column];
  return changes.updates.get(changeKey(table, row.id, column))?.newValue ?? row[column];
}

function Editor({ column, value, onChange, onKeyDown, onBlur }: { column: ColumnMeta; value: unknown; onChange: (value: unknown) => void; onKeyDown: (event: KeyboardEvent<HTMLInputElement | HTMLSelectElement>) => void; onBlur: () => void }) {
  if (column.type === 'bool') return <select autoFocus aria-label={zhCN.editCell(column.name)} value={String(value)} onChange={(event) => onChange(event.target.value)} onKeyDown={onKeyDown} onBlur={onBlur}><option value="true">true</option><option value="false">false</option></select>;
  return <input autoFocus aria-label={zhCN.editCell(column.name)} type={column.type === 'int' ? 'number' : column.type === 'date' ? 'date' : 'text'} value={String(value ?? '')} onChange={(event) => onChange(event.target.value)} onKeyDown={onKeyDown} onBlur={onBlur} />;
}

export function DataGrid(props: DataGridProps) {
  const [editing, setEditing] = useState<EditingCell>();
  const [filterColumn, setFilterColumn] = useState<string>();
  const deleted = useMemo(() => new Set(Array.from(props.changes.deletes.values()).filter((item) => item.table === props.table).map((item) => String(item.pk))), [props.changes, props.table]);
  const commitEdit = () => {
    if (!editing) return;
    const row = props.rows[editing.rowIndex];
    const column = props.columns.find((item) => item.name === editing.column);
    if (!row || !column) return setEditing(undefined);
    let next: unknown = editing.value;
    if (column.type === 'int') next = Number(editing.value);
    if (column.type === 'bool') next = editing.value === true || editing.value === 'true';
    props.onEdit(row, column.name, valueFor(row, props.table, column.name, props.changes), next);
    setEditing(undefined);
  };
  const onEditorKeyDown = (event: KeyboardEvent<HTMLInputElement | HTMLSelectElement>) => {
    if (event.key === 'Escape') setEditing(undefined);
    if (event.key === 'Enter' || event.key === 'Tab') { event.preventDefault(); commitEdit(); }
  };
  const paste = (event: ClipboardEvent<HTMLTableCellElement>, rowIndex: number, columnIndex: number) => {
    const matrix = event.clipboardData.getData('text/plain').replace(/\r/g, '').split('\n').filter((line) => line.length).map((line) => line.split('\t'));
    if (!matrix.length) return;
    event.preventDefault();
    matrix.forEach((values, y) => values.forEach((value, x) => {
      const row = props.rows[rowIndex + y];
      const column = props.columns[columnIndex + x];
      if (row && column && !column.primaryKey) props.onEdit(row, column.name, valueFor(row, props.table, column.name, props.changes), value);
    }));
  };
  const beginResize = (event: PointerEvent<HTMLSpanElement>, column: string) => {
    event.preventDefault();
    const startX = event.clientX;
    const startWidth = props.widths[column] ?? 140;
    const move = (next: globalThis.PointerEvent) => props.onWidth(column, Math.max(70, startWidth + next.clientX - startX));
    const stop = () => { window.removeEventListener('pointermove', move); window.removeEventListener('pointerup', stop); };
    window.addEventListener('pointermove', move); window.addEventListener('pointerup', stop);
  };
  return (
    <table className={styles.grid} style={{ width: props.columns.reduce((sum, column) => sum + (props.widths[column.name] ?? 140), 0) }}>
      <colgroup>{props.columns.map((column) => <col key={column.name} style={{ width: props.widths[column.name] ?? 140 }} />)}</colgroup>
      <thead>
        <tr>{props.columns.map((column) => <th key={column.name} aria-sort={props.sort?.column === column.name ? props.sort.dir === 'asc' ? 'ascending' : 'descending' : 'none'}>
          <div className={styles.columnHeader}><button className={styles.columnButton} aria-label={column.name} onClick={() => props.onSort(column.name)}>{column.name}<span>{props.sort?.column === column.name ? props.sort.dir === 'asc' ? '▲' : '▼' : ''}</span></button>
          <button className={styles.filterButton} aria-label={zhCN.filter(column.name)} aria-expanded={filterColumn === column.name} aria-pressed={Boolean(props.filters[column.name])} onClick={() => setFilterColumn(filterColumn === column.name ? undefined : column.name)}><svg width="14" height="14" viewBox="0 0 16 16" fill="none" stroke="currentColor" strokeWidth="1.4" aria-hidden="true"><path d="M2 3h12L9.5 8v5l-3-1.5V8Z" /></svg></button></div>
          {filterColumn === column.name && <div className={styles.filterPopover}><input autoFocus aria-label={zhCN.filter(column.name)} placeholder={zhCN.filterPlaceholder} value={props.filters[column.name] ?? ''} onChange={(event) => props.onFilter(column.name, event.target.value)} onKeyDown={(event) => { if (event.key === 'Escape' || event.key === 'Enter') { setFilterColumn(undefined); event.currentTarget.closest('th')?.querySelector('button')?.focus(); } }} /><button onClick={() => props.onFilter(column.name, '')}>{zhCN.clear}</button></div>}
          <span className={styles.resizeHandle} role="separator" aria-label={zhCN.resizeColumn(column.name)} aria-orientation="vertical" tabIndex={0} onKeyDown={(event) => { if (event.key === 'ArrowLeft' || event.key === 'ArrowRight') { event.preventDefault(); props.onWidth(column.name, Math.max(70, (props.widths[column.name] ?? 140) + (event.key === 'ArrowRight' ? 10 : -10))); } }} onPointerDown={(event) => beginResize(event, column.name)} />
        </th>)}</tr>
      </thead>
      <tbody>
        {props.rows.map((row, rowIndex) => {
          const key = rowKey(row, rowIndex);
          const isDeleted = !row._tempId && deleted.has(String(row.id));
          return <tr key={key} aria-selected={props.selectedRows.has(key)} className={`${isDeleted ? styles.deleted : ''} ${row._tempId ? styles.inserted : ''}`} onContextMenu={(event) => { event.preventDefault(); props.onDelete(row); }}>
            {props.columns.map((column, columnIndex) => {
              const value = valueFor(row, props.table, column.name, props.changes);
              const dirty = row._tempId || props.changes.updates.has(changeKey(props.table, row.id, column.name));
              const isEditing = editing?.rowIndex === rowIndex && editing.column === column.name;
              return <td key={column.name} className={dirty ? styles.dirty : ''} tabIndex={0} onClick={(event) => { if (!isEditing && event.detail < 2) props.onSelectRow(rowIndex, event.ctrlKey || event.metaKey ? 'toggle' : event.shiftKey ? 'range' : 'single'); }} onKeyDown={(event) => { if (!isEditing && event.key === ' ') { event.preventDefault(); props.onSelectRow(rowIndex, event.ctrlKey || event.metaKey ? 'toggle' : 'single'); } }} onPaste={(event) => paste(event, rowIndex, columnIndex)} onDoubleClick={() => !column.primaryKey && setEditing({ rowIndex, column: column.name, value })}>
                {isEditing ? <Editor column={column} value={editing.value} onChange={(value) => setEditing({ ...editing, value })} onKeyDown={onEditorKeyDown} onBlur={commitEdit} /> : value === null || value === undefined ? <span className={styles.nullValue}>{props.nullText}</span> : String(value)}
              </td>;
            })}
          </tr>;
        })}
      </tbody>
    </table>
  );
}
