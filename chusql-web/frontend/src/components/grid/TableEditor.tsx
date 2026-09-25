import { Plus, RefreshCw, Replace, Trash2 } from 'lucide-react';
import { useEffect, useMemo, useRef, useState } from 'react';
import type { DataSourceAdapter, FilterExpr, TableMeta } from '../../types';
import type { IdeSettings } from '../../store/settingsStore';
import { useChangeStore } from '../../store/changeStore';
import { zhCN } from '../../i18n/zh-CN';
import { IconButton } from '../common/IconButton';
import { DataGrid, rowKey, type GridRow, type SelectionMode } from './DataGrid';
import { RowDialog } from './RowDialog';
import styles from './grid.module.css';

// 表数据编辑器：分页加载、工具栏操作与批量删除/替换。
interface TableEditorProps { adapter: DataSourceAdapter; table: TableMeta; settings: IdeSettings; revision: number; onTotal: (count: number) => void; onMessage: (message: string) => void }

export function TableEditor({ adapter, table, settings, revision, onTotal, onMessage }: TableEditorProps) {
  const [rows, setRows] = useState<GridRow[]>([]);
  const [page, setPage] = useState(0);
  const [total, setTotal] = useState(0);
  const [sort, setSort] = useState<{ column: string; dir: 'asc' | 'desc' }>();
  const [filters, setFilters] = useState<Record<string, string>>({});
  const [widths, setWidths] = useState<Record<string, number>>(() => JSON.parse(localStorage.getItem(`chusql.widths.${table.name}`) ?? '{}'));
  const [selectedRows, setSelectedRows] = useState(new Set<string>());
  const [selectionAnchor, setSelectionAnchor] = useState<number>();
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState('');
  const [replaceOpen, setReplaceOpen] = useState(false);
  const [find, setFind] = useState('');
  const [replacement, setReplacement] = useState('');
  const [insertOpen, setInsertOpen] = useState(false);
  const requestId = useRef(0);
  const { changes, updateCell, insertRow, updateInsert, deleteRow } = useChangeStore();
  const inserted = useMemo(() => Array.from(changes.inserts.values()).filter((item) => item.table === table.name).map((item): GridRow => ({ ...item.values, _tempId: item.tempId })), [changes, table.name]);
  const visibleRows: GridRow[] = [...rows, ...inserted];
  const activeFilters: FilterExpr[] = Object.entries(filters).filter(([, value]) => value.trim()).map(([column, expression]) => ({ column, expression }));

  const load = async (nextPage: number, append: boolean) => {
    const id = ++requestId.current; setLoading(true); setError('');
    try {
      const result = await adapter.fetchRows({ db: zhCN.database, table: table.name, page: nextPage, pageSize: settings.pageSize, sort: sort ? [sort] : [], filters: activeFilters });
      if (id !== requestId.current) return;
      setRows((current) => append ? [...current, ...result.rows] : result.rows);
      if (!append) { setSelectedRows(new Set()); setSelectionAnchor(undefined); }
      setTotal(result.total); setPage(nextPage); onTotal(result.total);
    } catch (reason) { if (id === requestId.current) setError(reason instanceof Error ? reason.message : String(reason)); }
    finally { if (id === requestId.current) setLoading(false); }
  };

  useEffect(() => { const timer = window.setTimeout(() => void load(0, false), 180); return () => window.clearTimeout(timer); }, [table.name, settings.pageSize, sort?.column, sort?.dir, JSON.stringify(filters), revision]);
  const sortColumn = (column: string) => setSort((current) => current?.column !== column ? { column, dir: 'asc' } : { column, dir: current.dir === 'asc' ? 'desc' : 'asc' });
  const setWidth = (column: string, width: number) => setWidths((current) => { const next = { ...current, [column]: width }; localStorage.setItem(`chusql.widths.${table.name}`, JSON.stringify(next)); return next; });
  const addRow = (values: Record<string, unknown>) => {
    const tempId = `temp-${crypto.randomUUID()}`;
    insertRow(table.name, tempId, values);
    setInsertOpen(false);
    onMessage(zhCN.rowStaged);
  };
  const stageDelete = (row: GridRow) => {
    deleteRow(table.name, row._tempId ?? row.id, row, Boolean(row._tempId));
  };
  const selectRow = (index: number, mode: SelectionMode) => {
    const key = rowKey(visibleRows[index], index);
    if (mode === 'range' && selectionAnchor !== undefined) {
      const from = Math.min(selectionAnchor, index);
      const to = Math.max(selectionAnchor, index);
      setSelectedRows(new Set(visibleRows.slice(from, to + 1).map((row, offset) => rowKey(row, from + offset))));
      return;
    }
    if (mode === 'toggle') {
      setSelectedRows((current) => { const next = new Set(current); if (next.has(key)) next.delete(key); else next.add(key); return next; });
      setSelectionAnchor(index);
      return;
    }
    setSelectedRows(new Set([key]));
    setSelectionAnchor(index);
  };
  const removeSelected = () => {
    if (!selectedRows.size) return onMessage(zhCN.selectedRowsRequired);
    visibleRows.forEach((row, index) => { if (selectedRows.has(row._tempId ?? String(row.id ?? index))) stageDelete(row); });
    setSelectedRows(new Set());
    setSelectionAnchor(undefined);
  };
  const replaceSelected = () => {
    if (!selectedRows.size) return onMessage(zhCN.selectedRowsRequired);
    visibleRows.forEach((row, index) => {
      const key = row._tempId ?? String(row.id ?? index); if (!selectedRows.has(key)) return;
      table.columns.filter((column) => !column.primaryKey && column.type === 'str').forEach((column) => {
        const oldValue = row._tempId ? changes.inserts.get(row._tempId)?.values[column.name] : row[column.name];
        if (String(oldValue ?? '').includes(find)) {
          const next = String(oldValue ?? '').replaceAll(find, replacement);
          if (row._tempId) updateInsert(row._tempId, { ...(changes.inserts.get(row._tempId)?.values ?? row), [column.name]: next });
          else updateCell(table.name, row.id, column.name, row[column.name], next);
        }
      });
    }); setReplaceOpen(false);
  };

  return <section className={styles.tableEditor}>
    <div className={styles.toolbar}>
      <IconButton icon={<RefreshCw size={14} />} label={zhCN.refresh} onClick={() => void load(0, false)} />
      <IconButton icon={<Plus size={14} />} label={zhCN.insertRow} onClick={() => setInsertOpen(true)} />
      <IconButton icon={<Trash2 size={14} />} label={zhCN.deleteRows} disabled={!selectedRows.size} onClick={removeSelected} />
      <IconButton icon={<Replace size={14} />} label={zhCN.batchReplace} onClick={() => selectedRows.size ? setReplaceOpen(true) : onMessage(zhCN.selectedRowsRequired)} />
      <span className={styles.spacer} /><span title={zhCN.selectRowsHint}>{zhCN.selectedRows(selectedRows.size)}</span><span>{zhCN.rowCount(total)}</span>
    </div>
    {replaceOpen && <div className={styles.replaceBar}><label>{zhCN.findText}<input value={find} onChange={(event) => setFind(event.target.value)} /></label><label>{zhCN.replaceText}<input value={replacement} onChange={(event) => setReplacement(event.target.value)} /></label><button onClick={replaceSelected}>{zhCN.replaceSelected}</button><button onClick={() => setReplaceOpen(false)}>{zhCN.close}</button></div>}
    <div className={styles.viewport} onScroll={(event) => { const target = event.currentTarget; if (!loading && rows.length < total && target.scrollTop + target.clientHeight >= target.scrollHeight - 32) void load(page + 1, true); }}>
      {error ? <div role="alert" className={styles.empty}>{zhCN.loadError}: {error} <button onClick={() => void load(0, false)}>{zhCN.retry}</button></div> : <DataGrid table={table.name} columns={table.columns} rows={visibleRows} changes={changes} nullText={settings.nullText} sort={sort} filters={filters} widths={widths} selectedRows={selectedRows} onSort={sortColumn} onFilter={(column, expression) => setFilters((current) => ({ ...current, [column]: expression }))} onWidth={setWidth} onSelectRow={selectRow} onEdit={(row, column, oldValue, newValue) => row._tempId ? updateInsert(row._tempId, { ...(changes.inserts.get(row._tempId)?.values ?? row), [column]: newValue }) : updateCell(table.name, row.id, column, oldValue, newValue)} onDelete={stageDelete} />}
      {loading && <div className={styles.loading}>{zhCN.loading}</div>}
      {!loading && !visibleRows.length && !error && <div className={styles.empty}>{zhCN.noRows}</div>}
    </div>
    {insertOpen && <RowDialog table={table} onClose={() => setInsertOpen(false)} onInsert={addRow} />}
  </section>;
}
