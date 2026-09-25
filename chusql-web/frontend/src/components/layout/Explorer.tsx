import { ChevronDown, Database, FileClock, GitCompareArrows, Table2, Plus, ListFilter, Trash2 } from 'lucide-react';
import { IconButton } from '../common/IconButton';
import type { ColumnMeta, TableMeta } from '../../types';
import { columnTypeLabels, zhCN } from '../../i18n/zh-CN';
import styles from './layout.module.css';
import { useState } from 'react';

// 资源管理器：库-表-列树、变更集与查询历史入口。
interface ExplorerProps {
  tables: TableMeta[];
  activeTable?: string;
  history: Array<{ sql: string; at: string }>;
  onOpenTable: (name: string) => void;
  onOpenSql: (sql?: string) => void;
  onCreateTable?: () => void;
  onIndexes?: (table: TableMeta) => void;
  onDropTable?: (table: TableMeta) => void;
}

// 列图标的可访问名，按主键/索引/普通区分。
function columnKind(column: ColumnMeta): string {
  if (column.primaryKey) return zhCN.columnPrimaryKey;
  return column.indexed ? zhCN.columnIndexed : zhCN.columnOrdinary;
}

export function Explorer({ tables, activeTable, history, onOpenTable, onOpenSql, onCreateTable, onIndexes, onDropTable }: ExplorerProps) {
  const [expanded, setExpanded] = useState(new Set<string>());
  return (
    <aside className={styles.sidebar} aria-label={zhCN.explorer}>
      <h2>{zhCN.explorer}</h2>
      {onCreateTable && <IconButton icon={<Plus size={14} />} label={zhCN.newTable} onClick={onCreateTable} />}
      <div className={styles.sectionTitle}><ChevronDown size={13} />{zhCN.databaseSection}</div>
      <div className={styles.treeRoot}><Database size={14} /><span>{zhCN.database}</span></div>
      <ul className={styles.tableList}>
        {tables.map((table) => (
          <li key={table.name}>
            <div className={styles.tableEntry}>
            <button className={styles.expandButton} aria-label={zhCN.expandColumns(table.name)} aria-expanded={expanded.has(table.name)} onClick={() => setExpanded((current) => { const next = new Set(current); if (next.has(table.name)) next.delete(table.name); else next.add(table.name); return next; })}><svg width="12" height="12" viewBox="0 0 16 16" fill="none" stroke="currentColor" strokeWidth="1.5" aria-hidden="true"><path d={expanded.has(table.name) ? 'm4 6 4 4 4-4' : 'm6 4 4 4-4 4'} /></svg></button>
            <button
              className={activeTable === table.name ? styles.activeTreeItem : styles.treeItem}
              aria-label={zhCN.openTable(table.name)}
              onClick={() => onOpenTable(table.name)}
            >
              <Table2 size={13} /><span>{table.name}</span><small>{table.rowCount}</small>
            </button>
            {onIndexes && <IconButton compact icon={<ListFilter size={13} />} label={zhCN.manageIndexes(table.name)} onClick={() => onIndexes(table)} />}
            {onDropTable && <IconButton compact icon={<Trash2 size={13} />} label={zhCN.dropTableAction(table.name)} onClick={() => onDropTable(table)} />}
            </div>
            {expanded.has(table.name) && <ul className={styles.columnList}>{table.columns.map((column) => <li key={column.name}><svg width="16" height="16" viewBox="0 0 16 16" fill="none" stroke="currentColor" strokeWidth="1.3" role="img" aria-label={columnKind(column)}><title>{columnKind(column)}</title>{column.primaryKey ? <><circle cx="5" cy="5" r="3" /><path d="m7 7 6 6m-3-3 2-2m0 4 2-2" /></> : column.indexed ? <><path d="M2 3h9v10H2ZM5 3v10M2 6h9M2 10h9m10-5 4 3-4 3" /></> : <><rect x="4" y="2" width="8" height="12" rx="1" /><path d="M4 6h8M4 10h8" /></>}</svg><span>{column.name}</span><small>{columnTypeLabels[column.type] ?? column.type}</small></li>)}</ul>}
          </li>
        ))}
      </ul>
      <div className={styles.sectionTitle}><GitCompareArrows size={13} />{zhCN.changesSection}</div>
      <div className={styles.sectionTitle}><FileClock size={13} />{zhCN.historySection}</div>
      <button className={styles.treeItem} onClick={() => onOpenSql()}>{zhCN.newQuery}</button>
      <ul className={styles.historyList}>{history.slice(0, 8).map((entry) => <li key={`${entry.at}-${entry.sql}`}><button title={entry.sql} onClick={() => onOpenSql(entry.sql)}>{entry.sql}</button></li>)}</ul>
    </aside>
  );
}
