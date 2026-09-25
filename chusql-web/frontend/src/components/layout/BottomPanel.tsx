import { Undo2 } from 'lucide-react';
import { useState } from 'react';
import type { ChangeSet, SchemaChange, StatementResult } from '../../types';
import { deleteSql, insertSql, schemaSql, updateSql } from '../../core/changeSql';
import { SQL_PLAIN_COLOR, SQL_TOKEN_COLORS, tokenizeSql } from '../../core/sqlHighlight';
import { zhCN } from '../../i18n/zh-CN';
import { IconButton } from '../common/IconButton';
import styles from './bottom.module.css';

// 底部面板：待提交变更、查询结果与消息三个标签页。
interface BottomPanelProps { changes: ChangeSet; results: StatementResult[]; messages: string[]; onUndoOne: (kind: keyof ChangeSet, key: string) => void }

// 结构变更的中文描述。
function schemaLabel(change: SchemaChange): string {
  if (change.op === 'createTable') return zhCN.createTableChange(change.table);
  if (change.op === 'dropTable') return zhCN.dropTableChange(change.table);
  if (change.op === 'createIndex') return zhCN.createIndexChange(change.table, change.column);
  return zhCN.dropIndexChange(change.table, change.column);
}

// 按执行器的 token 配色渲染一条 SQL。
function SqlText({ sql }: { sql: string }) {
  return <>{tokenizeSql(sql).map((token, index) => <span key={index} style={{ color: token.kind === 'plain' ? SQL_PLAIN_COLOR : SQL_TOKEN_COLORS[token.kind] }}>{token.text}</span>)}</>;
}

export function BottomPanel({ changes, results, messages, onUndoOne }: BottomPanelProps) {
  const [tab, setTab] = useState<'changes' | 'results' | 'messages'>('changes');
  const [resultIndex, setResultIndex] = useState(0);
  const entries = [
    ...Array.from(changes.updates, ([key, value]) => ({ kind: 'updates' as const, key, label: zhCN.updateChange(value.table, value.column), detail: updateSql(value.table, value.pk, value.column, value.newValue) })),
    ...Array.from(changes.inserts, ([key, value]) => ({ kind: 'inserts' as const, key, label: zhCN.insertChange(value.table), detail: insertSql(value.table, value.values) })),
    ...Array.from(changes.deletes, ([key, value]) => ({ kind: 'deletes' as const, key, label: zhCN.deleteChange(value.table), detail: deleteSql(value.table, value.pk) })),
    ...Array.from(changes.schemas, ([key, value]) => ({ kind: 'schemas' as const, key, label: schemaLabel(value), detail: schemaSql(value) })),
  ];
  const result = results[resultIndex];
  return <div className={styles.panel}>
    <nav><button className={tab === 'changes' ? styles.active : ''} onClick={() => setTab('changes')}>{zhCN.pendingChanges}</button><button className={tab === 'results' ? styles.active : ''} onClick={() => setTab('results')}>{zhCN.results}</button><button className={tab === 'messages' ? styles.active : ''} onClick={() => setTab('messages')}>{zhCN.messages}</button></nav>
    <div className={styles.body}>
      {tab === 'changes' && (entries.length ? <ul className={styles.changes}>{entries.map((entry) => <li key={`${entry.kind}-${entry.key}`}><strong>{entry.label}</strong><code title={entry.detail}><SqlText sql={entry.detail} /></code><IconButton compact icon={<Undo2 size={13} />} label={zhCN.undoChange} onClick={() => onUndoOne(entry.kind, entry.key)} /></li>)}</ul> : <p>{zhCN.noPendingChanges}</p>)}
      {tab === 'results' && (result ? <><select aria-label={zhCN.results} value={resultIndex} onChange={(event) => setResultIndex(Number(event.target.value))}>{results.map((item, index) => <option key={index} value={index}>{zhCN.resultLabel(index + 1, item.rows?.length ?? item.affected ?? 0, item.durationMs)}</option>)}</select>{result.type === 'rows' ? <div className={styles.resultScroll}><table><thead><tr>{result.columns?.map((column) => <th key={column}>{column}</th>)}</tr></thead><tbody>{result.rows?.map((row, rowIndex) => <tr key={rowIndex}>{row.map((value, columnIndex) => <td key={columnIndex}>{value === null ? zhCN.nullValue : String(value)}</td>)}</tr>)}</tbody></table></div> : <p>{result.message ?? zhCN.executionAffected(result.affected ?? 0, result.durationMs)}</p>}</> : <p>{zhCN.noResults}</p>)}
      {tab === 'messages' && <ol className={styles.messages}>{messages.map((message, index) => <li key={`${index}-${message}`}>{message}</li>)}</ol>}
    </div>
  </div>;
}
