import { useState } from 'react';
import { columnTypeLabels, zhCN } from '../../i18n/zh-CN';
import type { TableMeta } from '../../types';
import { Modal } from '../common/Modal';
import styles from './grid.module.css';

// 插入行对话框：按列类型校验并暂存待提交变更。
export function RowDialog({ table, onClose, onInsert }: { table: TableMeta; onClose: () => void; onInsert: (values: Record<string, unknown>) => void }) {
  const [draft, setDraft] = useState<Record<string, string>>(() => Object.fromEntries(table.columns.map((column) => [column.name, column.type === 'bool' ? 'false' : ''])));
  const [error, setError] = useState('');
  const submit = () => {
    const values: Record<string, unknown> = {};
    for (const column of table.columns) {
      const value = draft[column.name];
      if (column.type === 'int' && (!/^-?\d+$/.test(value) || !Number.isSafeInteger(Number(value)))) return setError(zhCN.invalidInteger(column.name));
      if (column.type === 'date' && !/^\d{4}-\d{2}-\d{2}$/.test(value)) return setError(zhCN.chooseDate(column.name));
      values[column.name] = column.type === 'int' ? Number(value) : column.type === 'bool' ? value === 'true' : value;
    }
    onInsert(values);
  };
  return <Modal title={zhCN.insertRowTitle(table.name)} onClose={onClose} footer={<><button onClick={onClose}>{zhCN.cancel}</button><button className={styles.primaryButton} onClick={submit}>{zhCN.addToPending}</button></>}>
    <p>{zhCN.insertRowHint}</p>
    <div className={styles.formFields}>{table.columns.map((column) => <label key={column.name}><span>{column.name} <small>{columnTypeLabels[column.type] ?? column.type}{column.primaryKey ? zhCN.primaryKeyBadge : ''}</small></span>{column.type === 'bool' ? <select aria-label={column.name} value={draft[column.name]} onChange={(event) => setDraft({ ...draft, [column.name]: event.target.value })}><option value="false">{zhCN.booleanFalse}</option><option value="true">{zhCN.booleanTrue}</option></select> : <input aria-label={column.name} type={column.type === 'date' ? 'date' : 'text'} inputMode={column.type === 'int' ? 'numeric' : undefined} value={draft[column.name]} onChange={(event) => setDraft({ ...draft, [column.name]: event.target.value })} />}</label>)}</div>
    {error && <p role="alert">{error}</p>}
  </Modal>;
}
