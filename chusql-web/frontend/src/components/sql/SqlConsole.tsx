import { Play, PlayCircle, Square, TextSelect } from 'lucide-react';
import * as monaco from 'monaco-editor/esm/vs/editor/editor.api';
import EditorWorker from 'monaco-editor/esm/vs/editor/editor.worker?worker';
import 'monaco-editor/min/vs/editor/editor.main.css';
import 'monaco-editor/esm/vs/basic-languages/sql/sql.contribution';
import { useEffect, useMemo, useRef, useState } from 'react';
import { splitSqlStatements, type SqlStatement } from '../../core/sql';
import { SQL_TOKEN_COLORS } from '../../core/sqlHighlight';
import { zhCN } from '../../i18n/zh-CN';
import type { DataSourceAdapter, StatementResult } from '../../types';
import type { IdeSettings } from '../../store/settingsStore';
import { IconButton } from '../common/IconButton';
import styles from './sql.module.css';
import { restoreEditorLayout } from './editorLayout';
import { fontFamilyValue } from '../../store/settingsStore';

// SQL 控制台：Monaco 编辑器、语句切分与逐条执行。

(self as unknown as { MonacoEnvironment: { getWorker: () => Worker } }).MonacoEnvironment = { getWorker: () => new EditorWorker() };

export interface QueryHistoryEntry { sql: string; at: string; ok: boolean; durationMs: number }

interface SqlConsoleProps {
  adapter: DataSourceAdapter;
  settings: IdeSettings;
  initialSql?: string;
  onResult: (result: StatementResult) => void;
  onMessage: (message: string) => void;
  onHistory: (history: QueryHistoryEntry[]) => void;
}

type RunStatus = { state: 'pending' | 'running' | 'success' | 'error'; durationMs?: number };

function readHistory(): QueryHistoryEntry[] {
  try {
    const parsed = JSON.parse(localStorage.getItem('chusql.sql.history.v2') ?? '[]');
    return Array.isArray(parsed) ? parsed.slice(0, 50) : [];
  } catch { return []; }
}

export function SqlConsole({ adapter, settings, initialSql = 'SELECT * FROM users;', onResult, onMessage, onHistory }: SqlConsoleProps) {
  const host = useRef<HTMLDivElement>(null);
  const editor = useRef<monaco.editor.IStandaloneCodeEditor>();
  const stopRequested = useRef(false);
  const [sql, setSql] = useState(initialSql);
  const [running, setRunning] = useState(false);
  const [statuses, setStatuses] = useState<Record<string, RunStatus>>({});
  const statements = useMemo(() => splitSqlStatements(sql), [sql]);

  useEffect(() => {
    if (!host.current) return undefined;
    const stopLayoutRestore = restoreEditorLayout(host.current);
    monaco.editor.defineTheme('chusql-dark', {
      base: 'vs-dark', inherit: true,
      rules: [
        { token: 'keyword', foreground: SQL_TOKEN_COLORS.keyword.slice(1) },
        { token: 'string', foreground: SQL_TOKEN_COLORS.string.slice(1) },
        { token: 'number', foreground: SQL_TOKEN_COLORS.number.slice(1) },
        { token: 'comment', foreground: SQL_TOKEN_COLORS.comment.slice(1) },
      ],
      colors: { 'editor.background': '#1e1e1e', 'editorLineNumber.foreground': '#858585', 'editor.selectionBackground': '#264f78' },
    });
    const instance = monaco.editor.create(host.current, {
      value: sql, language: 'sql', theme: 'chusql-dark', glyphMargin: true,
      automaticLayout: true, minimap: { enabled: settings.minimap }, fontFamily: fontFamilyValue(settings.sqlFonts),
      fontSize: settings.sqlFontSize, lineHeight: settings.sqlLineHeight, tabSize: settings.sqlTabSize,
      lineNumbers: settings.sqlLineNumbers ? 'on' : 'off', quickSuggestions: settings.autocomplete,
    });
    editor.current = instance;
    const subscription = instance.onDidChangeModelContent(() => setSql(instance.getValue()));
    return () => { stopLayoutRestore(); subscription.dispose(); instance.dispose(); editor.current = undefined; };
  }, []);

  useEffect(() => { editor.current?.updateOptions({ minimap: { enabled: settings.minimap }, fontFamily: fontFamilyValue(settings.sqlFonts), fontSize: settings.sqlFontSize, lineHeight: settings.sqlLineHeight, tabSize: settings.sqlTabSize, lineNumbers: settings.sqlLineNumbers ? 'on' : 'off', quickSuggestions: settings.autocomplete }); }, [settings]);
  useEffect(() => { setStatuses(Object.fromEntries(statements.map((statement) => [statement.id, { state: 'pending' }]))); }, [sql]);
  useEffect(() => {
    const instance = editor.current;
    if (!instance) return;
    const decorations = statements.map((statement) => {
      const status = statuses[statement.id]?.state ?? 'pending';
      return { range: new monaco.Range(statement.startLine, 1, statement.endLine, 1), options: { isWholeLine: status === 'running', className: status === 'running' ? styles.runningLine : undefined, linesDecorationsClassName: status === 'running' ? styles.runningEdge : undefined, glyphMarginClassName: styles[`glyph${status[0].toUpperCase()}${status.slice(1)}`] } };
    });
    const ids = instance.deltaDecorations([], decorations);
    return () => { instance.deltaDecorations(ids, []); };
  }, [statements, statuses]);

  const recordHistory = (statement: SqlStatement, result: StatementResult) => {
    const next = [{ sql: statement.sql, at: new Date().toISOString(), ok: result.type !== 'error', durationMs: result.durationMs }, ...readHistory()].slice(0, 50);
    localStorage.setItem('chusql.sql.history.v2', JSON.stringify(next)); onHistory(next);
  };
  const run = async (items: SqlStatement[]) => {
    if (!items.length) return onMessage(zhCN.noStatement);
    stopRequested.current = false; setRunning(true);
    for (const statement of items) {
      if (stopRequested.current) break;
      setStatuses((current) => ({ ...current, [statement.id]: { state: 'running' } }));
      const result = await adapter.execStatement({ db: zhCN.database, sql: statement.sql });
      setStatuses((current) => ({ ...current, [statement.id]: { state: result.type === 'error' ? 'error' : 'success', durationMs: result.durationMs } }));
      recordHistory(statement, result); onResult(result);
      if (result.type === 'error') onMessage(result.message ?? zhCN.statementFailed);
      else if (result.type === 'rows') onMessage(zhCN.executionSuccess(result.rows?.length ?? 0, result.durationMs));
      else onMessage(zhCN.executionAffected(result.affected ?? 0, result.durationMs));
    }
    if (stopRequested.current) onMessage(zhCN.executionStopped);
    setRunning(false);
  };
  const pending = () => statements.find((statement) => statuses[statement.id]?.state !== 'success');
  const selection = () => {
    const instance = editor.current; const range = instance?.getSelection();
    if (!instance || !range || range.isEmpty()) return [];
    return splitSqlStatements(instance.getModel()?.getValueInRange(range) ?? '');
  };

  return <section className={styles.console}>
    <div className={styles.toolbar}><IconButton icon={<PlayCircle size={14} />} label={zhCN.runNext} disabled={running} onClick={() => { const next = pending(); void run(next ? [next] : []); }} /><IconButton icon={<Play size={14} />} label={zhCN.runAll} disabled={running} onClick={() => void run(statements)} /><IconButton icon={<TextSelect size={14} />} label={zhCN.runSelection} disabled={running} onClick={() => void run(selection())} /><IconButton icon={<Square size={13} />} label={zhCN.stop} disabled={!running} onClick={() => { stopRequested.current = true; }} /></div>
    <div className={styles.editor} ref={host} aria-label={zhCN.sqlConsole} />
    <ol className={styles.statementList}>{statements.map((statement) => { const status = statuses[statement.id] ?? { state: 'pending' }; return <li key={statement.id}><span>{status.state === 'pending' ? '○' : status.state === 'running' ? '▶' : status.state === 'success' ? '✓' : '✗'}</span><span>{status.state === 'pending' ? zhCN.statementPending : status.state === 'running' ? zhCN.statementRunning : status.state === 'success' ? zhCN.statementSuccess : zhCN.statementFailed}</span>{status.durationMs !== undefined && <small>{status.durationMs.toFixed(1)} ms</small>}</li>; })}</ol>
  </section>;
}
