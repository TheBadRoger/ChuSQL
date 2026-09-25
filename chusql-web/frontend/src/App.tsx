import { lazy, Suspense, useEffect, useMemo, useRef, useState } from 'react';
import { MockAdapter } from './adapters/MockAdapter';
import { IconButton } from './components/common/IconButton';
import { TableEditor } from './components/grid/TableEditor';
import { BottomPanel } from './components/layout/BottomPanel';
import { Explorer } from './components/layout/Explorer';
import { Workbench } from './components/layout/Workbench';
import { SchemaDialog, type SchemaAction } from './components/layout/SchemaDialog';
import { SettingsPanel } from './components/settings/SettingsPanel';
import type { QueryHistoryEntry } from './components/sql/SqlConsole';
import { changeCount, toCommitPayload } from './core/changes';
import { closeTab, openQueryTab, openSettingsTab, openTableTab, type Tab, type TabState } from './core/tabs';
import { syncUiSettings } from './core/uiSettings';
import { zhCN } from './i18n/zh-CN';
import { useChangeStore } from './store/changeStore';
import { fontFamilyValue, useSettingsStore } from './store/settingsStore';
import type { DataSourceAdapter, SchemaChange, StatementResult, TableMeta } from './types';
import styles from './App.module.css';

// 应用根组件：装配数据源、多标签编辑区、变更提交与设置视图。

interface AppProps { adapter?: DataSourceAdapter }
const SqlConsole = lazy(async () => ({ default: (await import('./components/sql/SqlConsole')).SqlConsole }));

function initialHistory(): QueryHistoryEntry[] {
  localStorage.removeItem('chusql.sql.history.v1');
  try { const value = JSON.parse(localStorage.getItem('chusql.sql.history.v2') ?? '[]'); return Array.isArray(value) ? value : []; }
  catch { return []; }
}

export function App({ adapter: providedAdapter }: AppProps) {
  const adapter = useMemo(() => providedAdapter ?? new MockAdapter(), [providedAdapter]);
  const [tables, setTables] = useState<TableMeta[]>([]);
  const [tabs, setTabs] = useState<Tab[]>([]);
  const [activeId, setActiveId] = useState<string>();
  const [totals, setTotals] = useState<Record<string, number>>({});
  const [messages, setMessages] = useState<string[]>([]);
  const [results, setResults] = useState<StatementResult[]>([]);
  const [history, setHistory] = useState<QueryHistoryEntry[]>(initialHistory);
  const [revision, setRevision] = useState(0);
  const [committing, setCommitting] = useState(false);
  const [schemaAction, setSchemaAction] = useState<SchemaAction>();
  const idRef = useRef(0);
  const queryRef = useRef(0);
  const { changes, undoStack, redoStack, undoOne, undo, redo, clear, acknowledge } = useChangeStore();
  const settings = useSettingsStore((state) => state.settings);
  const count = changeCount(changes);
  const activeTab = tabs.find((tab) => tab.id === activeId);
  const activeTableTab = activeTab?.kind === 'table' ? activeTab : undefined;
  const activeMeta = activeTableTab ? tables.find((table) => table.name === activeTableTab.table) : undefined;
  const pushMessage = (message: string) => setMessages((current) => [...current, message].slice(-100));
  const nextTabId = () => `tab-${(idRef.current += 1)}`;
  const applyTabs = (state: TabState) => { setTabs(state.tabs); setActiveId(state.activeId); };
  const forgetTotal = (id: string) => setTotals((current) => { const next = { ...current }; delete next[id]; return next; });

  const loadTables = async () => {
    try { setTables(await adapter.listTables(zhCN.database)); }
    catch (reason) { pushMessage(`${zhCN.loadError}: ${reason instanceof Error ? reason.message : String(reason)}`); }
  };
  useEffect(() => { void loadTables(); }, [adapter]);
  useEffect(() => syncUiSettings(), []);
  useEffect(() => {
    const root = document.documentElement;
    root.style.setProperty('--ui-font', fontFamilyValue(settings.uiFonts));
    root.style.setProperty('--grid-font', fontFamilyValue(settings.gridFonts));
    root.style.setProperty('--grid-font-size', `${settings.gridFontSize}px`);
    root.style.setProperty('--grid-row-height', `${settings.gridRowHeight}px`);
    root.style.fontSize = `${settings.uiFontSize}px`;
  }, [settings]);
  useEffect(() => {
    const warn = (event: BeforeUnloadEvent) => { if (count) event.preventDefault(); };
    window.addEventListener('beforeunload', warn);
    return () => window.removeEventListener('beforeunload', warn);
  }, [count]);

  const rollback = () => { clear(); setRevision((value) => value + 1); };
  const applySchema = async (change: SchemaChange) => {
    if (change.op === 'createTable') await adapter.createTable(change.table, change.columns);
    else if (change.op === 'dropTable') await adapter.dropTable(change.table);
    else if (change.op === 'createIndex') await adapter.createIndex(change.table, change.column);
    else await adapter.dropIndex(change.table, change.column);
  };
  const commit = async () => {
    if (committing) return;
    setCommitting(true);
    try {
      const rows = toCommitPayload(changes);
      const schemas = Array.from(changes.schemas.values());
      const creates = schemas.filter((change) => change.op === 'createTable' || change.op === 'createIndex');
      const drops = schemas.filter((change) => change.op === 'dropIndex' || change.op === 'dropTable');
      let applied = 0;
      let failure = '';
      try {
        for (const change of creates) { await applySchema(change); applied += 1; }
        if (rows.length) {
          const result = await adapter.commitChanges({ db: zhCN.database, changes: rows });
          applied += result.ok ? rows.length : result.applied;
          if (!result.ok) failure = result.errors?.map((item) => item.message).join('; ') ?? '';
        }
        if (!failure) for (const change of drops) { await applySchema(change); applied += 1; }
      } catch (reason) { failure = reason instanceof Error ? reason.message : String(reason); }
      if (applied) acknowledge(changes, applied);
      if (failure) pushMessage(`${zhCN.commitFailed} (${applied} applied; retry submits only the remainder): ${failure}`);
      else pushMessage(zhCN.committed(applied));
      await loadTables(); setRevision((value) => value + 1);
    } finally { setCommitting(false); }
  };
  const requestCommit = () => { if (count) void commit(); };
  const openTable = (name: string) => applyTabs(openTableTab(tabs, name, nextTabId()));
  const newQuery = (sql?: string) => applyTabs(openQueryTab(tabs, zhCN.queryTab((queryRef.current += 1)), nextTabId(), sql));
  const openSettings = () => applyTabs(openSettingsTab(tabs, zhCN.settings, nextTabId()));
  const closeTabById = (tab: Tab) => {
    forgetTotal(tab.id);
    applyTabs(closeTab(tabs, tab.id, activeId));
  };
  let editor = <div className={styles.welcome}>{zhCN.emptyWorkspace}</div>;
  if (tabs.length) {
    let body = <div className={styles.welcome}>{zhCN.emptyWorkspace}</div>;
    if (activeTab?.kind === 'sql') body = <Suspense fallback={<div className={styles.notice}>{zhCN.loading}</div>}><SqlConsole key={activeTab.id} adapter={adapter} settings={settings} initialSql={activeTab.sql} onResult={(result) => setResults((current) => [...current, result])} onMessage={pushMessage} onHistory={setHistory} /></Suspense>;
    else if (activeTab?.kind === 'settings') body = <SettingsPanel />;
    else if (activeTableTab && activeMeta) body = <TableEditor key={activeTableTab.id} adapter={adapter} table={activeMeta} settings={settings} revision={revision} onTotal={(value) => setTotals((current) => ({ ...current, [activeTableTab.id]: value }))} onMessage={pushMessage} />;
    editor = <section className={styles.editorPane}><div className={styles.tabs} role="tablist">{tabs.map((tab) => <div key={tab.id} className={tab.id === activeId ? styles.tabActive : styles.tab}><button role="tab" aria-selected={tab.id === activeId} className={styles.tabLabel} onClick={() => setActiveId(tab.id)}>{tab.title}</button><IconButton compact icon={<svg width="12" height="12" viewBox="0 0 16 16" fill="none" stroke="currentColor" strokeWidth="1.6" aria-hidden="true"><path d="m4 4 8 8m0-8-8 8" /></svg>} label={zhCN.closeTab(tab.title)} onClick={() => closeTabById(tab)} /></div>)}</div>{body}</section>;
  }

  return <><Workbench explorer={<Explorer tables={tables} activeTable={activeMeta?.name} history={history} onOpenTable={openTable} onOpenSql={newQuery} onCreateTable={() => setSchemaAction({ kind: 'create' })} onIndexes={(table) => setSchemaAction({ kind: 'indexes', table })} onDropTable={(table) => setSchemaAction({ kind: 'drop', table })} />} editor={editor} bottom={<BottomPanel changes={changes} results={results} messages={messages} onUndoOne={undoOne} />} status={<><span>{zhCN.connection}</span><span>{zhCN.database}</span>{activeMeta && <span>{activeMeta.name}</span>}{activeTableTab && activeMeta && <span>{zhCN.rowCount(totals[activeTableTab.id] ?? 0)}</span>}<span>{zhCN.pendingCount(count)}</span></>} changeCount={count} canUndo={undoStack.length > 0} canRedo={redoStack.length > 0} onCommit={requestCommit} onRollback={rollback} onUndo={undo} onRedo={redo} onSql={() => newQuery()} onSettings={openSettings} />{schemaAction && <SchemaDialog action={schemaAction} onClose={() => setSchemaAction(undefined)} onChanged={pushMessage} />}</>;
}
