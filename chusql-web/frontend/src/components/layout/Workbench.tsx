import { Database, RotateCcw, Settings, TerminalSquare, Undo2 } from 'lucide-react';
import { useEffect, useState, type CSSProperties, type KeyboardEvent, type PointerEvent, type ReactNode } from 'react';
import { IconButton } from '../common/IconButton';
import { zhCN } from '../../i18n/zh-CN';
import styles from './layout.module.css';

// 工作台外壳：标题栏、活动栏与编辑器/底栏/状态栏布局，面板边界可拖动。
interface WorkbenchProps {
  explorer: ReactNode;
  editor: ReactNode;
  bottom: ReactNode;
  status: ReactNode;
  changeCount: number;
  canUndo: boolean;
  canRedo: boolean;
  onCommit: () => void;
  onRollback: () => void;
  onUndo: () => void;
  onRedo: () => void;
  onSql: () => void;
  onSettings: () => void;
}

type SplitterKind = 'sidebar' | 'dock';

interface LayoutSize { sidebar: number; dock: number }

interface DragState { kind: SplitterKind; start: number; size: number }

const STORAGE_KEY = 'chusql.layout.v1';
const SIDEBAR_MIN = 160;
const SIDEBAR_MAX = 560;
const DOCK_MIN = 96;
const KEY_STEP = 16;

// 底部船坞高度上限取窗口高度的七成
function dockMax() { return Math.max(DOCK_MIN, Math.round(window.innerHeight * 0.7)); }

// 把尺寸夹紧到允许区间并取整
function clampSize(kind: SplitterKind, value: number) {
  const rounded = Math.round(value);
  return kind === 'sidebar' ? Math.min(SIDEBAR_MAX, Math.max(SIDEBAR_MIN, rounded)) : Math.min(dockMax(), Math.max(DOCK_MIN, rounded));
}

// 读取本地存储的面板尺寸，异常时用默认值
function readLayout(): LayoutSize {
  const fallback = { sidebar: 250, dock: Math.round(window.innerHeight * 0.22) };
  try {
    const raw = JSON.parse(localStorage.getItem(STORAGE_KEY) ?? '') as { sidebarWidth?: unknown; dockHeight?: unknown };
    if (typeof raw.sidebarWidth !== 'number' || typeof raw.dockHeight !== 'number') return fallback;
    return { sidebar: clampSize('sidebar', raw.sidebarWidth), dock: clampSize('dock', raw.dockHeight) };
  } catch { return fallback; }
}

// 写入面板尺寸到本地存储，失败时静默忽略
function writeLayout(size: LayoutSize) {
  try { localStorage.setItem(STORAGE_KEY, JSON.stringify({ sidebarWidth: size.sidebar, dockHeight: size.dock })); } catch { return; }
}

export function Workbench(props: WorkbenchProps) {
  const [layout, setLayout] = useState<LayoutSize>(readLayout);
  const [drag, setDrag] = useState<DragState>();
  const { sidebar, dock } = layout;

  useEffect(() => { writeLayout(layout); }, [layout]);

  useEffect(() => {
    if (!drag) return;
    const move = (event: globalThis.PointerEvent) => setLayout((current) => {
      const delta = drag.kind === 'sidebar' ? event.clientX - drag.start : drag.start - event.clientY;
      const next = drag.size + delta;
      return drag.kind === 'sidebar' ? { ...current, sidebar: clampSize('sidebar', next) } : { ...current, dock: clampSize('dock', next) };
    });
    const stop = () => setDrag(undefined);
    window.addEventListener('pointermove', move);
    window.addEventListener('pointerup', stop);
    return () => { window.removeEventListener('pointermove', move); window.removeEventListener('pointerup', stop); };
  }, [drag]);

  const resize = (kind: SplitterKind, value: number) => setLayout((current) => (kind === 'sidebar' ? { ...current, sidebar: clampSize('sidebar', value) } : { ...current, dock: clampSize('dock', value) }));

  const startDrag = (kind: SplitterKind, event: PointerEvent<HTMLButtonElement>) => {
    event.preventDefault();
    setDrag({ kind, start: kind === 'sidebar' ? event.clientX : event.clientY, size: kind === 'sidebar' ? sidebar : dock });
  };

  const onSplitterKeyDown = (kind: SplitterKind, event: KeyboardEvent<HTMLButtonElement>) => {
    const step = kind === 'sidebar'
      ? (event.key === 'ArrowRight' ? KEY_STEP : event.key === 'ArrowLeft' ? -KEY_STEP : 0)
      : (event.key === 'ArrowUp' ? KEY_STEP : event.key === 'ArrowDown' ? -KEY_STEP : 0);
    if (!step) return;
    event.preventDefault();
    resize(kind, (kind === 'sidebar' ? sidebar : dock) + step);
  };

  return (
    <div className={styles.workbench} style={{ '--sidebar-width': `${sidebar}px`, '--dock-height': `${dock}px` } as CSSProperties}>
      <header className={styles.titlebar}>
        <span className={styles.brand}><Database size={15} />{zhCN.connection}</span>
        <nav>
          <IconButton icon={<Database size={14} />} label={zhCN.commit(props.changeCount)} disabled={!props.changeCount} onClick={props.onCommit} />
          <IconButton icon={<RotateCcw size={14} />} label={zhCN.rollback} disabled={!props.changeCount} onClick={props.onRollback} />
          <IconButton icon={<Undo2 size={14} />} label={zhCN.undo} disabled={!props.canUndo} onClick={props.onUndo} />
          <IconButton icon={<Undo2 size={14} transform="rotate(180)" />} label={zhCN.redo} disabled={!props.canRedo} onClick={props.onRedo} />
          <IconButton icon={<Settings size={14} />} label={zhCN.settings} onClick={props.onSettings} />
        </nav>
      </header>
      <div className={styles.activitybar} aria-label={zhCN.activityBar}><button aria-label={zhCN.databaseSection}><Database size={20} /></button><button aria-label={zhCN.sqlConsole} onClick={props.onSql}><TerminalSquare size={20} /></button><button aria-label={zhCN.settings} onClick={props.onSettings}><Settings size={20} /></button><GitMark /></div>
      {props.explorer}
      <main className={styles.editor}>{props.editor}</main>
      <section className={styles.bottom}>{props.bottom}</section>
      <footer className={styles.statusbar}>{props.status}</footer>
      <button type="button" className={`${styles.splitterV}${drag?.kind === 'sidebar' ? ` ${styles.dragging}` : ''}`} role="separator" aria-orientation="vertical" aria-label={zhCN.resizeSidebar} aria-valuenow={sidebar} aria-valuemin={SIDEBAR_MIN} aria-valuemax={SIDEBAR_MAX} tabIndex={0} onPointerDown={(event) => startDrag('sidebar', event)} onKeyDown={(event) => onSplitterKeyDown('sidebar', event)} />
      <button type="button" className={`${styles.splitterH}${drag?.kind === 'dock' ? ` ${styles.dragging}` : ''}`} role="separator" aria-orientation="horizontal" aria-label={zhCN.resizeDock} aria-valuenow={dock} aria-valuemin={DOCK_MIN} aria-valuemax={dockMax()} tabIndex={0} onPointerDown={(event) => startDrag('dock', event)} onKeyDown={(event) => onSplitterKeyDown('dock', event)} />
    </div>
  );
}

function GitMark() {
  return <span aria-hidden="true">⌘</span>;
}
