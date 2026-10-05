import * as api from './api.js';
import { AUTH_EVENT } from './api.js';
import {
  DEFAULT_SETTINGS,
  IDE_SETTINGS_KEY,
  accountsTable,
  cellText,
  changeCount,
  changeEntries,
  changeKey,
  changesSql,
  clampSetting,
  closeTab,
  createEmptyChangeSet,
  creatableTypes,
  deleteKey,
  editText,
  escapeHtml,
  fontFamilyValue,
  fontsFromText,
  fontsToText,
  highlightHtml,
  insertKey,
  matchesFilter,
  openQueryTab,
  openSettingsTab,
  openTableTab,
  parseEdited,
  parseUiSettings,
  settingLabels,
  toCommitPayload,
  typeLabel,
} from './core.js';

// ChuSQL Web IDE 主逻辑：状态、渲染、事件代理与会话/数据/变更动作。

// ---------------------------------------------------------------- 状态

const state = {
  view: 'loading', // loading | login | workbench
  session: null,
  databases: [],
  database: '',
  tables: [],
  expanded: new Set(),
  tabs: [],
  activeId: undefined,
  changes: createEmptyChangeSet(),
  edit: null,
  editStarting: null,
  editAttempt: null,
  editBusy: false,
  editNeedsReload: false,
  editUncertain: null,
  insertSeq: 0,
  messages: [],
  tabSeq: 0,
  settings: { ...DEFAULT_SETTINGS },
  serverSettings: null,
  bottomTab: 'changes',
  status: '',
  loading: false,
  sidebarWidth: 250,
  dockHeight: '22vh',
};

const root = document.getElementById('root');

// ---------------------------------------------------------------- 小工具

// 按 id 取 DOM 元素。
function region(id) {
  return document.getElementById(id);
}

// 当前时间（时分秒）。
function now() {
  return new Date().toLocaleTimeString('zh-CN', { hour12: false });
}

// 记一条消息并刷新下方面板。
function log(level, text) {
  state.messages.push({ level, text, time: now() });
  if (state.messages.length > 200) state.messages.shift();
  renderBottom();
}

// 设置状态栏文案。
function setStatus(text) {
  state.status = text;
  const el = region('status-message');
  if (el) el.textContent = text;
}

// 取错误的文字描述。
function describe(error) {
  return error instanceof Error ? error.message : String(error);
}

// 记录错误并写到状态栏。
function fail(error) {
  const message = describe(error);
  log('error', message);
  setStatus(message);
  return message;
}

// 生成下一个标签 id。
function nextTabId(prefix) {
  state.tabSeq += 1;
  return `${prefix}-${state.tabSeq}`;
}

// 当前激活的标签。
function activeTab() {
  return state.tabs.find((tab) => tab.id === state.activeId);
}

// ---------------------------------------------------------------- 登录页

// 渲染登录页。
function renderLogin(message, level) {
  state.view = 'login';
  root.innerHTML = `
    <div class="auth-view">
      <form class="auth-card" id="login-form">
        <header><span class="brand-mark">ChuSQL</span><h1>数据库终端</h1></header>
        <label><span>账号</span><input id="login-user" name="user" autocomplete="username" autofocus /></label>
        <label><span>口令</span><input id="login-password" name="password" type="password" autocomplete="current-password" /></label>
        <p class="auth-hint">配置里口令留空时，管理员直接留空口令登录。</p>
        ${message ? `<p class="alert alert-${level ?? 'error'}">${escapeHtml(message)}</p>` : ''}
        <button type="submit" class="primary"${state.loading ? ' disabled' : ''}>${state.loading ? '登录中…' : '登录'}</button>
      </form>
    </div>`;
}

// ---------------------------------------------------------------- 页面骨架

// 渲染工作台骨架与对话框。
function renderShell() {
  state.view = 'workbench';
  root.innerHTML = `
    <div class="workbench" id="workbench">
      <header class="titlebar">
        <div class="brand"><span class="brand-mark">ChuSQL</span><span class="brand-name">数据库终端</span></div>
        <nav class="titlebar-actions">
          <button data-action="new-query" title="新建查询">新建查询</button>
          <button data-action="demo-data" title="灌一份演示数据">演示数据</button>
          <button data-action="refresh" title="重新加载当前库">刷新</button>
          <button data-action="settings" title="IDE 与服务器设置">设置</button>
        </nav>
        <div class="account" id="account"></div>
      </header>
      <div class="activitybar">
        <button data-action="focus-explorer" title="资源管理器">▤</button>
        <button data-action="settings" title="设置">⚙</button>
      </div>
      <aside class="sidebar" id="sidebar"></aside>
      <main class="editor" id="editor"></main>
      <section class="bottom" id="bottom"></section>
      <footer class="statusbar">
        <span id="status-database"></span>
        <span id="status-message"></span>
        <span class="flex-spacer"></span>
        <span id="status-changes"></span>
      </footer>
      <button class="splitter-v" data-split="v" title="调整侧栏宽度"></button>
      <button class="splitter-h" data-split="h" title="调整下方面板高度"></button>
    </div>

    <dialog id="dlg-db" class="dialog">
      <div class="dialog-body">
        <header class="dialog-header"><h2>新建数据库</h2></header>
        <div class="dialog-content">
          <label class="field"><span>数据库名</span><input id="dlg-db-name" placeholder="字母或下划线开头" /></label>
          <p class="alert" id="dlg-db-error"></p>
        </div>
        <footer class="dialog-footer">
          <button type="button" data-action="close-dialog">取消</button>
          <button type="button" class="primary" data-action="create-database">创建</button>
        </footer>
      </div>
    </dialog>

    <dialog id="dlg-table" class="dialog">
      <div class="dialog-body">
        <header class="dialog-header"><h2>新建表</h2></header>
        <div class="dialog-content">
          <label class="field"><span>表名</span><input id="dlg-table-name" placeholder="字母或下划线开头" /></label>
          <div class="field"><span>列</span><div id="dlg-table-columns" class="column-rows"></div></div>
          <button type="button" class="plain" data-action="add-column">＋ 增加一列</button>
          <p class="alert" id="dlg-table-error"></p>
        </div>
        <footer class="dialog-footer">
          <button type="button" data-action="close-dialog">取消</button>
          <button type="button" class="primary" data-action="create-table">创建</button>
        </footer>
      </div>
    </dialog>

    <dialog id="dlg-index" class="dialog">
      <div class="dialog-body">
        <header class="dialog-header"><h2>建索引</h2></header>
        <div class="dialog-content">
          <label class="field"><span>列</span><select id="dlg-index-column"></select></label>
          <p class="alert" id="dlg-index-error"></p>
        </div>
        <footer class="dialog-footer">
          <button type="button" data-action="close-dialog">取消</button>
          <button type="button" class="primary" data-action="create-index">创建</button>
        </footer>
      </div>
    </dialog>`;
  renderAccount();
  applyLayout();
}

// 应用侧栏宽度与下方面板高度。
function applyLayout() {
  const workbench = region('workbench');
  if (!workbench) return;
  workbench.style.setProperty('--sidebar-width', `${state.sidebarWidth}px`);
  workbench.style.setProperty('--dock-height', state.dockHeight);
}

// ---------------------------------------------------------------- 账号区

// 渲染右上角账号区。
function renderAccount() {
  const el = region('account');
  if (!el) return;
  if (!state.session) {
    el.innerHTML = '';
    return;
  }
  const title = state.session.administrator ? '管理员' : '普通账号';
  const badge = state.session.administrator ? ' · 管理员' : '';
  el.innerHTML = `<span class="account-name" title="${title}">${escapeHtml(state.session.user)}${badge}</span>
    <button data-action="logout">退出</button>`;
}

// ---------------------------------------------------------------- 侧栏

// 渲染资源管理器侧栏。
function renderSidebar() {
  const el = region('sidebar');
  if (!el) return;
  const options = state.databases
    .map((name) => `<option value="${escapeHtml(name)}"${name === state.database ? ' selected' : ''}>${escapeHtml(name)}</option>`)
    .join('');
  const databaseOptions = state.database ? options : `<option value="" selected>（未选择）</option>${options}`;

  const rows = state.tables.map((table) => {
    const open = state.expanded.has(table.name);
    const hasId = table.columns.some((column) => column.name === 'id');
    const columns = table.columns.map((column) => {
      const marks = [
        column.primaryKey || column.name === 'id' ? '<span class="mark-pk" title="主键">PK</span>' : '',
        column.indexed ? '<span class="mark-ix" title="有索引">IX</span>' : '',
        column.nullable ? '' : '<span class="mark-nn" title="非空">NN</span>',
      ].join('');
      const drop = column.name === 'id'
        ? ''
        : `<button class="mini danger" data-action="drop-column" data-table="${escapeHtml(table.name)}" data-column="${escapeHtml(column.name)}" title="删除列">✕</button>`;
      return `<li><span class="col-name">${escapeHtml(column.name)}</span>
        <span class="col-type">${escapeHtml(typeLabel(column.type))}</span>
        <span class="col-marks">${marks}</span>${drop}</li>`;
    }).join('');

    const own = (table.indexes ?? []).filter((index) => !index.builtIn && index.column !== 'id');
    const indexList = own.length
      ? `<ul class="index-list">${own.map((index) => `<li><span>索引 ${escapeHtml(index.column)}</span>
          <button class="mini danger" data-action="drop-index" data-table="${escapeHtml(table.name)}" data-column="${escapeHtml(index.column)}" title="删除索引">✕</button></li>`).join('')}</ul>`
      : '';

    return `<li class="tree-table">
      <div class="tree-row">
        <button class="mini toggle" data-action="expand-table" data-table="${escapeHtml(table.name)}" title="${open ? '收起' : '展开'}">${open ? '▾' : '▸'}</button>
        <button class="tree-label" data-action="open-table" data-table="${escapeHtml(table.name)}" title="${escapeHtml(table.name)}">${escapeHtml(table.name)}</button>
        <small>${table.rowCount}</small>
        ${hasId ? '' : '<small class="warn" title="没有 id 列，无法编辑或删除">只读</small>'}
        <button class="mini danger" data-action="drop-table" data-table="${escapeHtml(table.name)}" title="删除表">✕</button>
      </div>
      ${open ? `<ul class="column-list">${columns}</ul>${indexList}
        <div class="tree-actions"><button class="mini" data-action="open-index-dialog" data-table="${escapeHtml(table.name)}">＋ 索引</button></div>` : ''}
    </li>`;
  }).join('');

  const count = changeCount(state.changes);
  el.innerHTML = `
    <h2>资源管理器</h2>
    <section class="side-section">
      <div class="side-title">数据库</div>
      <div class="side-row">
        <select id="database-select" data-action="switch-database">${databaseOptions}</select>
        <button class="mini" data-action="open-db-dialog" title="新建数据库">＋</button>
        <button class="mini danger" data-action="drop-database" title="删除当前数据库">✕</button>
      </div>
    </section>
    <section class="side-section">
      <div class="side-title">表<span class="side-actions">
        <button class="mini" data-action="open-table-dialog" title="新建表">＋</button>
        <button class="mini" data-action="refresh" title="刷新">⟳</button>
      </span></div>
      ${state.tables.length ? `<ul class="table-list">${rows}</ul>` : `<p class="muted">${state.database ? '这个库还没有表。' : '未选择数据库。'}</p>`}
    </section>
    <section class="side-section">
      <div class="side-title">变更</div>
      <div class="side-row">
        <button data-action="commit"${count && !state.editBusy && !state.editNeedsReload ? '' : ' disabled'}>提交 ${count}</button>
        <button data-action="rollback"${(state.edit || state.editAttempt) && !state.editBusy ? '' : ' disabled'}>撤销</button>
      </div>
    </section>`;
}

// ---------------------------------------------------------------- 标签栏

// 渲染标签栏并刷新内容区。
function renderTabs() {
  const el = region('editor');
  if (!el) return;
  const tabs = state.tabs.map((tab) => `
    <span class="tab${tab.id === state.activeId ? ' tab-active' : ''}">
      <button class="tab-label" data-action="activate-tab" data-tab="${tab.id}" title="${escapeHtml(tab.title)}">${escapeHtml(tab.title)}</button>
      <button class="mini" data-action="close-tab" data-tab="${tab.id}" title="关闭">✕</button>
    </span>`).join('');
  el.innerHTML = `
    <div class="tabbar">${tabs || '<span class="muted tab-empty">没有打开的标签页</span>'}</div>
    <div class="tab-content" id="tab-content"></div>`;
  renderEditor();
}

// ---------------------------------------------------------------- 编辑器区

// 渲染当前标签的内容。
function renderEditor() {
  const el = region('tab-content');
  if (!el) return;
  const tab = activeTab();
  if (!tab) {
    el.innerHTML = `<div class="welcome"><div>
      <h1>ChuSQL 数据库终端</h1>
      <p>从左边选一张表，或者 <button class="link" data-action="new-query">新建查询</button>。</p>
    </div></div>`;
    return;
  }
  if (tab.kind === 'table') renderTableTab(el, tab);
  else if (tab.kind === 'sql') renderSqlTab(el, tab);
  else renderSettingsTab(el, tab);
}

// ---- 表格标签

// 单元格显示内容的 HTML。
function cellContent(value, nullText) {
  if (value === null || value === undefined) return `<span class="null-value">${escapeHtml(nullText)}</span>`;
  return escapeHtml(cellText(value, nullText));
}

// 渲染表格标签（工具栏、表头、数据行）。
function renderTableTab(el, tab) {
  const data = tab.data;
  if (!data) {
    el.innerHTML = '<p class="notice">正在加载…</p>';
    return;
  }
  if (data.error) {
    el.innerHTML = `<p class="notice alert alert-error">${escapeHtml(data.error)}</p>`;
    return;
  }
  const columns = data.columns;
  const keyColumn = columns.find((column) => column.name === 'id');
  const editable = Boolean(keyColumn) && tab.table !== accountsTable && Boolean(state.edit)
    && data.editId === state.edit.id && !data.loading
    && !state.editBusy && !state.editNeedsReload && !state.editUncertain;
  const filters = data.filters ?? {};
  const count = changeCount(state.changes);

  const head = columns.map((column) => {
    const arrow = data.sort === column.name ? (data.dir === 'desc' ? ' ▼' : ' ▲') : '';
    return `<th><button class="column-head" data-action="sort-column" data-column="${escapeHtml(column.name)}" title="排序">${escapeHtml(column.name)}${arrow}</button>
      <small>${escapeHtml(typeLabel(column.type))}</small></th>`;
  }).join('');

  const filterRow = columns.map((column) => `<th><input class="filter-input" data-action="filter-column" data-column="${escapeHtml(column.name)}" value="${escapeHtml(filters[column.name] ?? '')}" placeholder="筛选" /></th>`).join('');

  const stagedInserts = [...state.changes.inserts.values()].filter((insert) => insert.table === tab.table && insert.database === tab.database);

  const insertRows = stagedInserts.map((insert) => {
    const cells = columns.map((column) => `<td class="inserted" data-action="edit-insert" data-temp="${escapeHtml(insert.tempId)}" data-column="${escapeHtml(column.name)}">${cellContent(insert.values[column.name], state.settings.nullText)}</td>`).join('');
    return `<tr class="row-inserted">
      <td class="row-number">＋</td>
      <td class="select-cell"><button class="mini danger" data-action="unstage-insert" data-temp="${escapeHtml(insert.tempId)}" title="取消这一行">✕</button></td>
      ${cells}</tr>`;
  }).join('');

  const body = data.rows.map((row, index) => {
    const pk = keyColumn ? row[keyColumn.name] : undefined;
    const removed = keyColumn ? state.changes.deletes.has(deleteKey(tab.table, pk)) : false;
    const selected = tab.selected?.has(String(pk)) === true;
    const cells = columns.map((column) => {
      const staged = keyColumn ? state.changes.updates.get(changeKey(tab.table, pk, column.name)) : undefined;
      const value = staged ? staged.newValue : row[column.name];
      const action = editable ? ` data-action="edit-cell" data-pk="${escapeHtml(pk)}" data-column="${escapeHtml(column.name)}"` : '';
      return `<td class="${staged ? 'dirty' : ''}"${action}>${cellContent(value, state.settings.nullText)}</td>`;
    }).join('');
    const checkbox = editable
      ? `<td class="select-cell"><input type="checkbox" data-action="select-row" data-pk="${escapeHtml(pk)}"${selected ? ' checked' : ''} /></td>`
      : '<td class="select-cell"></td>';
    return `<tr class="${removed ? 'row-deleted' : ''}">
      <td class="row-number">${data.offset + index + 1}</td>
      ${checkbox}
      ${cells}</tr>`;
  }).join('');

  const pageCount = Math.max(1, Math.ceil(data.total / data.pageSize));

  el.innerHTML = `
    <div class="table-editor">
      <div class="toolbar">
        <button data-action="insert-row"${editable ? '' : ' disabled'}>＋ 新增行</button>
        <button data-action="delete-selected"${editable && (tab.selected?.size ?? 0) ? '' : ' disabled'}>删除选中</button>
        <span class="sep"></span>
        <button class="primary" data-action="commit"${count && !state.editBusy && !state.editNeedsReload ? '' : ' disabled'}>提交 (${count})</button>
        <button data-action="rollback"${(state.edit || state.editAttempt) && !state.editBusy ? '' : ' disabled'}>撤销</button>
        <span class="sep"></span>
        <button data-action="reload-table">刷新</button>
        <span class="flex-spacer"></span>
        <label class="inline">每页
          <select data-action="change-page-size">
            ${[50, 100, 200, 500].map((size) => `<option value="${size}"${size === data.pageSize ? ' selected' : ''}>${size}</option>`).join('')}
          </select>
        </label>
        <button data-action="prev-page"${data.page <= 0 ? ' disabled' : ''}>上一页</button>
        <span class="page-info">第 ${data.page + 1} / ${pageCount} 页 · 共 ${data.total} 行</span>
        <button data-action="next-page"${data.page + 1 >= pageCount ? ' disabled' : ''}>下一页</button>
      </div>
      <div class="grid-scroll">
        <table class="data-grid">
          <thead>
            <tr><th class="row-number"></th><th class="select-cell"></th>${head}</tr>
            <tr class="filter-row"><th class="row-number"></th><th class="select-cell"></th>${filterRow}</tr>
          </thead>
          <tbody>${insertRows}${body}</tbody>
        </table>
        ${data.rows.length || stagedInserts.length ? '' : '<p class="empty">这张表没有数据。</p>'}
      </div>
      ${data.loading ? '<div class="grid-loading">加载中…</div>' : ''}
      ${editable ? '' : '<p class="notice">当前表或事务状态仅允许浏览；系统身份请通过 SQL 管理。</p>'}
    </div>`;
}

// ---- SQL 标签

// 生成行号文本。
function lineNumbers(sql) {
  const count = String(sql ?? '').split('\n').length;
  return Array.from({ length: count }, (_, index) => index + 1).join('\n');
}

// 渲染 SQL 控制台标签。
function renderSqlTab(el, tab) {
  const result = tab.result;
  // 结果区 HTML。
  const resultHtml = (() => {
    if (!result) return '<p class="muted">按 Ctrl+Enter 运行，或点上面的"运行"。</p>';
    if (result.type === 'error') return `<p class="alert alert-error">${escapeHtml(result.message ?? '执行失败')}</p>`;
    if (result.type === 'affected') return `<p class="muted">影响 ${result.affected ?? 0} 行 · ${result.durationMs.toFixed(1)} ms</p>`;
    const head = result.columns.map((column) => `<th>${escapeHtml(column)}</th>`).join('');
    const body = result.rows
      .map((row) => `<tr>${row.map((value) => `<td>${cellContent(value, state.settings.nullText)}</td>`).join('')}</tr>`)
      .join('');
    return `<div class="result-scroll"><table class="result-grid"><thead><tr>${head}</tr></thead><tbody>${body}</tbody></table></div>
      <p class="muted">${result.rows.length} 行${result.truncated ? '（已截断）' : ''} · ${result.durationMs.toFixed(1)} ms</p>`;
  })();

  el.innerHTML = `
    <div class="sql-console">
      <div class="toolbar">
        <button class="primary" data-action="run-sql">运行</button>
        <button data-action="clear-sql">清空</button>
        <span class="sep"></span>
        <span class="muted">库 ${escapeHtml(tab.database ?? state.database)}</span>
      </div>
      <div class="sql-editor">
        ${state.settings.sqlLineNumbers ? `<pre class="sql-gutter" id="sql-gutter">${lineNumbers(tab.sql)}</pre>` : ''}
        <div class="sql-layer">
          <pre class="sql-highlight" id="sql-highlight" aria-hidden="true">${highlightHtml(tab.sql)}</pre>
          <textarea class="sql-input" id="sql-input" spellcheck="false" wrap="off" placeholder="SELECT * FROM users">${escapeHtml(tab.sql)}</textarea>
        </div>
        ${state.settings.minimap ? '<div class="sql-minimap" id="sql-minimap" title="缩略图"><div class="minimap-thumb"></div></div>' : ''}
      </div>
      <div class="sql-result" id="sql-result">${resultHtml}</div>
    </div>`;

  const textarea = region('sql-input');
  const highlight = region('sql-highlight');
  const gutter = region('sql-gutter');
  const minimap = region('sql-minimap');
  if (!textarea) return;

  // 刷新高亮层与行号。
  const refreshLayers = () => {
    if (highlight) highlight.innerHTML = highlightHtml(tab.sql);
    if (gutter) gutter.textContent = lineNumbers(tab.sql);
    sync();
  };
  // 同步滚动位置与缩略图。
  const sync = () => {
    if (highlight) {
      highlight.scrollTop = textarea.scrollTop;
      highlight.scrollLeft = textarea.scrollLeft;
    }
    if (gutter) gutter.scrollTop = textarea.scrollTop;
    if (minimap) {
      const thumb = minimap.querySelector('.minimap-thumb');
      if (!thumb) return;
      const visible = Math.max(8, (textarea.clientHeight / Math.max(1, textarea.scrollHeight)) * 100);
      const ratio = textarea.scrollHeight > textarea.clientHeight
        ? textarea.scrollTop / (textarea.scrollHeight - textarea.clientHeight)
        : 0;
      thumb.style.height = `${visible}%`;
      thumb.style.top = `${Math.min(100 - visible, ratio * (100 - visible))}%`;
    }
  };

  // 输入时更新 SQL 并刷新高亮。
  textarea.addEventListener('input', () => {
    tab.sql = textarea.value;
    refreshLayers();
  });
  // 滚动时同步各层。
  textarea.addEventListener('scroll', sync);
  // Tab 缩进，Ctrl+Enter 运行。
  textarea.addEventListener('keydown', (event) => {
    if (event.key === 'Tab') {
      event.preventDefault();
      const spaces = ' '.repeat(state.settings.sqlTabSize);
      const start = textarea.selectionStart;
      const end = textarea.selectionEnd;
      textarea.value = `${textarea.value.slice(0, start)}${spaces}${textarea.value.slice(end)}`;
      textarea.selectionStart = start + spaces.length;
      textarea.selectionEnd = start + spaces.length;
      tab.sql = textarea.value;
      refreshLayers();
    }
    if (event.key === 'Enter' && (event.ctrlKey || event.metaKey)) {
      event.preventDefault();
      void runSql(tab);
    }
  });
  sync();
}

// ---- 设置标签

// 渲染设置标签页。
function renderSettingsTab(el) {
  const settings = state.settings;
  // 字体链输入框。
  const fontField = (key) => `
    <label class="field"><span>${escapeHtml(settingLabels[key])}</span>
      <input data-setting="fonts" data-key="${key}" value="${escapeHtml(fontsToText(settings[key]))}" placeholder="字体名用逗号分隔" /></label>`;
  // 数值输入框。
  const numberField = (key) => `
    <label class="field"><span>${escapeHtml(settingLabels[key])}</span>
      <input type="number" data-setting="number" data-key="${key}" value="${settings[key]}" /></label>`;
  // 布尔开关。
  const boolField = (key) => `
    <label class="toggle"><input type="checkbox" data-setting="boolean" data-key="${key}"${settings[key] ? ' checked' : ''} /><span>${escapeHtml(settingLabels[key])}</span></label>`;

  // 服务器设置分组表单。
  const serverHtml = (() => {
    const server = state.serverSettings;
    if (!server) return '<p class="muted">正在读取服务器设置…</p>';
    if (!server.items.length) return `<p class="alert alert-error">${escapeHtml(server.error ?? '没有可显示的设置。')}</p>`;
    const groups = [];
    for (const item of server.items) {
      let group = groups.find((entry) => entry.name === item.group);
      if (!group) {
        group = { name: item.group, items: [] };
        groups.push(group);
      }
      group.items.push(item);
    }
    return groups.map((group) => `
      <fieldset class="settings-group"><legend>${escapeHtml(group.name)}</legend>
        ${group.items.map((item) => `
          <label class="field"><span>${escapeHtml(item.label)}${item.rootOnly ? ' <em>（管理员）</em>' : ''}</span>
            <input data-server-setting="${escapeHtml(item.key)}" value="${escapeHtml(item.kind === 'secret' ? '' : item.value)}"${item.editable ? '' : ' disabled'} placeholder="${escapeHtml(item.default)}" />
            <small class="muted">来源：${escapeHtml(item.source)}${item.restart ? ' · 需重启' : item.live ? ' · 立即生效' : ''}${item.locked ? ' · 文件锁定' : ''}</small>
          </label>`).join('')}
      </fieldset>`).join('');
  })();

  el.innerHTML = `
    <div class="settings-view">
      <header><h1>设置</h1><p class="muted">IDE 设置存在 chusql.ui.settings.json，服务器设置写回 settings.toml 的 [web] 分区。</p></header>
      <fieldset class="settings-group"><legend>界面</legend>
        ${fontField('uiFonts')}${numberField('uiFontSize')}
      </fieldset>
      <fieldset class="settings-group"><legend>数据表格</legend>
        ${fontField('gridFonts')}${numberField('gridFontSize')}${numberField('gridRowHeight')}${numberField('pageSize')}
        <label class="field"><span>${escapeHtml(settingLabels.nullText)}</span>
          <input data-setting="text" data-key="nullText" value="${escapeHtml(settings.nullText)}" /></label>
      </fieldset>
      <fieldset class="settings-group"><legend>SQL 控制台</legend>
        ${fontField('sqlFonts')}${numberField('sqlFontSize')}${numberField('sqlLineHeight')}${numberField('sqlTabSize')}
        ${boolField('sqlLineNumbers')}${boolField('autocomplete')}${boolField('minimap')}
      </fieldset>
      <div class="preview" id="settings-preview">
        <strong>预览</strong>
        <div>SELECT * FROM users WHERE id = 1;</div>
      </div>
      <footer class="settings-footer"><button data-action="reset-settings">恢复默认</button></footer>
      <section class="server-settings">${serverHtml}
        <footer class="settings-footer">
          <button class="primary" data-action="save-server-settings">保存服务器设置</button>
          <span class="muted" id="server-settings-note"></span>
        </footer>
      </section>
    </div>`;

  const preview = region('settings-preview');
  if (preview) {
    preview.style.fontFamily = fontFamilyValue(settings.gridFonts);
    preview.style.fontSize = `${settings.gridFontSize}px`;
    preview.style.lineHeight = `${settings.gridRowHeight}px`;
  }
}

// ---------------------------------------------------------------- 下方面板

// 渲染下方面板（变更/消息）。
function renderBottom() {
  const el = region('bottom');
  if (!el) return;
  const count = changeCount(state.changes);
  const entries = changeEntries(state.changes);
  const preview = entries.map((entry) => entry.sql);
  const changeList = entries.length
    ? `<ul class="change-list">${entries.map((entry) => `
        <li>
          <span class="change-table">${escapeHtml(entry.table)}</span>
          <code title="${escapeHtml(entry.sql)}">${escapeHtml(entry.sql)}</code>
          <button class="mini danger" data-action="unstage" data-key="${escapeHtml(entry.key)}" title="撤下这一条">✕</button>
        </li>`).join('')}</ul>
      <details class="sql-preview"><summary>预览生成的全部 SQL（${preview.length} 条）</summary>
        <pre>${escapeHtml(preview.join('\n'))}</pre></details>`
    : '<p class="muted">没有待提交的变更。编辑单元格、插入或删除行都会先记在这里。</p>';
  const messages = state.messages.length
    ? `<ul class="message-list">${state.messages.slice(-100).map((message) => `
        <li class="msg-${message.level}"><span class="msg-time">${escapeHtml(message.time)}</span>${escapeHtml(message.text)}</li>`).join('')}</ul>`
    : '<p class="muted">还没有消息。</p>';

  el.innerHTML = `
    <div class="bottom-head">
      <button class="${state.bottomTab === 'changes' ? 'active' : ''}" data-action="bottom-tab" data-tab="changes">待提交变更 (${count})</button>
      <button class="${state.bottomTab === 'messages' ? 'active' : ''}" data-action="bottom-tab" data-tab="messages">消息 (${state.messages.length})</button>
      <span class="flex-spacer"></span>
      <button data-action="commit"${count && !state.editBusy && !state.editNeedsReload ? '' : ' disabled'}>提交全部</button>
      <button data-action="rollback"${(state.edit || state.editAttempt) && !state.editBusy ? '' : ' disabled'}>撤销全部</button>
      <button data-action="copy-sql"${count ? '' : ' disabled'}>复制 SQL</button>
    </div>
    <div class="bottom-body">${state.bottomTab === 'changes' ? changeList : messages}</div>`;

  const dbEl = region('status-database');
  if (dbEl) dbEl.textContent = state.database ? `库 ${state.database}` : '未选择数据库';
  const changeEl = region('status-changes');
  if (changeEl) changeEl.textContent = `待提交 ${count}`;
}

// ---------------------------------------------------------------- 数据加载

// 读取并缓存当前会话。
async function refreshSession() {
  try {
    state.session = await api.currentSession();
    return true;
  } catch {
    state.session = null;
    return false;
  }
}

// 加载数据库列表与当前库的表。
async function loadWorkspace() {
  try {
    state.databases = await api.listDatabases();
    if (!state.databases.includes(state.database)) {
      // 没有默认库：优先留在原来的库，否则挑第一个非系统库，一个都没有就空着
      state.database = state.databases.filter((name) => name !== 'system')[0] ?? '';
    }
    state.tables = state.database ? await api.listTables(state.database) : [];
    renderSidebar();
    renderBottom();
    setStatus(state.database ? `已加载 ${state.tables.length} 张表` : '未选择数据库：选一个库，或新建一个');
  } catch (error) {
    fail(error);
  }
}

// 切换当前数据库。
async function switchDatabase(name) {
  if (state.editBusy || state.editStarting) return;
  if (name === state.database) return;
  if (changeCount(state.changes) > 0) {
    log('warn', '请先提交或撤销当前库的修改，再切换数据库。');
    renderSidebar();
    return;
  }
  const previousEdit = state.edit ?? state.editAttempt;
  state.editBusy = true;
  afterChange();
  try {
    if (previousEdit) await api.rollbackEdit(previousEdit.id, previousEdit.database);
  } catch (error) {
    state.editBusy = false;
    fail(error);
    afterChange();
    return;
  }
  state.edit = null;
  state.editAttempt = null;
  const result = await api.query(`USE ${name}`, state.database);
  state.editBusy = false;
  if (result.type === 'error') {
    fail(new Error(result.message));
    renderSidebar();
    return;
  }
  state.database = result.database ?? name;
  state.tabs = state.tabs.filter((entry) => entry.kind !== 'table');
  state.activeId = state.tabs[0]?.id;
  log('info', `已切换到数据库 ${state.database}`);
  await loadWorkspace();
  renderEditor();
}

// 打开表标签并加载数据。
async function openTable(database, name) {
  const opened = openTableTab(state.tabs, database, name, nextTabId('table'));
  state.tabs = opened.tabs;
  state.activeId = opened.activeId;
  const tab = state.tabs.find((entry) => entry.id === opened.activeId);
  if (!tab.data) {
    tab.data = {
      columns: [], rows: [], total: 0, page: 0, pageSize: state.settings.pageSize,
      sort: null, dir: 'asc', filters: {}, offset: 0, loading: true,
    };
    tab.selected = new Set();
  }
  tab.database = database;
  renderTabs();
  await loadTableData(tab);
}

// 加载表标签的一页数据。
async function loadTableData(tab) {
  if (state.editBusy || state.editNeedsReload || state.editUncertain) return;
  const data = tab.data;
  data.loading = true;
  renderEditor();
  try {
    if (tab.table !== accountsTable) await ensureEdit(tab.database);
    const meta = await api.getTable(tab.database, tab.table);
    const filters = Object.entries(data.filters ?? {})
      .filter(([, expression]) => String(expression).trim().length > 0)
      .map(([column, expression]) => ({ column, expression }));
    const pageSize = data.pageSize || state.settings.pageSize;
    const offset = data.page * pageSize;
    const page = await api.fetchRows({
      database: tab.database,
      table: tab.table,
      limit: pageSize,
      offset,
      sort: data.sort,
      dir: data.dir,
      filters,
    });
    data.columns = meta.columns;
    data.total = page.total;
    data.offset = offset;
    data.pageSize = pageSize;
    data.error = null;
    data.editId = state.edit?.id;
    // 服务端做等值过滤，界面上的 like / 比较表达式再在本地收敛一次
    data.rows = filters.length
      ? page.rows.filter((row) => filters.every((filter) => matchesFilter(row[filter.column], filter.expression)))
      : page.rows;
    if (!data.rows.length && data.page > 0) data.page -= 1;
  } catch (error) {
    data.error = describe(error);
  } finally {
    data.loading = false;
    renderEditor();
    renderBottom();
  }
}

// 在首个数据表加载时固定编辑快照。
async function ensureEdit(database) {
  if (database !== state.database) throw new Error('请切换到表所属数据库后重新打开。');
  if (state.edit) {
    if (state.edit.database !== database) throw new Error('请先撤销当前数据库的编辑事务。');
    return;
  }
  if (!state.editStarting) {
    const edit = state.editAttempt ?? { id: [...crypto.getRandomValues(new Uint8Array(16))].map((byte) => byte.toString(16).padStart(2, '0')).join(''), database };
    if (edit.database !== database) throw new Error('请先撤销之前的编辑快照，再切换数据库。');
    state.editAttempt = edit;
    const session = state.session;
    state.editStarting = api.beginEdit(edit.id, database).then(() => {
      if (state.session !== session) throw new Error('登录会话已更换，请重新加载。');
      state.edit = edit;
      state.editAttempt = null;
    })
      .finally(() => { state.editStarting = null; });
  }
  await state.editStarting;
}

// 重新加载所有表标签。
async function reloadOpenTables() {
  for (const tab of state.tabs) {
    if (tab.kind === 'table') await loadTableData(tab);
  }
}

// ---------------------------------------------------------------- 变更动作

// 撤下某行的全部变更。
function unstageRow(table, pk) {
  const prefix = `${table}:${JSON.stringify(pk)}:`;
  for (const key of [...state.changes.updates.keys()]) {
    if (key.startsWith(prefix)) state.changes.updates.delete(key);
  }
  state.changes.deletes.delete(deleteKey(table, pk));
}

// 暂存单元格修改。
function stageUpdate(table, database, pk, column, oldValue, newValue) {
  if (!canEdit(database)) return;
  if (oldValue === newValue) state.changes.updates.delete(changeKey(table, pk, column));
  else state.changes.updates.set(changeKey(table, pk, column), { table, database, pk, column, oldValue, newValue });
  afterChange();
}

// 暂存新增行。
function stageInsert(table, database, values) {
  if (!canEdit(database)) return;
  state.insertSeq += 1;
  const tempId = `new-${state.insertSeq}`;
  state.changes.inserts.set(insertKey(table, tempId), { table, database, tempId, values });
  afterChange();
}

// 暂存删除行。
function stageDelete(table, database, pk, row) {
  if (!canEdit(database)) return;
  unstageRow(table, pk);
  state.changes.deletes.set(deleteKey(table, pk), { table, database, pk, row });
  afterChange();
}

// 变更后重绘相关区域。
function afterChange() {
  renderSidebar();
  renderEditor();
  renderBottom();
}

// 撤下一条暂存变更。
function removeStaged(key) {
  if (!canEdit(state.database)) return;
  if (state.changes.updates.has(key)) state.changes.updates.delete(key);
  else if (state.changes.inserts.has(key)) state.changes.inserts.delete(key);
  else if (state.changes.deletes.has(key)) state.changes.deletes.delete(key);
  afterChange();
}

// 撤销全部暂存变更。
async function rollbackAll() {
  if (state.editBusy || state.editStarting) return;
  state.editBusy = true;
  afterChange();
  try {
    const edit = state.edit ?? state.editAttempt;
    const result = edit ? await api.rollbackEdit(edit.id, edit.database) : null;
    state.edit = null;
    state.editAttempt = null;
    state.editNeedsReload = false;
    state.editUncertain = null;
    state.changes = createEmptyChangeSet();
    for (const tab of state.tabs) if (tab.selected) tab.selected.clear();
    log('info', result?.state === 'committed' ? '先前提交已成功，现重新加载数据。' : '已回滚编辑事务，重新加载数据。');
    state.editBusy = false;
    await loadWorkspace();
    await reloadOpenTables();
  } catch (error) {
    state.editNeedsReload = true;
    fail(error);
  } finally {
    state.editBusy = false;
    afterChange();
  }
}

// 检查编辑事务与页面操作状态。
function canEdit(database) {
  return Boolean(state.edit) && state.edit.database === database && !state.editBusy
    && !state.editNeedsReload && !state.editUncertain;
}

// 提交全部暂存变更。
async function commitAll() {
  if (state.editBusy || state.editStarting) return;
  if (state.editNeedsReload || !state.edit) {
    fail(new Error('编辑快照已失效，请撤销并重新加载后编辑。'));
    return;
  }
  const payload = state.editUncertain ?? toCommitPayload(state.changes);
  if (!payload.length) {
    log('info', '没有待提交的变更。');
    return;
  }
  state.editBusy = true;
  afterChange();
  const edit = state.edit;
  const result = await api.commitChanges(edit.id, edit.database, payload);
  if (state.edit !== edit) { state.editBusy = false; return; }
  if (result.ok) {
    state.edit = null;
    state.editUncertain = null;
    state.changes = createEmptyChangeSet();
    for (const tab of state.tabs) if (tab.selected) tab.selected.clear();
    log('info', `已提交 ${result.applied} 条变更。`);
    setStatus(`已提交 ${result.applied} 条变更`);
  } else {
    state.editNeedsReload = result.reloadRequired === true;
    state.editUncertain = result.state === 'unknown' ? payload : null;
    log('error', `${result.message ?? '提交失败'}（事务状态：${result.state}）。${state.editNeedsReload
      ? result.state === 'lost' ? '连接已失效，请重新登录，确认数据后重新编辑。' : '请撤销并重新加载，禁止覆盖其他会话修改。' : state.editUncertain
        ? '结果尚未确认，编辑已冻结；再次提交仅查询同一提交结果。' : '全部修改已回退，编辑内容保留，可修正后重试。'}`);
  }
  state.editBusy = false;
  await loadWorkspace();
  await reloadOpenTables();
  afterChange();
}

// ---------------------------------------------------------------- SQL 运行

// 运行 SQL 并展示结果。
async function runSql(tab) {
  if (state.editBusy || state.editStarting || state.editUncertain || state.editNeedsReload) return;
  if (changeCount(state.changes)) {
    log('warn', '请先提交或撤销表格修改，再运行 SQL。');
    return;
  }
  const sql = String(tab.sql ?? '').trim();
  if (!sql) return;
  state.editBusy = true;
  afterChange();
  const previousEdit = state.edit ?? state.editAttempt;
  if (previousEdit) {
    try {
      await api.rollbackEdit(previousEdit.id, previousEdit.database);
      state.edit = null;
      state.editAttempt = null;
    } catch (error) {
      state.editBusy = false;
      fail(error);
      afterChange();
      return;
    }
  }
  setStatus('执行中…');
  const result = await api.query(sql, tab.database, tab.id);
  state.editBusy = false;
  tab.result = result;
  if (result.type === 'error') {
    log('error', result.message ?? '执行失败');
    setStatus(result.message ?? '执行失败');
  } else {
    if (result.database && result.database !== state.database) {
      state.database = result.database;
      state.tabs = state.tabs.filter((entry) => entry.kind !== 'table');
      tab.database = result.database;
      await loadWorkspace();
    }
    if (result.type === 'rows') {
      log('info', `查询返回 ${result.rows.length} 行`);
      setStatus(`${result.rows.length} 行 · ${result.durationMs.toFixed(1)} ms`);
    } else {
      log('info', `语句影响 ${result.affected ?? 0} 行`);
      setStatus(`影响 ${result.affected ?? 0} 行`);
      if (!result.transactionActive) {
        await loadWorkspace();
        await reloadOpenTables();
      }
    }
  }
  renderEditor();
}

// ---------------------------------------------------------------- 标签动作

// 激活指定标签。
function activateTab(id) {
  state.activeId = id;
  const tab = activeTab();
  if (tab?.kind === 'sql') tab.database = state.database;
  if (tab?.kind === 'settings' && !state.serverSettings) void loadServerSettings();
  if (tab?.kind === 'table' && !state.edit && tab.table !== accountsTable) void loadTableData(tab);
  renderTabs();
}

// 关闭指定标签。
function closeTabById(id) {
  if (state.tabs.find((tab) => tab.id === id)?.kind === 'sql') {
    void api.rollbackConsole(id).catch(fail);
  }
  const closed = closeTab(state.tabs, id, state.activeId);
  state.tabs = closed.tabs;
  state.activeId = closed.activeId;
  renderTabs();
  if (!state.tabs.some((tab) => tab.kind === 'table') && !changeCount(state.changes) && (state.edit || state.editAttempt)) {
    void rollbackAll();
  }
}

// 新开一个查询标签。
function openQuery(sql) {
  const title = `查询 ${state.tabs.filter((tab) => tab.kind === 'sql').length + 1}`;
  const opened = openQueryTab(state.tabs, title, nextTabId('sql'), state.database, sql ?? '');
  state.tabs = opened.tabs;
  state.activeId = opened.activeId;
  renderTabs();
}

// 打开设置标签。
function openSettings() {
  const opened = openSettingsTab(state.tabs, '设置', nextTabId('settings'));
  state.tabs = opened.tabs;
  state.activeId = opened.activeId;
  renderTabs();
  if (!state.serverSettings) void loadServerSettings();
}

// ---------------------------------------------------------------- 设置动作

// 把 IDE 设置写到 CSS 变量。
function applySettingsToDom() {
  const style = document.documentElement.style;
  style.setProperty('--ui-font', fontFamilyValue(state.settings.uiFonts));
  style.setProperty('--ui-font-size', `${state.settings.uiFontSize}px`);
  style.setProperty('--grid-font', fontFamilyValue(state.settings.gridFonts));
  style.setProperty('--grid-font-size', `${state.settings.gridFontSize}px`);
  style.setProperty('--grid-row-height', `${state.settings.gridRowHeight}px`);
  style.setProperty('--sql-font', fontFamilyValue(state.settings.sqlFonts));
  style.setProperty('--sql-font-size', `${state.settings.sqlFontSize}px`);
  style.setProperty('--sql-line-height', `${state.settings.sqlLineHeight}px`);
  style.setProperty('--sql-tab-size', String(state.settings.sqlTabSize));
}

let saveTimer;
// 保存 IDE 设置（本地+服务器）。
function persistSettings() {
  try {
    localStorage.setItem(IDE_SETTINGS_KEY, JSON.stringify(state.settings));
  } catch {
    // 本地存储不可用时忽略：服务器那边仍会保存
  }
  clearTimeout(saveTimer);
  saveTimer = setTimeout(() => {
    api.saveUiSettings(state.settings).catch((error) => log('warn', `IDE 设置没能保存到服务器：${describe(error)}`));
  }, 400);
}

// 更新一项 IDE 设置。
function updateSetting(key, value) {
  state.settings = { ...state.settings, [key]: value };
  applySettingsToDom();
  persistSettings();
  renderEditor();
}

// 从本地存储读 IDE 设置。
function loadLocalSettings() {
  try {
    const raw = localStorage.getItem(IDE_SETTINGS_KEY);
    if (!raw) return;
    state.settings = { ...state.settings, ...parseUiSettings(JSON.parse(raw)) };
  } catch {
    // 本地设置坏了就用默认值
  }
}

// 从服务器读 IDE 设置。
async function loadRemoteSettings() {
  try {
    state.settings = { ...state.settings, ...parseUiSettings(await api.loadUiSettings()) };
    applySettingsToDom();
  } catch (error) {
    log('warn', `IDE 设置没能从服务器读取：${describe(error)}`);
  }
}

// 读取服务器设置。
async function loadServerSettings() {
  try {
    state.serverSettings = await api.loadServerSettings();
  } catch (error) {
    state.serverSettings = { error: describe(error), items: [] };
  }
  renderEditor();
}

// 保存改过的服务器设置。
async function saveServerSettings() {
  const inputs = [...document.querySelectorAll('[data-server-setting]')];
  const values = {};
  for (const input of inputs) {
    if (input.disabled || !input.dataset.dirty) continue;
    values[input.dataset.serverSetting] = input.value;
  }
  if (!Object.keys(values).length) {
    const note = region('server-settings-note');
    if (note) note.textContent = '没有需要保存的改动。';
    return;
  }
  try {
    const result = await api.saveServerSettings(values);
    const applied = Array.isArray(result.applied) ? result.applied : [];
    const restart = Array.isArray(result.restartRequired) ? result.restartRequired : [];
    log('info', `服务器设置已保存：${Object.keys(values).join('、')}`);
    state.serverSettings = await api.loadServerSettings();
    renderEditor();
    const note = region('server-settings-note');
    if (note) {
      note.textContent = `已应用 ${applied.length} 项${restart.length ? `，需重启生效 ${restart.length} 项：${restart.join('、')}` : ''}`;
    }
  } catch (error) {
    fail(error);
  }
}

// ---------------------------------------------------------------- 表格动作

// 切换该列的排序。
function sortColumn(tab, column) {
  const data = tab.data;
  if (data.sort === column) data.dir = data.dir === 'asc' ? 'desc' : 'asc';
  else {
    data.sort = column;
    data.dir = 'asc';
  }
  data.page = 0;
  void loadTableData(tab);
}

// 暂存删除选中的行。
function deleteSelected(tab) {
  const keyColumn = tab.data.columns.find((column) => column.name === 'id');
  if (!keyColumn || !tab.selected) return;
  for (const row of tab.data.rows) {
    if (tab.selected.has(String(row[keyColumn.name]))) stageDelete(tab.table, tab.database, row[keyColumn.name], row);
  }
  tab.selected.clear();
  afterChange();
}

// 就地编辑单元格：提交或放弃。
function beginCellEdit(cell, options) {
  if (/^runtime\(\[1,\[[567],/.test(String(options.type))) {
    setStatus('复合值请通过 SQL 控制台修改');
    return;
  }
  if (cell.querySelector('input')) return;
  const input = document.createElement('input');
  input.className = 'cell-input';
  input.value = editText(options.currentValue);
  cell.textContent = '';
  cell.appendChild(input);
  input.focus();
  input.select();
  let done = false;
  // 结束编辑并按需提交。
  const finish = (save) => {
    if (done) return;
    done = true;
    const raw = input.value;
    input.remove();
    options.commit(save ? parseEdited(raw, options.type) : undefined);
  };
  // Enter 提交，Esc 放弃。
  input.addEventListener('keydown', (event) => {
    if (event.key === 'Enter') {
      event.preventDefault();
      finish(true);
    } else if (event.key === 'Escape') {
      event.preventDefault();
      finish(false);
    }
  });
  // 失焦时提交。
  input.addEventListener('blur', () => finish(true));
}

// 在新建表对话框加一列。
function addColumnRow(name, type) {
  const container = region('dlg-table-columns');
  if (!container) return;
  const row = document.createElement('div');
  row.className = 'column-row';
  row.innerHTML = `
    <input class="column-name" value="${escapeHtml(name)}" placeholder="列名" />
    <select class="column-type">${creatableTypes.map((option) => `<option value="${option}"${option === type ? ' selected' : ''}>${escapeHtml(typeLabel(option))}</option>`).join('')}</select>
    <button class="mini danger" data-action="remove-column" title="移除">✕</button>`;
  container.appendChild(row);
}

// 打开新建表对话框。
function openCreateTableDialog() {
  region('dlg-table-name').value = '';
  region('dlg-table-error').textContent = '';
  region('dlg-table-columns').innerHTML = '';
  addColumnRow('id', 'int');
  addColumnRow('', 'str');
  region('dlg-table').showModal();
}

// 打开建索引对话框。
function openIndexDialog(table) {
  const meta = state.tables.find((entry) => entry.name === table);
  const select = region('dlg-index-column');
  const candidates = (meta?.columns ?? []).filter((column) => column.name !== 'id' && !column.indexed);
  select.innerHTML = candidates
    .map((column) => `<option value="${escapeHtml(column.name)}">${escapeHtml(column.name)}（${escapeHtml(typeLabel(column.type))}）</option>`)
    .join('');
  region('dlg-index-error').textContent = candidates.length ? '' : '没有可建索引的列。';
  region('dlg-index').dataset.table = table;
  region('dlg-index').showModal();
}

// 提交新建数据库。
async function createDatabase() {
  const name = region('dlg-db-name').value.trim();
  try {
    await api.createDatabase(name, state.database);
    region('dlg-db').close();
    log('info', `已创建数据库 ${name}`);
    await loadWorkspace();
  } catch (error) {
    region('dlg-db-error').textContent = describe(error);
  }
}

// 提交新建表。
async function createTable() {
  const name = region('dlg-table-name').value.trim();
  const rows = [...region('dlg-table-columns').querySelectorAll('.column-row')];
  const columns = rows
    .map((row) => ({ name: row.querySelector('.column-name').value.trim(), type: row.querySelector('.column-type').value }))
    .filter((column) => column.name);
  if (!name) {
    region('dlg-table-error').textContent = '表名不能为空。';
    return;
  }
  if (!columns.length) {
    region('dlg-table-error').textContent = '至少需要一列。';
    return;
  }
  try {
    await api.createTable(state.database, name, columns);
    region('dlg-table').close();
    log('info', `已创建表 ${name}`);
    await loadWorkspace();
    await openTable(state.database, name);
  } catch (error) {
    region('dlg-table-error').textContent = describe(error);
  }
}

// 提交建索引。
async function createIndexFor(table) {
  const dialog = region('dlg-index');
  const column = region('dlg-index-column').value;
  if (!column) return;
  try {
    await api.createIndex(state.database, table, column);
    dialog.close();
    log('info', `已在 ${table}.${column} 上建索引`);
    await loadWorkspace();
  } catch (error) {
    region('dlg-index-error').textContent = describe(error);
  }
}

// 删除当前数据库。
async function dropCurrentDatabase() {
  const name = state.database;
  if (!window.confirm(`删除数据库 ${name}？此操作不可撤销。`)) return;
  try {
    await api.dropDatabase(name, name);
    log('info', `已删除数据库 ${name}`);
    state.tabs = state.tabs.filter((tab) => tab.kind !== 'table' || tab.database !== name);
    if (!state.tabs.some((tab) => tab.id === state.activeId)) state.activeId = state.tabs[0]?.id;
    await loadWorkspace();
    renderTabs();
  } catch (error) {
    fail(error);
  }
}

// 删除指定表。
async function dropTableByName(table) {
  if (!window.confirm(`删除表 ${table}？`)) return;
  try {
    await api.dropTable(state.database, table);
    log('info', `已删除表 ${table}`);
    state.tabs = state.tabs.filter((tab) => !(tab.kind === 'table' && tab.table === table));
    if (!state.tabs.some((tab) => tab.id === state.activeId)) state.activeId = state.tabs[0]?.id;
    await loadWorkspace();
    renderTabs();
  } catch (error) {
    fail(error);
  }
}

// 删除指定列。
async function dropColumnByName(table, column) {
  if (!window.confirm(`删除 ${table}.${column}？`)) return;
  try {
    await api.dropColumn(state.database, table, column);
    log('info', `已删除列 ${table}.${column}`);
    await loadWorkspace();
    await reloadOpenTables();
  } catch (error) {
    fail(error);
  }
}

// 删除指定索引。
async function dropIndexByName(table, column) {
  if (!window.confirm(`删除 ${table}.${column} 上的索引？`)) return;
  try {
    await api.dropIndex(state.database, table, column);
    log('info', `已删除 ${table}.${column} 上的索引`);
    await loadWorkspace();
  } catch (error) {
    fail(error);
  }
}

// ---------------------------------------------------------------- 事件代理

// 拖动分隔条调整尺寸。
function startSplitterDrag(kind, event) {
  event.preventDefault();
  const workbench = region('workbench');
  // 按鼠标位置调整宽度或高度。
  const move = (moveEvent) => {
    if (kind === 'v') {
      const width = Math.min(560, Math.max(180, moveEvent.clientX - 48));
      state.sidebarWidth = width;
      workbench.style.setProperty('--sidebar-width', `${width}px`);
    } else {
      const height = Math.min(window.innerHeight - 160, Math.max(80, window.innerHeight - moveEvent.clientY - 22));
      state.dockHeight = `${height}px`;
      workbench.style.setProperty('--dock-height', `${height}px`);
    }
  };
  // 移除拖动监听。
  const stop = () => {
    window.removeEventListener('mousemove', move);
    window.removeEventListener('mouseup', stop);
  };
  window.addEventListener('mousemove', move);
  window.addEventListener('mouseup', stop);
}

// 这些动作都要求已经选中一个库（服务没有默认库）
const DATABASE_ACTIONS = new Set([
  'open-table', 'demo-data', 'open-table-dialog', 'create-table',
  'open-index-dialog', 'create-index', 'drop-table', 'drop-column', 'drop-index',
  'drop-database', 'commit', 'rollback', 'copy-sql',
]);

// 该动作是否需要当前库。
function needsDatabase(action) {
  return DATABASE_ACTIONS.has(action);
}

// 点击事件代理。
function onClick(event) {
  const splitter = event.target.closest('[data-split]');
  if (splitter) {
    startSplitterDrag(splitter.dataset.split, event);
    return;
  }
  const target = event.target.closest('[data-action]');
  if (!target) return;
  const action = target.dataset.action;
  if (state.editBusy || state.editStarting) return;
  const tab = activeTab();

  // 没有选库时，需要当前库的动作先拦下来（服务端也会拒，这里给人话）
  if (!state.database && needsDatabase(action)) {
    log('warn', '还没有选择数据库：先在左侧数据库下拉里选一个，或者新建一个库。');
    return;
  }

  switch (action) {
    case 'logout':
      void (async () => {
        try {
          await api.logout();
        } catch (error) {
          fail(error);
          return;
        }
        state.session = null;
        state.tabs = [];
        state.activeId = undefined;
        state.changes = createEmptyChangeSet();
        state.edit = null;
        state.editAttempt = null;
        state.editNeedsReload = false;
        state.editUncertain = null;
        api.forgetEdit();
        renderLogin('已退出登录。', 'info');
      })();
      return;
    case 'new-query':
      openQuery('');
      return;
    case 'focus-explorer': {
      const sidebar = region('sidebar');
      if (sidebar) sidebar.scrollTop = 0;
      return;
    }
    case 'settings':
      openSettings();
      return;
    case 'close-dialog': {
      const dialog = target.closest('dialog');
      if (dialog) dialog.close();
      return;
    }
    case 'activate-tab':
      activateTab(target.dataset.tab);
      return;
    case 'close-tab':
      closeTabById(target.dataset.tab);
      return;
    case 'bottom-tab':
      state.bottomTab = target.dataset.tab;
      renderBottom();
      return;
    case 'expand-table': {
      const name = target.dataset.table;
      if (state.expanded.has(name)) state.expanded.delete(name);
      else state.expanded.add(name);
      renderSidebar();
      return;
    }
    case 'open-table':
      void openTable(state.database, target.dataset.table);
      return;
    case 'refresh':
      void loadWorkspace().then(reloadOpenTables);
      return;
    case 'demo-data':
      void (async () => {
        try {
          const report = await api.seedDemoData(state.database);
          log('info', `演示数据：新建 ${(report.created ?? []).length} 张，保留 ${(report.skipped ?? []).length} 张。`);
          await loadWorkspace();
          await reloadOpenTables();
        } catch (error) {
          fail(error);
        }
      })();
      return;
    case 'open-db-dialog':
      region('dlg-db-name').value = '';
      region('dlg-db-error').textContent = '';
      region('dlg-db').showModal();
      return;
    case 'open-table-dialog':
      openCreateTableDialog();
      return;
    case 'add-column':
      addColumnRow('', 'int');
      return;
    case 'remove-column': {
      const row = target.closest('.column-row');
      if (row) row.remove();
      return;
    }
    case 'create-database':
      void createDatabase();
      return;
    case 'create-table':
      void createTable();
      return;
    case 'open-index-dialog':
      openIndexDialog(target.dataset.table);
      return;
    case 'create-index':
      void createIndexFor(region('dlg-index').dataset.table);
      return;
    case 'drop-database':
      void dropCurrentDatabase();
      return;
    case 'drop-table':
      void dropTableByName(target.dataset.table);
      return;
    case 'drop-column':
      void dropColumnByName(target.dataset.table, target.dataset.column);
      return;
    case 'drop-index':
      void dropIndexByName(target.dataset.table, target.dataset.column);
      return;
    case 'sort-column':
      if (tab?.kind === 'table') sortColumn(tab, target.dataset.column);
      return;
    case 'prev-page':
      if (tab?.kind === 'table' && tab.data.page > 0) {
        tab.data.page -= 1;
        void loadTableData(tab);
      }
      return;
    case 'next-page':
      if (tab?.kind === 'table') {
        tab.data.page += 1;
        void loadTableData(tab);
      }
      return;
    case 'reload-table':
      if (tab?.kind === 'table') void loadTableData(tab);
      return;
    case 'insert-row':
      if (tab?.kind === 'table') {
        const values = {};
        for (const column of tab.data.columns) values[column.name] = null;
        if (tab.table === accountsTable) {
          values.user = '';
          values.password = '';
        }
        stageInsert(tab.table, tab.database, values);
      }
      return;
    case 'delete-selected':
      if (tab?.kind === 'table') deleteSelected(tab);
      return;
    case 'unstage':
      removeStaged(target.dataset.key);
      return;
    case 'unstage-insert': {
      const owner = tab?.kind === 'table' ? tab : undefined;
      if (owner) removeStaged(insertKey(owner.table, target.dataset.temp));
      return;
    }
    case 'commit':
      void commitAll();
      return;
    case 'rollback':
      void rollbackAll();
      return;
    case 'copy-sql': {
      const text = changesSql(state.changes).join('\n');
      const clipboard = navigator.clipboard;
      if (!clipboard) {
        log('warn', '这个浏览器不允许写剪贴板，请手动选中下面的 SQL。');
        return;
      }
      clipboard.writeText(text).then(
        () => log('info', 'SQL 已复制到剪贴板。'),
        () => log('warn', '复制失败，请手动选中下面的 SQL。'),
      );
      return;
    }
    case 'run-sql':
      if (tab?.kind === 'sql') void runSql(tab);
      return;
    case 'clear-sql':
      if (tab?.kind === 'sql') {
        tab.sql = '';
        tab.result = undefined;
        renderEditor();
      }
      return;
    case 'reset-settings':
      state.settings = { ...DEFAULT_SETTINGS };
      applySettingsToDom();
      persistSettings();
      renderEditor();
      return;
    case 'save-server-settings':
      void saveServerSettings();
      return;
    case 'edit-cell': {
      if (tab?.kind !== 'table') return;
      const pk = target.dataset.pk;
      const column = target.dataset.column;
      const meta = tab.data.columns.find((entry) => entry.name === column);
      const row = tab.data.rows.find((entry) => String(entry.id) === String(pk));
      if (!meta || !row) return;
      const staged = state.changes.updates.get(changeKey(tab.table, pk, column));
      const oldValue = staged ? staged.oldValue : row[column.name];
      const editId = state.edit?.id;
      beginCellEdit(target, {
        type: meta.type,
        currentValue: staged ? staged.newValue : row[column.name],
        commit: (value) => {
          if (state.edit?.id !== editId) return;
          if (value === undefined) renderEditor();
          else stageUpdate(tab.table, tab.database, pk, column, oldValue, value);
        },
      });
      return;
    }
    case 'edit-insert': {
      if (tab?.kind !== 'table') return;
      const insert = state.changes.inserts.get(insertKey(tab.table, target.dataset.temp));
      if (!insert) return;
      const column = target.dataset.column;
      const meta = tab.data.columns.find((entry) => entry.name === column);
      beginCellEdit(target, {
        type: meta ? meta.type : 'str',
        currentValue: insert.values[column],
        commit: (value) => {
          if (!canEdit(tab.database)) return;
          if (value === undefined) renderEditor();
          else {
            insert.values[column] = value;
            afterChange();
          }
        },
      });
      return;
    }
    default:
  }
}

// 输入与选择事件代理。
function onChange(event) {
  const target = event.target;
  if (target.dataset.action === 'switch-database') {
    void switchDatabase(target.value);
    return;
  }
  if (target.dataset.action === 'filter-column') {
    const tab = activeTab();
    if (tab?.kind !== 'table') return;
    tab.data.filters = { ...(tab.data.filters ?? {}), [target.dataset.column]: target.value };
    tab.data.page = 0;
    void loadTableData(tab);
    return;
  }
  if (target.dataset.action === 'change-page-size') {
    const tab = activeTab();
    if (tab?.kind !== 'table') return;
    tab.data.pageSize = Number(target.value);
    tab.data.page = 0;
    void loadTableData(tab);
    return;
  }
  if (target.dataset.action === 'select-row') {
    const tab = activeTab();
    if (tab?.kind !== 'table') return;
    tab.selected = tab.selected ?? new Set();
    if (target.checked) tab.selected.add(String(target.dataset.pk));
    else tab.selected.delete(String(target.dataset.pk));
    renderEditor();
    return;
  }
  if (target.dataset.serverSetting) {
    target.dataset.dirty = '1';
    return;
  }
  const setting = target.dataset.setting;
  if (!setting) return;
  const key = target.dataset.key;
  if (setting === 'number') updateSetting(key, clampSetting(key, target.value));
  else if (setting === 'boolean') updateSetting(key, target.checked);
  else if (setting === 'fonts') updateSetting(key, fontsFromText(target.value));
  else updateSetting(key, target.value);
}

// 提交事件代理（登录表单）。
function onSubmit(event) {
  if (event.target.id !== 'login-form') return;
  event.preventDefault();
  const user = region('login-user').value.trim();
  const password = region('login-password').value;
  state.loading = true;
  renderLogin('', 'info');
  void (async () => {
    try {
      await api.login(user, password);
      state.loading = false;
      if (!(await refreshSession())) {
        renderLogin('登录成功但会话读取失败，请重试。', 'error');
        return;
      }
      renderShell();
      renderSidebar();
      renderTabs();
      renderBottom();
      applySettingsToDom();
      await loadWorkspace();
      log('info', `已登录：${state.session.user}`);
    } catch (error) {
      state.loading = false;
      renderLogin(describe(error), 'error');
    }
  })();
}

// 键盘事件代理。
function onKeydown(event) {
  if (event.key === 'Enter' && event.target.id === 'dlg-db-name') {
    event.preventDefault();
    void createDatabase();
  }
}

// ---------------------------------------------------------------- 启动

// 会话过期时回到登录页。
window.addEventListener(AUTH_EVENT, () => {
  if (state.view === 'login') return;
  state.session = null;
  state.tabs = [];
  state.activeId = undefined;
  state.changes = createEmptyChangeSet();
  state.edit = null;
  state.editAttempt = null;
  state.editStarting = null;
  state.editUncertain = null;
  state.editNeedsReload = false;
  api.forgetEdit();
  renderLogin('会话已过期，请重新登录。', 'warn');
});

// 离开页面时请求回滚仍持有的编辑快照。
window.addEventListener('pagehide', () => {
  api.closeConsoleOnPagehide();
  const edit = state.edit ?? state.editAttempt;
  if (edit && !state.editBusy) {
    navigator.sendBeacon('/api/edit/rollback', new Blob([JSON.stringify(edit)], { type: 'application/json' }));
  }
});

// 未提交或结果未知时提示页面关闭。
window.addEventListener('beforeunload', (event) => {
  if (changeCount(state.changes) || state.editBusy) {
    event.preventDefault();
    event.returnValue = '';
  }
});

root.addEventListener('click', onClick);
root.addEventListener('change', onChange);
root.addEventListener('submit', onSubmit);
root.addEventListener('keydown', onKeydown);

// 启动：读设置、恢复会话、渲染。
async function boot() {
  loadLocalSettings();
  applySettingsToDom();
  if (!(await refreshSession())) {
    renderLogin('', 'info');
    return;
  }
  renderShell();
  renderSidebar();
  renderTabs();
  renderBottom();
  applySettingsToDom();
  await loadWorkspace();
  renderEditor();
  await loadRemoteSettings();
  renderEditor();
}

void boot();
