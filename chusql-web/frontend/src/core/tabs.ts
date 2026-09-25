// 标签页状态迁移：打开与关闭均返回新状态，不改入参数组。

export type TabKind = 'table' | 'sql' | 'settings';

export interface Tab { id: string; kind: TabKind; title: string; table?: string; sql?: string }

export interface TabState { tabs: Tab[]; activeId: string | undefined }

// 打开表标签：同表已开则激活，否则追加。
export function openTableTab(tabs: Tab[], name: string, id: string): TabState {
  const existing = tabs.find((tab) => tab.kind === 'table' && tab.table === name);
  if (existing) return { tabs, activeId: existing.id };
  return { tabs: [...tabs, { id, kind: 'table', title: name, table: name }], activeId: id };
}

// 打开查询标签：每次追加，可选初始 SQL。
export function openQueryTab(tabs: Tab[], title: string, id: string, sql?: string): TabState {
  return { tabs: [...tabs, { id, kind: 'sql', title, sql }], activeId: id };
}

// 打开设置标签：只保留一个，已存在则激活。
export function openSettingsTab(tabs: Tab[], title: string, id: string): TabState {
  const existing = tabs.find((tab) => tab.kind === 'settings');
  if (existing) return { tabs, activeId: existing.id };
  return { tabs: [...tabs, { id, kind: 'settings', title }], activeId: id };
}

// 关闭标签：关当前标签时激活右邻，无则左邻。
export function closeTab(tabs: Tab[], id: string, activeId: string | undefined): TabState {
  const index = tabs.findIndex((tab) => tab.id === id);
  if (index < 0) return { tabs, activeId };
  const remaining = tabs.filter((tab) => tab.id !== id);
  if (activeId !== id) return { tabs: remaining, activeId };
  return { tabs: remaining, activeId: (remaining[index] ?? remaining[index - 1])?.id };
}
