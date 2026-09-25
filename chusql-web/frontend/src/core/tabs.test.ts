import { describe, expect, it } from 'vitest';
import { closeTab, openQueryTab, openSettingsTab, openTableTab, type Tab } from './tabs';

// 覆盖标签页打开与关闭的状态迁移。

const base: Tab[] = [
  { id: 'tab-1', kind: 'table', title: 'users', table: 'users' },
  { id: 'tab-2', kind: 'table', title: 'orders', table: 'orders' },
  { id: 'tab-3', kind: 'sql', title: '查询 1' },
];

describe('openTableTab', () => {
  it('activates an existing tab instead of appending a duplicate', () => {
    const state = openTableTab(base, 'users', 'tab-9');
    expect(state.tabs).toHaveLength(3);
    expect(state.activeId).toBe('tab-1');
    expect(state.tabs).toBe(base);
  });

  it('appends a new tab for a table that is not open yet', () => {
    const state = openTableTab(base, 'products', 'tab-9');
    expect(state.tabs).toHaveLength(4);
    expect(state.tabs[3]).toEqual({ id: 'tab-9', kind: 'table', title: 'products', table: 'products' });
    expect(state.activeId).toBe('tab-9');
  });
});

describe('openQueryTab', () => {
  it('always appends so queries can be opened multiple times', () => {
    const first = openQueryTab(base, '查询 1', 'tab-9');
    const second = openQueryTab(first.tabs, '查询 2', 'tab-10', 'SELECT 1');
    expect(second.tabs).toHaveLength(5);
    expect(second.activeId).toBe('tab-10');
    expect(second.tabs[4]).toEqual({ id: 'tab-10', kind: 'sql', title: '查询 2', sql: 'SELECT 1' });
  });
});

describe('openSettingsTab', () => {
  it('keeps a single settings tab and activates it', () => {
    const first = openSettingsTab(base, '设置', 'tab-9');
    const second = openSettingsTab(first.tabs, '设置', 'tab-10');
    expect(second.tabs.filter((tab) => tab.kind === 'settings')).toHaveLength(1);
    expect(second.tabs).toHaveLength(4);
    expect(second.activeId).toBe('tab-9');
  });
});

describe('closeTab', () => {
  it('activates the tab that shifts in from the right', () => {
    const state = closeTab(base, 'tab-1', 'tab-1');
    expect(state.tabs.map((tab) => tab.id)).toEqual(['tab-2', 'tab-3']);
    expect(state.activeId).toBe('tab-2');
  });

  it('falls back to the left neighbour when the last tab closes', () => {
    const state = closeTab(base, 'tab-3', 'tab-3');
    expect(state.activeId).toBe('tab-2');
  });

  it('keeps the active tab when a different one closes', () => {
    const state = closeTab(base, 'tab-2', 'tab-3');
    expect(state.tabs).toHaveLength(2);
    expect(state.activeId).toBe('tab-3');
  });

  it('clears the active id when the only tab closes', () => {
    const state = closeTab([base[0]], 'tab-1', 'tab-1');
    expect(state.tabs).toHaveLength(0);
    expect(state.activeId).toBeUndefined();
  });

  it('does not mutate the input array', () => {
    const snapshot = JSON.stringify(base);
    closeTab(base, 'tab-2', 'tab-2');
    openTableTab(base, 'products', 'tab-9');
    openQueryTab(base, '查询 2', 'tab-10');
    openSettingsTab(base, '设置', 'tab-11');
    expect(JSON.stringify(base)).toBe(snapshot);
  });
});
