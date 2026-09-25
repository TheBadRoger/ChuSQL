import { act, fireEvent, render, screen } from '@testing-library/react';
import { afterEach, beforeEach, expect, it, vi } from 'vitest';
import { zhCN } from '../../i18n/zh-CN';
import { Workbench } from './Workbench';

// 工作台布局测试：拖动、方向键、夹紧范围与本地存储持久化。

// 用最简桩节点渲染工作台
function renderWorkbench() {
  render(<Workbench explorer={<div />} editor={<div />} bottom={<div />} status={<div />} changeCount={0} canUndo={false} canRedo={false} onCommit={vi.fn()} onRollback={vi.fn()} onUndo={vi.fn()} onRedo={vi.fn()} onSql={vi.fn()} onSettings={vi.fn()} />);
}

// 取侧边栏与船坞两条拖动条
function splitters() {
  return { sidebar: screen.getByRole('separator', { name: zhCN.resizeSidebar }), dock: screen.getByRole('separator', { name: zhCN.resizeDock }) };
}

// 取底部船坞默认高度
function defaultDock() { return Math.round(window.innerHeight * 0.22); }

// 取底部船坞高度上限
function dockCeiling() { return Math.max(96, Math.round(window.innerHeight * 0.7)); }

beforeEach(() => localStorage.clear());
afterEach(() => localStorage.clear());

it('resizes the sidebar and the dock by 16px with arrow keys', () => {
  renderWorkbench();
  const { sidebar, dock } = splitters();
  expect(sidebar).toHaveAttribute('aria-valuenow', '250');
  fireEvent.keyDown(sidebar, { key: 'ArrowRight' });
  expect(sidebar).toHaveAttribute('aria-valuenow', '266');
  fireEvent.keyDown(sidebar, { key: 'ArrowLeft' });
  expect(sidebar).toHaveAttribute('aria-valuenow', '250');
  expect(dock).toHaveAttribute('aria-valuenow', String(defaultDock()));
  fireEvent.keyDown(dock, { key: 'ArrowUp' });
  expect(dock).toHaveAttribute('aria-valuenow', String(defaultDock() + 16));
  fireEvent.keyDown(dock, { key: 'ArrowDown' });
  expect(dock).toHaveAttribute('aria-valuenow', String(defaultDock()));
});

it('clamps the sidebar to 160..560 and the dock to 96 and above', () => {
  renderWorkbench();
  const { sidebar, dock } = splitters();
  for (let i = 0; i < 30; i += 1) fireEvent.keyDown(sidebar, { key: 'ArrowLeft' });
  expect(sidebar).toHaveAttribute('aria-valuenow', '160');
  fireEvent.keyDown(sidebar, { key: 'ArrowLeft' });
  expect(sidebar).toHaveAttribute('aria-valuenow', '160');
  for (let i = 0; i < 40; i += 1) fireEvent.keyDown(sidebar, { key: 'ArrowRight' });
  expect(sidebar).toHaveAttribute('aria-valuenow', '560');
  fireEvent.keyDown(sidebar, { key: 'ArrowRight' });
  expect(sidebar).toHaveAttribute('aria-valuenow', '560');
  for (let i = 0; i < 30; i += 1) fireEvent.keyDown(dock, { key: 'ArrowDown' });
  expect(dock).toHaveAttribute('aria-valuenow', '96');
  fireEvent.keyDown(dock, { key: 'ArrowDown' });
  expect(dock).toHaveAttribute('aria-valuenow', '96');
  for (let i = 0; i < 60; i += 1) fireEvent.keyDown(dock, { key: 'ArrowUp' });
  expect(dock).toHaveAttribute('aria-valuenow', String(dockCeiling()));
});

it('exposes separator roles with orientation, value and bounds', () => {
  renderWorkbench();
  const { sidebar, dock } = splitters();
  expect(sidebar).toHaveAttribute('aria-orientation', 'vertical');
  expect(sidebar).toHaveAttribute('aria-valuemin', '160');
  expect(sidebar).toHaveAttribute('aria-valuemax', '560');
  expect(sidebar).toHaveAttribute('tabindex', '0');
  expect(dock).toHaveAttribute('aria-orientation', 'horizontal');
  expect(dock).toHaveAttribute('aria-valuemin', '96');
  expect(dock).toHaveAttribute('aria-valuemax', String(dockCeiling()));
  expect(dock).toHaveAttribute('tabindex', '0');
});

it('stores changed sizes in localStorage under chusql.layout.v1', () => {
  renderWorkbench();
  const { sidebar, dock } = splitters();
  fireEvent.keyDown(sidebar, { key: 'ArrowRight' });
  fireEvent.keyDown(dock, { key: 'ArrowUp' });
  expect(JSON.parse(localStorage.getItem('chusql.layout.v1') ?? '{}')).toEqual({ sidebarWidth: 266, dockHeight: defaultDock() + 16 });
});

it('restores saved sizes from localStorage on the first render', () => {
  localStorage.setItem('chusql.layout.v1', JSON.stringify({ sidebarWidth: 300, dockHeight: 150 }));
  renderWorkbench();
  const { sidebar, dock } = splitters();
  expect(sidebar).toHaveAttribute('aria-valuenow', '300');
  expect(dock).toHaveAttribute('aria-valuenow', '150');
});

it('clamps saved sizes that fall outside the allowed range', () => {
  localStorage.setItem('chusql.layout.v1', JSON.stringify({ sidebarWidth: 9999, dockHeight: 1 }));
  renderWorkbench();
  const { sidebar, dock } = splitters();
  expect(sidebar).toHaveAttribute('aria-valuenow', '560');
  expect(dock).toHaveAttribute('aria-valuenow', '96');
});

it('falls back to default sizes when the stored layout is unreadable', () => {
  localStorage.setItem('chusql.layout.v1', '{not json');
  renderWorkbench();
  const { sidebar, dock } = splitters();
  expect(sidebar).toHaveAttribute('aria-valuenow', '250');
  expect(dock).toHaveAttribute('aria-valuenow', String(defaultDock()));
});

it('widens the sidebar by 40px when the vertical splitter is dragged right', () => {
  renderWorkbench();
  const { sidebar } = splitters();
  act(() => {
    sidebar.dispatchEvent(new MouseEvent('pointerdown', { clientX: 300, bubbles: true }));
  });
  act(() => {
    window.dispatchEvent(new MouseEvent('pointermove', { clientX: 340, bubbles: true }));
  });
  act(() => {
    window.dispatchEvent(new MouseEvent('pointerup', { bubbles: true }));
  });
  expect(sidebar).toHaveAttribute('aria-valuenow', '290');
});

it('makes the dock taller by 40px when the horizontal splitter is dragged up', () => {
  renderWorkbench();
  const { dock } = splitters();
  act(() => {
    dock.dispatchEvent(new MouseEvent('pointerdown', { clientY: 500, bubbles: true }));
  });
  act(() => {
    window.dispatchEvent(new MouseEvent('pointermove', { clientY: 460, bubbles: true }));
  });
  act(() => {
    window.dispatchEvent(new MouseEvent('pointerup', { bubbles: true }));
  });
  expect(dock).toHaveAttribute('aria-valuenow', String(defaultDock() + 40));
});

it('stops resizing after the pointer is released', () => {
  renderWorkbench();
  const { sidebar } = splitters();
  act(() => {
    sidebar.dispatchEvent(new MouseEvent('pointerdown', { clientX: 300, bubbles: true }));
  });
  act(() => {
    window.dispatchEvent(new MouseEvent('pointerup', { bubbles: true }));
  });
  act(() => {
    window.dispatchEvent(new MouseEvent('pointermove', { clientX: 400, bubbles: true }));
  });
  expect(sidebar).toHaveAttribute('aria-valuenow', '250');
});
