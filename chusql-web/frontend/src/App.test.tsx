import { fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { beforeEach, describe, expect, it, vi } from 'vitest';
import { useChangeStore } from './store/changeStore';
import { App } from './App';
import { MockAdapter } from './adapters/MockAdapter';

// App 组件测试：校验行删除、编辑提交与多标签编辑区。

vi.mock('./components/sql/SqlConsole', () => ({ SqlConsole: () => <div data-testid="sql-console" /> }));

describe('App table editing flow', () => {
  beforeEach(() => useChangeStore.getState().clear());
  it('stages row deletion on the first click and allows undo', async () => {
    const adapter = new MockAdapter({ latencyMs: 0 });
    const commit = vi.spyOn(adapter, 'commitChanges');
    render(<App adapter={adapter} />);
    fireEvent.click(await screen.findByRole('button', { name: '打开表 users' }));
    fireEvent.click(await screen.findByText('User_001'));
    fireEvent.click(screen.getByRole('button', { name: '删除选中行' }));
    expect(useChangeStore.getState().changes.deletes.size).toBe(1);
    expect(commit).not.toHaveBeenCalled();
    fireEvent.click(screen.getByRole('button', { name: '撤销' }));
    expect(useChangeStore.getState().changes.deletes.size).toBe(0);
  });
  it('keeps edits local and commits the complete change set once', async () => {
    const adapter = new MockAdapter({ latencyMs: 0 });
    const commit = vi.spyOn(adapter, 'commitChanges');
    render(<App adapter={adapter} />);

    expect(screen.getByText('资源管理器')).toBeInTheDocument();
    fireEvent.click(await screen.findByRole('button', { name: /打开表 users/ }));
    const cell = await screen.findByText('User_001');
    fireEvent.doubleClick(cell);
    const editor = screen.getByRole('textbox', { name: '编辑 name' });
    fireEvent.change(editor, { target: { value: 'edited' } });
    fireEvent.keyDown(editor, { key: 'Enter' });

    expect(screen.getByRole('button', { name: '提交 (1)' })).toBeEnabled();
    expect(commit).not.toHaveBeenCalled();
    fireEvent.click(screen.getByRole('button', { name: '提交 (1)' }));
    await waitFor(() => expect(commit).toHaveBeenCalledTimes(1));
    expect(commit.mock.calls[0][0].changes).toHaveLength(1);
  });
});

describe('App tabs', () => {
  beforeEach(() => useChangeStore.getState().clear());

  it('keeps two tables open and switches back to the first one', async () => {
    render(<App adapter={new MockAdapter({ latencyMs: 0 })} />);
    fireEvent.click(await screen.findByRole('button', { name: '打开表 users' }));
    expect(await screen.findByText('User_001')).toBeInTheDocument();
    fireEvent.click(screen.getByRole('button', { name: '打开表 orders' }));
    expect(screen.getAllByRole('tab')).toHaveLength(2);
    expect(screen.getByRole('tab', { name: 'orders' })).toHaveAttribute('aria-selected', 'true');
    fireEvent.click(screen.getByRole('tab', { name: 'users' }));
    expect(screen.getByRole('tab', { name: 'users' })).toHaveAttribute('aria-selected', 'true');
    expect(await screen.findByText('User_001')).toBeInTheDocument();
  });

  it('does not add a second tab for a table that is already open', async () => {
    render(<App adapter={new MockAdapter({ latencyMs: 0 })} />);
    fireEvent.click(await screen.findByRole('button', { name: '打开表 users' }));
    fireEvent.click(screen.getByRole('button', { name: '打开表 users' }));
    expect(screen.getAllByRole('tab')).toHaveLength(1);
    expect(await screen.findByText('User_001')).toBeInTheDocument();
  });

  it('activates the neighbouring tab after closing the active one', async () => {
    render(<App adapter={new MockAdapter({ latencyMs: 0 })} />);
    fireEvent.click(await screen.findByRole('button', { name: '打开表 users' }));
    await screen.findByText('User_001');
    fireEvent.click(screen.getByRole('button', { name: '打开表 orders' }));
    fireEvent.click(screen.getByRole('button', { name: '关闭 orders' }));
    expect(screen.getAllByRole('tab')).toHaveLength(1);
    expect(screen.queryByRole('tab', { name: 'orders' })).not.toBeInTheDocument();
    expect(await screen.findByText('User_001')).toBeInTheDocument();
  });

  it('opens one tab for every new query', async () => {
    render(<App adapter={new MockAdapter({ latencyMs: 0 })} />);
    fireEvent.click(await screen.findByRole('button', { name: '新建查询' }));
    fireEvent.click(screen.getByRole('button', { name: '新建查询' }));
    expect(screen.getAllByRole('tab')).toHaveLength(2);
    expect(await screen.findByTestId('sql-console')).toBeInTheDocument();
    expect(screen.getAllByTestId('sql-console')).toHaveLength(1);
  });
});

describe('App pending changes', () => {
  beforeEach(() => useChangeStore.getState().clear());

  it('shows a staged row edit as highlighted SQL instead of JSON', async () => {
    render(<App adapter={new MockAdapter({ latencyMs: 0 })} />);
    fireEvent.click(await screen.findByRole('button', { name: '打开表 users' }));
    fireEvent.doubleClick(await screen.findByText('User_001'));
    const editor = screen.getByRole('textbox', { name: '编辑 name' });
    fireEvent.change(editor, { target: { value: "A'B" } });
    fireEvent.keyDown(editor, { key: 'Enter' });
    const code = await screen.findByTitle("UPDATE users SET name = 'A''B' WHERE id = 1;");
    expect(code).toHaveTextContent("UPDATE users SET name = 'A''B' WHERE id = 1;");
    expect(within(code).getByText('UPDATE')).toHaveStyle({ color: '#569CD6' });
    expect(within(code).getByText("'A''B'")).toHaveStyle({ color: '#CE9178' });
    expect(within(code).getByText('users')).toHaveStyle({ color: 'rgb(200, 200, 200)' });
    expect(screen.queryByText(/\{"id":1/)).not.toBeInTheDocument();
  });

  it('counts a staged index change as a pending change and previews its SQL', async () => {
    render(<App adapter={new MockAdapter({ latencyMs: 0 })} />);
    fireEvent.click(await screen.findByRole('button', { name: '打开表 users' }));
    await screen.findByText('User_001');
    fireEvent.click(screen.getByRole('button', { name: '管理 users 的索引' }));
    const indexRows = within(screen.getByRole('dialog')).getAllByRole('listitem');
    fireEvent.click(within(indexRows[2]).getByRole('button', { name: '建索引' }));
    expect(await screen.findByTitle('CREATE INDEX ON users (age);')).toBeInTheDocument();
    expect(screen.getByRole('button', { name: '提交 (1)' })).toBeEnabled();
  });

  it('creates a staged table only when the change set is committed', async () => {
    const adapter = new MockAdapter({ latencyMs: 0 });
    render(<App adapter={adapter} />);
    fireEvent.click(await screen.findByRole('button', { name: '新建表' }));
    fireEvent.change(screen.getByLabelText('表名'), { target: { value: 'items' } });
    fireEvent.change(screen.getByLabelText('列名 2'), { target: { value: 'title' } });
    fireEvent.click(screen.getByRole('button', { name: '建表' }));
    expect(await screen.findByTitle('CREATE TABLE items (id int, title str);')).toBeInTheDocument();
    expect((await adapter.listTables('chusql')).some((table) => table.name === 'items')).toBe(false);
    fireEvent.click(screen.getByRole('button', { name: '提交 (1)' }));
    await waitFor(async () => {
      expect((await adapter.listTables('chusql')).some((table) => table.name === 'items')).toBe(true);
    });
    expect(await screen.findByRole('button', { name: '打开表 items' })).toBeInTheDocument();
  });

  it('selects a single row on click, toggles with Ctrl and extends with Shift', async () => {
    render(<App adapter={new MockAdapter({ latencyMs: 0 })} />);
    fireEvent.click(await screen.findByRole('button', { name: '打开表 users' }));
    fireEvent.click(await screen.findByText('User_001'));
    expect(screen.getByText('已选 1 行')).toBeInTheDocument();
    fireEvent.click(screen.getByText('User_002'), { ctrlKey: true });
    expect(screen.getByText('已选 2 行')).toBeInTheDocument();
    fireEvent.click(screen.getByText('User_002'), { ctrlKey: true });
    expect(screen.getByText('已选 1 行')).toBeInTheDocument();
    fireEvent.click(screen.getByText('User_001'));
    fireEvent.click(screen.getByText('User_004'), { shiftKey: true });
    expect(screen.getByText('已选 4 行')).toBeInTheDocument();
    fireEvent.click(screen.getByText('User_003'));
    expect(screen.getByText('已选 1 行')).toBeInTheDocument();
  });
});
