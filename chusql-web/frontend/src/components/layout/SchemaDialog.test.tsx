import { cleanup, fireEvent, render, screen, within } from '@testing-library/react';
import { afterEach, beforeEach, expect, it, vi } from 'vitest';
import { MockAdapter } from '../../adapters/MockAdapter';
import { useChangeStore } from '../../store/changeStore';
import { SchemaDialog } from './SchemaDialog';

// 建表/删表/索引对话框测试：校验、直接响应与结构变更暂存。
afterEach(cleanup);
beforeEach(() => useChangeStore.getState().clear());

it('stages a create table change with explicit columns and rejects duplicate columns', () => {
  const onChanged = vi.fn();
  const onClose = vi.fn();
  render(<SchemaDialog action={{ kind: 'create' }} onClose={onClose} onChanged={onChanged} />);
  fireEvent.change(screen.getByLabelText('表名'), { target: { value: 'items' } });
  fireEvent.change(screen.getByLabelText('列名 2'), { target: { value: 'id' } });
  fireEvent.click(screen.getByRole('button', { name: '建表' }));
  expect(screen.getByRole('alert')).toHaveTextContent('列名重复');
  expect(useChangeStore.getState().changes.schemas.size).toBe(0);
  fireEvent.change(screen.getByLabelText('列名 2'), { target: { value: 'title' } });
  fireEvent.click(screen.getByRole('button', { name: '建表' }));
  expect(onChanged).toHaveBeenCalledWith('已暂存：CREATE TABLE items (id int, title str);');
  expect(onClose).toHaveBeenCalled();
  expect([...useChangeStore.getState().changes.schemas.values()]).toEqual([
    { op: 'createTable', table: 'items', columns: [{ name: 'id', type: 'int' }, { name: 'title', type: 'str' }] },
  ]);
});

it('stages a drop table change on the first click without asking for the name', async () => {
  const adapter = new MockAdapter({ latencyMs: 0 });
  const table = (await adapter.listTables('chusql'))[0];
  const onChanged = vi.fn();
  render(<SchemaDialog action={{ kind: 'drop', table }} onClose={vi.fn()} onChanged={onChanged} />);
  expect(screen.queryByLabelText(/输入/)).not.toBeInTheDocument();
  fireEvent.click(screen.getByRole('button', { name: '永久删除表' }));
  expect(onChanged).toHaveBeenCalledWith('已暂存：DROP TABLE users;');
  expect([...useChangeStore.getState().changes.schemas.values()]).toEqual([{ op: 'dropTable', table: 'users' }]);
  expect((await adapter.listTables('chusql')).some((item) => item.name === 'users')).toBe(true);
});

it('protects the built-in primary key index and non-integer columns', async () => {
  const adapter = new MockAdapter({ latencyMs: 0 });
  const table = (await adapter.listTables('chusql'))[0];
  render(<SchemaDialog action={{ kind: 'indexes', table }} onClose={vi.fn()} onChanged={vi.fn()} />);
  const rows = screen.getAllByRole('listitem');
  expect(within(rows[0]).getByRole('button')).toBeDisabled();
  expect(within(rows[1]).getByRole('button')).toBeDisabled();
  expect(within(rows[2]).getByRole('button')).toBeEnabled();
});

it('stages index creation and deletion on the first click and reflects the pending state', async () => {
  const adapter = new MockAdapter({ latencyMs: 0 });
  await adapter.createTable('items', [{ name: 'id', type: 'int' }, { name: 'sku', type: 'int' }]);
  const table = (await adapter.listTables('chusql')).find((item) => item.name === 'items')!;
  const onChanged = vi.fn();
  render(<SchemaDialog action={{ kind: 'indexes', table }} onClose={vi.fn()} onChanged={onChanged} />);
  expect(screen.getByText('无索引')).toBeInTheDocument();
  fireEvent.click(screen.getByRole('button', { name: '建索引' }));
  expect(screen.getByText('唯一索引')).toBeInTheDocument();
  expect(onChanged).toHaveBeenCalledWith('已暂存：CREATE INDEX ON items (sku);');
  fireEvent.click(within(screen.getAllByRole('listitem')[1]).getByRole('button', { name: '删索引' }));
  expect(screen.getByText('无索引')).toBeInTheDocument();
  expect([...useChangeStore.getState().changes.schemas.keys()]).toEqual(['createIndex:items:sku', 'dropIndex:items:sku']);
});

it('leaves nothing staged when the create dialog is cancelled', () => {
  const onClose = vi.fn();
  render(<SchemaDialog action={{ kind: 'create' }} onClose={onClose} onChanged={vi.fn()} />);
  fireEvent.change(screen.getByLabelText('表名'), { target: { value: 'items' } });
  fireEvent.click(screen.getByRole('button', { name: '取消' }));
  expect(onClose).toHaveBeenCalled();
  expect(useChangeStore.getState().changes.schemas.size).toBe(0);
});
