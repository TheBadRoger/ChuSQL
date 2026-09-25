import { fireEvent, render, screen } from '@testing-library/react';
import { expect, it, vi } from 'vitest';
import { Explorer } from './Explorer';

// 资源管理器测试：列图标、展开收起与打开表。
it('expands columns with distinct primary key, index and ordinary column icons', () => {
  const onOpenTable = vi.fn();
  render(<Explorer tables={[{ name: 'users', rowCount: 1, columns: [
    { name: 'id', type: 'int', primaryKey: true },
    { name: 'email', type: 'str', indexed: true },
    { name: 'name', type: 'str' },
  ] }]} history={[]} onOpenTable={onOpenTable} onOpenSql={vi.fn()} />);
  const expand = screen.getByRole('button', { name: '展开 users 的列' });
  expect(screen.queryByText('email')).not.toBeInTheDocument();
  fireEvent.click(expand);
  expect(expand).toHaveAttribute('aria-expanded', 'true');
  for (const name of ['主键列', '索引列', '普通列']) expect(screen.getByRole('img', { name })).toBeInTheDocument();
  expect(onOpenTable).not.toHaveBeenCalled();
  fireEvent.click(screen.getByRole('button', { name: '打开表 users' }));
  expect(onOpenTable).toHaveBeenCalledWith('users');
  fireEvent.click(expand);
  expect(screen.queryByText('email')).not.toBeInTheDocument();
});
