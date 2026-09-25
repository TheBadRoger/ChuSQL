import { fireEvent, render, screen } from '@testing-library/react';
import { expect, it, vi } from 'vitest';
import { RowDialog } from './RowDialog';
// 插入行对话框测试：整数校验与暂存值。
it('requires an explicit integer key and stages typed values without generating an id', () => {
  const onInsert = vi.fn();
  render(<RowDialog table={{ name: 'users', rowCount: 500, columns: [{ name: 'id', type: 'int', primaryKey: true }, { name: 'name', type: 'str' }, { name: 'active', type: 'bool' }] }} onClose={vi.fn()} onInsert={onInsert} />);
  expect(screen.getByLabelText('id')).toHaveValue('');
  fireEvent.click(screen.getByRole('button', { name: '加入待提交' }));
  expect(onInsert).not.toHaveBeenCalled();
  expect(screen.getByRole('alert')).toHaveTextContent('id：请输入合法整数');
  fireEvent.change(screen.getByLabelText('id'), { target: { value: '800' } });
  fireEvent.change(screen.getByLabelText('name'), { target: { value: "Ada's" } });
  fireEvent.click(screen.getByRole('button', { name: '加入待提交' }));
  expect(onInsert).toHaveBeenCalledWith({ id: 800, name: "Ada's", active: false });
});
