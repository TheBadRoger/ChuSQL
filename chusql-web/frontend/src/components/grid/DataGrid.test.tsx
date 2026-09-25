import { fireEvent, render, screen } from '@testing-library/react';
import { describe, expect, it, vi } from 'vitest';
import { DataGrid } from './DataGrid';

// 数据网格测试：表头排序、筛选弹层、行选中模式与列宽调整。
describe('DataGrid column controls', () => {
  it('keeps row selection out of the grid columns and exposes header sorting and filtering', () => {
    const onFilter = vi.fn();
    const onSort = vi.fn();
    const onSelectRow = vi.fn();
    const onWidth = vi.fn();

    render(<DataGrid
      table="users"
      columns={[{ name: 'id', type: 'int', primaryKey: true }, { name: 'name', type: 'str' }]}
      rows={[{ id: 1, name: 'Ada' }]}
      changes={{ updates: new Map(), inserts: new Map(), deletes: new Map(), schemas: new Map() }}
      nullText="NULL"
      filters={{}}
      widths={{}}
      selectedRows={new Set()}
      onSort={onSort}
      onFilter={onFilter}
      onWidth={onWidth}
      onSelectRow={onSelectRow}
      onEdit={vi.fn()}
      onDelete={vi.fn()}
    />);

    expect(screen.queryByRole('checkbox')).not.toBeInTheDocument();
    expect(screen.queryByText('#')).not.toBeInTheDocument();
    expect(screen.queryByRole('textbox', { name: '筛选 name' })).not.toBeInTheDocument();

    fireEvent.click(screen.getByRole('button', { name: 'name' }));
    expect(onSort).toHaveBeenCalledWith('name');
    fireEvent.click(screen.getByRole('button', { name: '筛选 name' }));
    fireEvent.change(screen.getByRole('textbox', { name: '筛选 name' }), { target: { value: 'Ada' } });
    expect(onFilter).toHaveBeenCalledWith('name', 'Ada');

    fireEvent.click(screen.getByText('Ada'));
    expect(onSelectRow).toHaveBeenCalledWith(0, 'single');
    fireEvent.click(screen.getByText('Ada'), { ctrlKey: true });
    expect(onSelectRow).toHaveBeenCalledWith(0, 'toggle');
    fireEvent.click(screen.getByText('Ada'), { shiftKey: true });
    expect(onSelectRow).toHaveBeenCalledWith(0, 'range');
    fireEvent.keyDown(screen.getByText('Ada'), { key: ' ' });
    expect(onSelectRow).toHaveBeenCalledTimes(4);
    fireEvent.keyDown(screen.getByRole('separator', { name: '调整 name 列宽' }), { key: 'ArrowRight' });
    expect(onWidth).toHaveBeenCalledWith('name', 150);
    expect(screen.getByRole('table')).toHaveStyle({ width: '280px' });
    fireEvent.keyDown(screen.getByRole('textbox', { name: '筛选 name' }), { key: 'Escape' });
    expect(screen.queryByRole('textbox', { name: '筛选 name' })).not.toBeInTheDocument();
  });
});
