import { describe, expect, it } from 'vitest';
import { MockAdapter } from './MockAdapter';

// MockAdapter 测试：列表行数、排序筛选与变更集提交。

describe('MockAdapter', () => {
  it('ships users and orders with 500 rows each', async () => {
    const adapter = new MockAdapter({ latencyMs: 0 });
    const tables = await adapter.listTables('chusql');
    expect(tables.map((table) => [table.name, table.rowCount])).toEqual([
      ['users', 500],
      ['orders', 500],
    ]);
  });

  it('sorts and filters before paginating', async () => {
    const adapter = new MockAdapter({ latencyMs: 0 });
    const result = await adapter.fetchRows({
      db: 'chusql', table: 'users', page: 0, pageSize: 3,
      sort: [{ column: 'age', dir: 'desc' }], filters: [{ column: 'age', expression: '>60' }],
    });
    expect(result.total).toBeGreaterThan(3);
    expect(result.rows).toHaveLength(3);
    expect(Number(result.rows[0].age)).toBeGreaterThanOrEqual(Number(result.rows[1].age));
  });

  it('applies a complete change set in one call', async () => {
    const adapter = new MockAdapter({ latencyMs: 0 });
    const response = await adapter.commitChanges({
      db: 'chusql',
      changes: [
        { op: 'update', table: 'users', pk: 1, set: { name: 'edited' } },
        { op: 'delete', table: 'users', pk: 2 },
      ],
    });
    expect(response).toEqual({ ok: true, applied: 2 });
    const page = await adapter.fetchRows({ db: 'chusql', table: 'users', page: 0, pageSize: 3, sort: [], filters: [] });
    expect(page.rows[0].name).toBe('edited');
    expect(page.rows.some((row) => row.id === 2)).toBe(false);
  });

  it('refuses an index on a column that holds duplicate values', async () => {
    const adapter = new MockAdapter({ latencyMs: 0 });
    await expect(adapter.createIndex('users', 'age')).rejects.toThrow('duplicate values');
    const users = (await adapter.listTables('chusql')).find((table) => table.name === 'users')!;
    expect(users.columns.find((column) => column.name === 'age')?.indexed).toBeFalsy();
  });
});
