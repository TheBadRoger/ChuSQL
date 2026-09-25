import { afterEach, describe, expect, it, vi } from 'vitest';
import { RestAdapter } from './RestAdapter';

// RestAdapter 测试：接口路径编码、响应归一化与错误处理。

describe('RestAdapter', () => {
  afterEach(() => vi.unstubAllGlobals());
  it('uses structured schema endpoints and encodes identifiers in paths', async () => {
    const request = vi.fn().mockImplementation(async () => new Response('{}', { status: 200 }));
    vi.stubGlobal('fetch', request);
    const adapter = new RestAdapter();
    await adapter.createTable('items', [{ name: 'id', type: 'int' }]);
    await adapter.createIndex('items', 'sku');
    await adapter.dropIndex('items', 'sku');
    await adapter.dropTable('a/b');
    expect(request.mock.calls.map(([path, init]) => [path, init.method, init.body])).toEqual([
      ['/api/tables', 'POST', JSON.stringify({ name: 'items', columns: [{ name: 'id', type: 'int' }] })],
      ['/api/tables/items/indexes', 'POST', JSON.stringify({ column: 'sku' })],
      ['/api/tables/items/indexes/sku', 'DELETE', undefined],
      ['/api/tables/a%2Fb', 'DELETE', undefined],
    ]);
  });

  it('normalizes the existing Haskell catalog response', async () => {
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(new Response(JSON.stringify([{
      table: 'users', rowCount: 2,
      columns: [{ name: 'id', type: 'int', primaryKey: true, indexed: true }],
      indexes: [{ column: 'id', builtIn: true }],
    }]), { status: 200, headers: { 'Content-Type': 'application/json' } })));
    const tables = await new RestAdapter().listTables('chusql');
    expect(tables).toEqual([{ name: 'users', rowCount: 2, columns: [{ name: 'id', type: 'int', primaryKey: true, indexed: true }] }]);
  });

  it('maps pages, sorting and filters to the existing rows endpoint', async () => {
    const request = vi.fn().mockResolvedValue(new Response(JSON.stringify({
      columns: ['id', 'name'], rows: [[1, 'Ada']], total: 1,
    }), { status: 200, headers: { 'Content-Type': 'application/json' } }));
    vi.stubGlobal('fetch', request);
    const result = await new RestAdapter().fetchRows({
      db: 'chusql', table: 'users', page: 2, pageSize: 20,
      sort: [{ column: 'name', dir: 'desc' }], filters: [{ column: 'name', expression: 'Ada' }],
    });
    expect(result).toEqual({ rows: [{ id: 1, name: 'Ada' }], total: 1 });
    expect(String(request.mock.calls[0][0])).toContain('/api/tables/users/rows?');
    expect(String(request.mock.calls[0][0])).toContain('offset=40');
    expect(String(request.mock.calls[0][0])).toContain('sort=name');
  });

  it('turns non-JSON and structured API failures into safe errors', async () => {
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(new Response('<stack trace>', { status: 500 })));
    await expect(new RestAdapter().listTables('chusql')).rejects.toThrow('Request failed with status 500');
  });
});
