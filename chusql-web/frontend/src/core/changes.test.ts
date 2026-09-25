import { describe, expect, it } from 'vitest';
import { changeCount, createEmptyChangeSet, pendingChanges, toCommitPayload } from './changes';

// 覆盖提交载荷的合并、顺序与结构变更。

describe('toCommitPayload', () => {
  it('coalesces cell updates into one row update', () => {
    const changes = createEmptyChangeSet();
    changes.updates.set('users:1:name', { table: 'users', pk: 1, column: 'name', oldValue: 'A', newValue: 'B' });
    changes.updates.set('users:1:age', { table: 'users', pk: 1, column: 'age', oldValue: 20, newValue: 21 });
    expect(toCommitPayload(changes)).toEqual([
      { op: 'update', table: 'users', pk: 1, set: { name: 'B', age: 21 } },
    ]);
  });

  it('keeps inserts and deletes in a stable order', () => {
    const changes = createEmptyChangeSet();
    changes.inserts.set('tmp-1', { table: 'users', tempId: 'tmp-1', values: { id: 9, name: 'N' } });
    changes.deletes.set('users:2', { table: 'users', pk: 2, row: { id: 2, name: 'D' } });
    expect(toCommitPayload(changes)).toEqual([
      { op: 'insert', table: 'users', values: { id: 9, name: 'N' } },
      { op: 'delete', table: 'users', pk: 2 },
    ]);
  });

  it('leaves schema changes out of the row payload', () => {
    const changes = createEmptyChangeSet();
    changes.schemas.set('createIndex:users:age', { op: 'createIndex', table: 'users', column: 'age' });
    expect(toCommitPayload(changes)).toEqual([]);
  });
});

describe('pendingChanges', () => {
  it('puts creates first, then row changes, then drops', () => {
    const changes = createEmptyChangeSet();
    changes.schemas.set('dropTable:items', { op: 'dropTable', table: 'items' });
    changes.schemas.set('createTable:items', { op: 'createTable', table: 'items', columns: [{ name: 'id', type: 'int' }] });
    changes.updates.set('items:1:name', { table: 'items', pk: 1, column: 'name', oldValue: 'A', newValue: 'B' });
    expect(pendingChanges(changes)).toEqual([
      { op: 'createTable', table: 'items', columns: [{ name: 'id', type: 'int' }] },
      { op: 'update', table: 'items', pk: 1, set: { name: 'B' } },
      { op: 'dropTable', table: 'items' },
    ]);
  });

  it('counts structural changes together with row changes', () => {
    const changes = createEmptyChangeSet();
    changes.schemas.set('dropTable:items', { op: 'dropTable', table: 'items' });
    changes.schemas.set('createIndex:users:age', { op: 'createIndex', table: 'users', column: 'age' });
    changes.updates.set('users:1:name', { table: 'users', pk: 1, column: 'name', oldValue: 'A', newValue: 'B' });
    expect(changeCount(changes)).toBe(3);
  });
});
