import { beforeEach, describe, expect, it } from 'vitest';
import { useChangeStore } from './changeStore';

// 覆盖变更记录、结构变更暂存与部分提交确认。

describe('changeStore', () => {
  beforeEach(() => useChangeStore.getState().clear());
  it('removes successful inserts after a partial commit so retries cannot duplicate them', () => {
    const store = useChangeStore.getState();
    store.insertRow('users', 'temp-a', { id: 901 });
    store.insertRow('users', 'temp-b', { id: 902 });
    const submitted = useChangeStore.getState().changes;
    store.acknowledge(submitted, 1);
    expect([...useChangeStore.getState().changes.inserts.keys()]).toEqual(['temp-b']);
    store.undo();
    expect([...useChangeStore.getState().changes.inserts.keys()]).toEqual(['temp-b']);
  });

  it('records edits without writing through and supports undo/redo', () => {
    const store = useChangeStore.getState();
    store.updateCell('users', 1, 'name', 'A', 'B');
    expect(useChangeStore.getState().changes.updates.size).toBe(1);
    useChangeStore.getState().undo();
    expect(useChangeStore.getState().changes.updates.size).toBe(0);
    useChangeStore.getState().redo();
    expect(useChangeStore.getState().changes.updates.get('users:1:name')?.newValue).toBe('B');
  });

  it('removes a new row instead of staging a delete for it', () => {
    const store = useChangeStore.getState();
    store.insertRow('users', 'temp-1', { name: 'N' });
    useChangeStore.getState().deleteRow('users', 'temp-1', { name: 'N' }, true);
    expect(useChangeStore.getState().changes.inserts.size).toBe(0);
    expect(useChangeStore.getState().changes.deletes.size).toBe(0);
  });

  it('stages structural changes and can undo a single one', () => {
    const store = useChangeStore.getState();
    store.stageSchema({ op: 'createIndex', table: 'users', column: 'age' });
    store.stageSchema({ op: 'dropTable', table: 'items' });
    expect([...useChangeStore.getState().changes.schemas.keys()]).toEqual(['createIndex:users:age', 'dropTable:items']);
    useChangeStore.getState().undoOne('schemas', 'createIndex:users:age');
    expect([...useChangeStore.getState().changes.schemas.keys()]).toEqual(['dropTable:items']);
    useChangeStore.getState().undo();
    expect([...useChangeStore.getState().changes.schemas.keys()]).toEqual(['createIndex:users:age', 'dropTable:items']);
  });

  it('keeps unapplied changes when the commit stops after a structural change', () => {
    const store = useChangeStore.getState();
    store.stageSchema({ op: 'createTable', table: 'items', columns: [{ name: 'id', type: 'int' }] });
    store.insertRow('users', 'temp-a', { id: 901 });
    store.acknowledge(useChangeStore.getState().changes, 1);
    expect(useChangeStore.getState().changes.schemas.size).toBe(0);
    expect([...useChangeStore.getState().changes.inserts.keys()]).toEqual(['temp-a']);
  });

  it('drops a structural change once the whole queue is acknowledged', () => {
    const store = useChangeStore.getState();
    store.insertRow('users', 'temp-a', { id: 901 });
    store.stageSchema({ op: 'dropTable', table: 'items' });
    store.acknowledge(useChangeStore.getState().changes, 2);
    expect(useChangeStore.getState().changes.schemas.size).toBe(0);
    expect(useChangeStore.getState().changes.inserts.size).toBe(0);
  });
});
