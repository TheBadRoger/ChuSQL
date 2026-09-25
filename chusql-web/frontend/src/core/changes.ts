import type { ChangeSet, CommitChange, PendingChange, SchemaChange } from '../types';

// 变更集工具：创建、克隆、计数与提交载荷组装。

// 建一个空的变更集。
export function createEmptyChangeSet(): ChangeSet {
  return { updates: new Map(), inserts: new Map(), deletes: new Map(), schemas: new Map() };
}

// 深拷贝变更集，供撤销栈留底。
export function cloneChangeSet(changes: ChangeSet): ChangeSet {
  return {
    updates: new Map(changes.updates),
    inserts: new Map(changes.inserts),
    deletes: new Map(changes.deletes),
    schemas: new Map(changes.schemas),
  };
}

// 统计待提交项，含结构变更。
export function changeCount(changes: ChangeSet): number {
  return changes.updates.size + changes.inserts.size + changes.deletes.size + changes.schemas.size;
}

// 结构变更的稳定标识，撤销与确认共用。
export function schemaKey(change: SchemaChange): string {
  return change.op === 'createIndex' || change.op === 'dropIndex'
    ? `${change.op}:${change.table}:${change.column}`
    : `${change.op}:${change.table}`;
}

// 组装 DML 提交载荷：同行的多列修改并成一条。
export function toCommitPayload(changes: ChangeSet): CommitChange[] {
  const updates = new Map<string, Extract<CommitChange, { op: 'update' }>>();
  for (const update of changes.updates.values()) {
    const key = `${update.table}:${JSON.stringify(update.pk)}`;
    const existing = updates.get(key);
    if (existing) {
      existing.set[update.column] = update.newValue;
    } else {
      updates.set(key, {
        op: 'update',
        table: update.table,
        pk: update.pk,
        set: { [update.column]: update.newValue },
      });
    }
  }

  return [
    ...updates.values(),
    ...Array.from(changes.inserts.values(), (insert): CommitChange => ({
      op: 'insert', table: insert.table, values: { ...insert.values },
    })),
    ...Array.from(changes.deletes.values(), (deleted): CommitChange => ({
      op: 'delete', table: deleted.table, pk: deleted.pk,
    })),
  ];
}

// 按执行顺序合并结构变更与 DML 变更。
export function pendingChanges(changes: ChangeSet): PendingChange[] {
  const schemas = Array.from(changes.schemas.values());
  const creates = schemas.filter((change) => change.op === 'createTable' || change.op === 'createIndex');
  const drops = schemas.filter((change) => change.op === 'dropIndex' || change.op === 'dropTable');
  return [...creates, ...toCommitPayload(changes), ...drops];
}

// 单元格变更的键，表 + 主键 + 列。
export function changeKey(table: string, pk: unknown, column: string): string {
  return `${table}:${JSON.stringify(pk)}:${column}`;
}
