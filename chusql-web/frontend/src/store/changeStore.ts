import { create } from 'zustand';
import { changeKey, cloneChangeSet, createEmptyChangeSet, pendingChanges, schemaKey } from '../core/changes';
import type { ChangeSet, SchemaChange } from '../types';

// 待提交变更状态：增删改记录、撤销重做与提交确认。

interface ChangeState {
  changes: ChangeSet;
  undoStack: ChangeSet[];
  redoStack: ChangeSet[];
  updateCell: (table: string, pk: unknown, column: string, oldValue: unknown, newValue: unknown) => void;
  insertRow: (table: string, tempId: string, values: Record<string, unknown>) => void;
  updateInsert: (tempId: string, values: Record<string, unknown>) => void;
  deleteRow: (table: string, pk: unknown, row: Record<string, unknown>, isInserted?: boolean) => void;
  stageSchema: (change: SchemaChange) => void;
  undoOne: (kind: keyof ChangeSet, key: string) => void;
  undo: () => void;
  redo: () => void;
  clear: () => void;
  acknowledge: (submitted: ChangeSet, applied: number) => void;
}

function withHistory(state: ChangeState, mutate: (next: ChangeSet) => void): Partial<ChangeState> {
  const next = cloneChangeSet(state.changes);
  mutate(next);
  return {
    changes: next,
    undoStack: [...state.undoStack, cloneChangeSet(state.changes)].slice(-100),
    redoStack: [],
  };
}

export const useChangeStore = create<ChangeState>((set) => ({
  changes: createEmptyChangeSet(),
  undoStack: [],
  redoStack: [],
  updateCell: (table, pk, column, oldValue, newValue) => set((state) => withHistory(state, (next) => {
    const key = changeKey(table, pk, column);
    if (Object.is(oldValue, newValue)) next.updates.delete(key);
    else next.updates.set(key, { table, pk, column, oldValue, newValue });
  })),
  insertRow: (table, tempId, values) => set((state) => withHistory(state, (next) => {
    next.inserts.set(tempId, { table, tempId, values: { ...values } });
  })),
  updateInsert: (tempId, values) => set((state) => withHistory(state, (next) => {
    const insert = next.inserts.get(tempId);
    if (insert) next.inserts.set(tempId, { ...insert, values: { ...values } });
  })),
  deleteRow: (table, pk, row, isInserted = false) => set((state) => withHistory(state, (next) => {
    if (isInserted) {
      next.inserts.delete(String(pk));
      return;
    }
    for (const [key, update] of next.updates) {
      if (update.table === table && Object.is(update.pk, pk)) next.updates.delete(key);
    }
    next.deletes.set(`${table}:${JSON.stringify(pk)}`, { table, pk, row: { ...row } });
  })),
  stageSchema: (change) => set((state) => withHistory(state, (next) => {
    next.schemas.set(schemaKey(change), change);
  })),
  undoOne: (kind, key) => set((state) => withHistory(state, (next) => {
    next[kind].delete(key);
  })),
  undo: () => set((state) => {
    const previous = state.undoStack.at(-1);
    if (!previous) return state;
    return {
      changes: cloneChangeSet(previous),
      undoStack: state.undoStack.slice(0, -1),
      redoStack: [...state.redoStack, cloneChangeSet(state.changes)].slice(-100),
    };
  }),
  redo: () => set((state) => {
    const next = state.redoStack.at(-1);
    if (!next) return state;
    return {
      changes: cloneChangeSet(next),
      undoStack: [...state.undoStack, cloneChangeSet(state.changes)].slice(-100),
      redoStack: state.redoStack.slice(0, -1),
    };
  }),
  clear: () => set({ changes: createEmptyChangeSet(), undoStack: [], redoStack: [] }),
  acknowledge: (submitted, applied) => set((state) => {
    const next = cloneChangeSet(state.changes);
    const completed = pendingChanges(submitted).slice(0, applied);
    const inserted = Array.from(submitted.inserts.entries());
    let insertIndex = 0;
    for (const operation of completed) {
      if (operation.op === 'insert') {
        const [key, value] = inserted[insertIndex++];
        if (next.inserts.get(key) === value) next.inserts.delete(key);
      } else if (operation.op === 'delete') {
        const key = `${operation.table}:${JSON.stringify(operation.pk)}`;
        if (next.deletes.get(key) === submitted.deletes.get(key)) next.deletes.delete(key);
      } else if (operation.op === 'update') {
        for (const column of Object.keys(operation.set)) {
          const key = changeKey(operation.table, operation.pk, column);
          if (next.updates.get(key) === submitted.updates.get(key)) next.updates.delete(key);
        }
      } else {
        const key = schemaKey(operation);
        if (next.schemas.get(key) === submitted.schemas.get(key)) next.schemas.delete(key);
      }
    }
    return { changes: next, undoStack: [], redoStack: [] };
  }),
}));
