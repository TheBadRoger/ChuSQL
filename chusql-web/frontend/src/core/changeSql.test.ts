import { describe, expect, it } from 'vitest';
import { createEmptyChangeSet } from './changes';
import { changeSql, changesSql, deleteSql, insertSql, schemaSql, sqlIdentifier, sqlLiteral, updateSql } from './changeSql';

// 覆盖展示用 SQL 的字面量、标识符与结构语句。

describe('sql literals and identifiers', () => {
  it('doubles single quotes and keeps null, numbers and booleans bare', () => {
    expect(sqlLiteral("O'Brien")).toBe("'O''Brien'");
    expect(sqlLiteral(7)).toBe('7');
    expect(sqlLiteral(false)).toBe('false');
    expect(sqlLiteral(null)).toBe('NULL');
    expect(sqlLiteral(undefined)).toBe('NULL');
  });

  it('quotes identifiers only when they leave the whitelist', () => {
    expect(sqlIdentifier('users')).toBe('users');
    expect(sqlIdentifier('we"ird')).toBe('"we""ird"');
  });
});

describe('row statements', () => {
  it('renders update, insert and delete', () => {
    expect(updateSql('users', 1, 'name', "A'B")).toBe("UPDATE users SET name = 'A''B' WHERE id = 1;");
    expect(insertSql('users', { id: 9, name: 'Ada' })).toBe("INSERT INTO users (id, name) VALUES (9, 'Ada');");
    expect(deleteSql('users', 2)).toBe('DELETE FROM users WHERE id = 2;');
  });
});

describe('schema statements', () => {
  it('matches the statements the server Actions build', () => {
    expect(schemaSql({ op: 'createTable', table: 'items', columns: [{ name: 'id', type: 'int' }, { name: 'title', type: 'str' }] })).toBe('CREATE TABLE items (id int, title str);');
    expect(schemaSql({ op: 'dropTable', table: 'items' })).toBe('DROP TABLE items;');
    expect(schemaSql({ op: 'createIndex', table: 'items', column: 'sku' })).toBe('CREATE INDEX ON items (sku);');
    expect(schemaSql({ op: 'dropIndex', table: 'items', column: 'sku' })).toBe('DROP INDEX ON items (sku);');
  });
});

describe('changesSql', () => {
  it('lists creates first, then row changes, then drops', () => {
    const changes = createEmptyChangeSet();
    changes.schemas.set('dropTable:items', { op: 'dropTable', table: 'items' });
    changes.schemas.set('createTable:items', { op: 'createTable', table: 'items', columns: [{ name: 'id', type: 'int' }] });
    changes.updates.set('items:1:name', { table: 'items', pk: 1, column: 'name', oldValue: 'A', newValue: 'B' });
    expect(changesSql(changes)).toEqual([
      'CREATE TABLE items (id int);',
      "UPDATE items SET name = 'B' WHERE id = 1;",
      'DROP TABLE items;',
    ]);
  });

  it('renders a single pending change through changeSql', () => {
    expect(changeSql({ op: 'insert', table: 'users', values: { id: 3 } })).toBe('INSERT INTO users (id) VALUES (3);');
  });
});
