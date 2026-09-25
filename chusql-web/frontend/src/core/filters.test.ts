import { describe, expect, it } from 'vitest';
import { matchesFilter } from './filters';

// 覆盖筛选表达式的数值、通配与布尔匹配。

describe('matchesFilter', () => {
  it('supports numeric comparisons', () => {
    expect(matchesFilter(101, '>100')).toBe(true);
    expect(matchesFilter(100, '>100')).toBe(false);
  });

  it('supports SQL-like wildcard matching without regular-expression injection', () => {
    expect(matchesFilter('Alphabet', 'like %pha%')).toBe(true);
    expect(matchesFilter('[abc]', 'like %[abc]%')).toBe(true);
    expect(matchesFilter('aXXb', 'like a.b')).toBe(false);
  });

  it('matches booleans, null and plain text', () => {
    expect(matchesFilter(true, 'true')).toBe(true);
    expect(matchesFilter(null, 'null')).toBe(true);
    expect(matchesFilter('Alice', 'alice')).toBe(true);
  });
});
