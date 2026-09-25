// 网格筛选表达式解析：比较、like 通配与纯文本匹配。

function escapeRegExp(value: string): string {
  return value.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

function likePattern(raw: string): RegExp {
  const escaped = escapeRegExp(raw).replaceAll('%', '.*').replaceAll('_', '.');
  return new RegExp(`^${escaped}$`, 'i');
}

export function matchesFilter(value: unknown, rawExpression: string): boolean {
  const expression = rawExpression.trim();
  if (!expression) return true;
  const normalized = expression.toLowerCase();

  if (normalized === 'null') return value === null || value === undefined;
  if (normalized === 'true' || normalized === 'false') return value === (normalized === 'true');

  const like = expression.match(/^like\s+(.+)$/i);
  if (like) return likePattern(like[1]).test(String(value ?? ''));

  const comparison = expression.match(/^(>=|<=|>|<|=|!=)\s*(-?\d+(?:\.\d+)?)$/);
  if (comparison) {
    const left = Number(value);
    const right = Number(comparison[2]);
    if (!Number.isFinite(left)) return false;
    switch (comparison[1]) {
      case '>': return left > right;
      case '<': return left < right;
      case '>=': return left >= right;
      case '<=': return left <= right;
      case '!=': return left !== right;
      default: return left === right;
    }
  }

  return String(value ?? '').toLocaleLowerCase().includes(normalized);
}
