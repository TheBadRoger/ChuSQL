import { describe, expect, it } from 'vitest';
import { parseArgs } from './args.js';

// 覆盖 CLI 参数解析与非法取值拒绝。

describe('parseArgs', () => {
  it('parses command-specific and global options', () => {
    expect(parseArgs(['web', '--port', '9000', '--no-open', '--adapter', 'rest'])).toMatchObject({
      command: 'web', port: 9000, autoOpenBrowser: false, adapter: 'rest',
    });
  });

  it('rejects unsafe or invalid values', () => {
    expect(() => parseArgs(['web', '--port', 'nope'])).toThrow('Port must be an integer');
    expect(() => parseArgs(['web', '--host', 'x & calc'])).toThrow('Host contains invalid characters');
    expect(() => parseArgs(['web', '--adapter', 'other'])).toThrow('Adapter must be mock or rest');
  });
});
