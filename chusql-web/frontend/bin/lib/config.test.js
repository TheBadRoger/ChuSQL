import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { afterEach, describe, expect, it } from 'vitest';
import { DEFAULT_CONFIG, getAtPath, parseConfigValue, readConfig, setAtPath, unsetAtPath, writeConfig } from './config.js';

// 覆盖 CLI 配置读写、取值解析与路径安全。

const temporaryDirectories = [];

afterEach(() => {
  for (const directory of temporaryDirectories.splice(0)) fs.rmSync(directory, { recursive: true, force: true });
});

function temporaryConfig() {
  const directory = fs.mkdtempSync(path.join(os.tmpdir(), 'chusql-cli-'));
  temporaryDirectories.push(directory);
  return path.join(directory, 'nested', 'config.json');
}

describe('CLI configuration', () => {
  it('creates the formatted default configuration on first read', () => {
    const file = temporaryConfig();
    expect(readConfig(file)).toEqual(DEFAULT_CONFIG);
    expect(fs.readFileSync(file, 'utf8')).toBe(`${JSON.stringify(DEFAULT_CONFIG, null, 2)}\n`);
  });

  it('persists dotted paths and can remove them', () => {
    const file = temporaryConfig();
    const config = readConfig(file);
    setAtPath(config, 'db.host', 'localhost');
    writeConfig(file, config);
    expect(getAtPath(readConfig(file), 'db.host')).toBe('localhost');
    expect(unsetAtPath(config, 'db.host')).toBe(true);
    expect(getAtPath(config, 'db.host')).toBeUndefined();
  });

  it('parses numbers and booleans without changing ordinary text', () => {
    expect(parseConfigValue('5173')).toBe(5173);
    expect(parseConfigValue('true')).toBe(true);
    expect(parseConfigValue('rest')).toBe('rest');
  });

  it('rejects prototype-polluting configuration paths', () => {
    const config = {};
    expect(() => setAtPath(config, '__proto__.polluted', true)).toThrow('forbidden path segment');
    expect({}.polluted).toBeUndefined();
  });
});
