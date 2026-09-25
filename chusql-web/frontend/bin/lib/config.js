import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';

// CLI 配置文件读写：默认值、点号路径访问与类型解析。

export const DEFAULT_CONFIG = Object.freeze({
  port: 5173,
  host: '127.0.0.1',
  autoOpenBrowser: true,
  adapter: 'mock',
});

export function defaultConfigPath() {
  return path.join(os.homedir(), '.chusql', 'config.json');
}

export function readConfig(file = defaultConfigPath()) {
  if (!fs.existsSync(file)) {
    writeConfig(file, DEFAULT_CONFIG);
    return { ...DEFAULT_CONFIG };
  }
  let parsed;
  try {
    parsed = JSON.parse(fs.readFileSync(file, 'utf8'));
  } catch {
    throw new Error(`Configuration file is not valid JSON: ${file}`);
  }
  if (!parsed || typeof parsed !== 'object' || Array.isArray(parsed)) throw new Error(`Configuration must be a JSON object: ${file}`);
  return { ...DEFAULT_CONFIG, ...parsed };
}

export function writeConfig(file, value) {
  fs.mkdirSync(path.dirname(path.resolve(file)), { recursive: true });
  fs.writeFileSync(file, `${JSON.stringify(value, null, 2)}\n`, 'utf8');
}

export function parseConfigValue(raw) {
  if (/^-?\d+(?:\.\d+)?$/.test(raw)) return Number(raw);
  if (raw === 'true') return true;
  if (raw === 'false') return false;
  return raw;
}

const FORBIDDEN_PATH_PARTS = new Set(['__proto__', 'prototype', 'constructor']);

function configPathParts(dottedPath) {
  const parts = dottedPath.split('.').filter(Boolean);
  if (!parts.length) throw new Error('Configuration key must not be empty.');
  if (parts.some((part) => FORBIDDEN_PATH_PARTS.has(part))) throw new Error('Configuration key contains a forbidden path segment.');
  return parts;
}

export function getAtPath(value, dottedPath) {
  return configPathParts(dottedPath).reduce((current, part) => current && typeof current === 'object' ? current[part] : undefined, value);
}

export function setAtPath(value, dottedPath, nextValue) {
  const parts = configPathParts(dottedPath);
  let cursor = value;
  for (const part of parts.slice(0, -1)) {
    if (!cursor[part] || typeof cursor[part] !== 'object' || Array.isArray(cursor[part])) cursor[part] = {};
    cursor = cursor[part];
  }
  cursor[parts.at(-1)] = nextValue;
}

export function unsetAtPath(value, dottedPath) {
  const parts = configPathParts(dottedPath);
  let cursor = value;
  for (const part of parts.slice(0, -1)) {
    if (!cursor[part] || typeof cursor[part] !== 'object') return false;
    cursor = cursor[part];
  }
  return parts.length ? delete cursor[parts.at(-1)] : false;
}
