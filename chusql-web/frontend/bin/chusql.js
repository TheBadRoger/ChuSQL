#!/usr/bin/env node
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { createServer } from 'vite';
import { parseArgs } from './lib/args.js';
import {
  defaultConfigPath, getAtPath, parseConfigValue, readConfig, setAtPath, unsetAtPath, writeConfig,
} from './lib/config.js';
import { openBrowser } from './lib/open.js';

// ChuSQL 前端 CLI：启动开发服务器并管理本地配置。

const cyan = (text) => `\u001b[36m${text}\u001b[0m`;
const green = (text) => `\u001b[32m${text}\u001b[0m`;
const red = (text) => `\u001b[31m${text}\u001b[0m`;

const help = `Usage: chusql <command> [options]

Commands:
  web                      Start the web server
  config list              List all configuration
  config get <key>         Get a configuration value
  config set <key> <val>   Set a configuration value
  config unset <key>       Remove a configuration value
  config path              Print the configuration file path
  help                     Show this help message

Global options:
  --port <n>               Specify port
  --host <h>               Specify host
  --no-open                Do not open the browser automatically
  --config <path>          Use a custom configuration file
  --adapter <mock|rest>    Specify the data source adapter
  --help, -h               Show help
  --version, -v            Show version

Run "chusql <command> --help" for command-specific options.`;

function configCommand(args, configPath) {
  const config = readConfig(configPath);
  const [key, rawValue] = args.operands;
  if (!args.action || args.action === 'list') console.log(JSON.stringify(config, null, 2));
  else if (args.action === 'path') console.log(configPath);
  else if (args.action === 'get') {
    if (!key) throw new Error('Usage: chusql config get <key>');
    const value = getAtPath(config, key);
    if (value === undefined) throw new Error(`Configuration key not found: ${key}`);
    console.log(typeof value === 'object' ? JSON.stringify(value, null, 2) : String(value));
  } else if (args.action === 'set') {
    if (!key || rawValue === undefined) throw new Error('Usage: chusql config set <key> <value>');
    setAtPath(config, key, parseConfigValue(rawValue));
    writeConfig(configPath, config);
    console.log(green(`Saved ${key}.`));
  } else if (args.action === 'unset') {
    if (!key) throw new Error('Usage: chusql config unset <key>');
    if (!unsetAtPath(config, key)) throw new Error(`Configuration key not found: ${key}`);
    writeConfig(configPath, config);
    console.log(green(`Removed ${key}.`));
  } else throw new Error(`Unknown config command: ${args.action}`);
}

async function webCommand(args, configPath) {
  const stored = readConfig(configPath);
  const config = {
    ...stored,
    ...(args.port === undefined ? {} : { port: args.port }),
    ...(args.host === undefined ? {} : { host: args.host }),
    ...(args.adapter === undefined ? {} : { adapter: args.adapter }),
    ...(args.autoOpenBrowser === undefined ? {} : { autoOpenBrowser: args.autoOpenBrowser }),
  };
  if (!Number.isInteger(config.port) || config.port < 1 || config.port > 65535) throw new Error('Configured port must be an integer between 1 and 65535.');
  if (!['mock', 'rest'].includes(config.adapter)) throw new Error('Configured adapter must be mock or rest.');
  process.env.VITE_CHUSQL_ADAPTER = config.adapter;
  const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
  const server = await createServer({ root, server: { host: config.host, port: config.port, strictPort: true } });
  await server.listen();
  const url = `http://${config.host}:${config.port}/`;
  console.log(`${cyan('➜')}  ${green('ChuSQL Web is running at:')}`);
  console.log(`   ${url}`);
  console.log('Press Ctrl+C to stop.');
  if (config.autoOpenBrowser) openBrowser(url);
}

try {
  const args = parseArgs(process.argv.slice(2));
  const configPath = path.resolve(args.configPath ?? defaultConfigPath());
  if (args.help || args.command === 'help') console.log(help);
  else if (args.command === 'version') console.log('ChuSQL 1.0.0');
  else if (args.command === 'config') configCommand(args, configPath);
  else if (args.command === 'web') await webCommand(args, configPath);
  else throw new Error(`Unknown command: ${args.command}`);
} catch (error) {
  console.error(red(`Error: ${error instanceof Error ? error.message : String(error)}`));
  process.exitCode = 1;
}
