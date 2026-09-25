// 命令行参数解析：命令、动作、全局选项与取值校验。

const HOST_PATTERN = /^[A-Za-z0-9.:[\]-]+$/;

function valueAfter(argv, index, label) {
  const value = argv[index + 1];
  if (!value || value.startsWith('--')) throw new Error(`${label} requires a value.`);
  return value;
}

export function parseArgs(argv) {
  const result = { command: 'help', action: '', operands: [], autoOpenBrowser: undefined };
  let index = 0;
  if (argv[index] && !argv[index].startsWith('-')) {
    result.command = argv[index];
    index += 1;
  } else if (argv.length) {
    result.command = argv[0] === '--version' || argv[0] === '-v' ? 'version' : 'help';
  }
  if (result.command === 'config' && argv[index] && !argv[index].startsWith('-')) {
    result.action = argv[index];
    index += 1;
  }
  while (index < argv.length) {
    const token = argv[index];
    if (token === '--help' || token === '-h') result.help = true;
    else if (token === '--no-open') result.autoOpenBrowser = false;
    else if (token === '--port') {
      const raw = valueAfter(argv, index, 'Port');
      if (!/^\d+$/.test(raw) || Number(raw) < 1 || Number(raw) > 65535) throw new Error('Port must be an integer between 1 and 65535.');
      result.port = Number(raw);
      index += 1;
    } else if (token === '--host') {
      const host = valueAfter(argv, index, 'Host');
      if (!HOST_PATTERN.test(host)) throw new Error('Host contains invalid characters.');
      result.host = host;
      index += 1;
    } else if (token === '--adapter') {
      const adapter = valueAfter(argv, index, 'Adapter');
      if (adapter !== 'mock' && adapter !== 'rest') throw new Error('Adapter must be mock or rest.');
      result.adapter = adapter;
      index += 1;
    } else if (token === '--config') {
      result.configPath = valueAfter(argv, index, 'Config');
      index += 1;
    } else if (token.startsWith('-')) {
      throw new Error(`Unknown option: ${token}`);
    } else {
      result.operands.push(token);
    }
    index += 1;
  }
  return result;
}
