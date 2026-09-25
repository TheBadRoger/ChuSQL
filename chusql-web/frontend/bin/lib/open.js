import { spawn } from 'node:child_process';

// 用系统默认浏览器打开本地地址，并校验 URL 合法性。

export function openBrowser(url) {
  if (!/^http:\/\/[A-Za-z0-9.:[\]-]+:\d+\/$/.test(url)) throw new Error('Refusing to open an invalid local URL.');
  const command = process.platform === 'win32' ? 'cmd.exe' : process.platform === 'darwin' ? 'open' : 'xdg-open';
  const args = process.platform === 'win32' ? ['/d', '/s', '/c', 'start', '', url] : [url];
  const child = spawn(command, args, { detached: true, stdio: 'ignore', windowsHide: true });
  child.unref();
}
