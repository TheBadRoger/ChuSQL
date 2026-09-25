import { expect, it, vi } from 'vitest';
import { restoreEditorLayout } from './editorLayout';

// editorLayout 测试：只恢复布局声明，不碰已生效样式。

it('restores only layout declarations from CSP-blocked editor markup', () => {
  const host = document.createElement('div');
  const line = document.createElement('div');
  line.setAttribute('style', 'left:20px;width:36px;background-image:url(https://example.invalid/x);');
  const setProperty = vi.fn();
  Object.defineProperty(line, 'style', { value: { length: 0, setProperty } });
  host.append(line);
  const stop = restoreEditorLayout(host);
  expect(setProperty.mock.calls).toEqual([['left', '20px'], ['width', '36px']]);
  stop();
});

it('leaves already active CSSOM styles untouched', () => {
  const host = document.createElement('div');
  host.style.width = '200px';
  const setProperty = vi.spyOn(host.style, 'setProperty');
  const stop = restoreEditorLayout(host);
  expect(setProperty).not.toHaveBeenCalled();
  stop();
});
