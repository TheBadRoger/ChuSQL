import { fireEvent, render, screen } from '@testing-library/react';
import { beforeEach, expect, it } from 'vitest';
import { SettingsPanel } from './SettingsPanel';
import { useSettingsStore } from '../../store/settingsStore';

// 覆盖设置面板的输入聚焦与数值提交。

beforeEach(() => useSettingsStore.getState().reset());
it('preserves the font input and focus while typing and deleting', () => {
  render(<SettingsPanel />);
  const input = screen.getAllByRole('textbox')[0];
  input.focus();
  fireEvent.change(input, { target: { value: 'Consola' } });
  expect(screen.getAllByRole('textbox')[0]).toBe(input);
  expect(input).toHaveFocus();
  fireEvent.change(input, { target: { value: '' } });
  expect(input).toHaveValue('');
  expect(input).toHaveFocus();
});
it('allows an empty numeric draft and commits a bounded value on blur', () => {
  render(<SettingsPanel />);
  const input = screen.getAllByRole('spinbutton')[0];
  fireEvent.change(input, { target: { value: '' } });
  expect(input).toHaveValue(null);
  expect(useSettingsStore.getState().settings.uiFontSize).toBe(13);
  fireEvent.change(input, { target: { value: '15' } });
  fireEvent.blur(input);
  expect(useSettingsStore.getState().settings.uiFontSize).toBe(15);
  fireEvent.change(input, { target: { value: '99' } });
  fireEvent.keyDown(input, { key: 'Enter' });
  expect(input).toHaveValue(16);
  fireEvent.change(input, { target: { value: '' } });
  fireEvent.blur(input);
  expect(input).toHaveValue(16);
});
