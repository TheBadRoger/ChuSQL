import { beforeEach, describe, expect, it } from 'vitest';
import { DEFAULT_SETTINGS, fontFamilyValue, useSettingsStore } from './settingsStore';

// 覆盖字体链转义与数值范围收敛。

describe('settingsStore', () => {
  beforeEach(() => {
    localStorage.clear();
    useSettingsStore.getState().reset();
  });

  it('quotes a user-defined fallback chain safely', () => {
    expect(fontFamilyValue(['JetBrains Mono', 'Consolas'])).toBe('"JetBrains Mono", "Consolas", monospace');
    expect(fontFamilyValue(['A"B', '  '])).toBe('"A\\"B", monospace');
  });

  it('clamps numeric settings to the supported range', () => {
    useSettingsStore.getState().setNumber('gridRowHeight', 200);
    expect(useSettingsStore.getState().settings.gridRowHeight).toBe(40);
    useSettingsStore.getState().setNumber('pageSize', 1);
    expect(useSettingsStore.getState().settings.pageSize).toBe(10);
    expect(DEFAULT_SETTINGS.uiFontSize).toBe(13);
  });
});
