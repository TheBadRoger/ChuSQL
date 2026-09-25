import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { DEFAULT_SETTINGS, useSettingsStore } from '../store/settingsStore';
import { loadUiSettings, saveUiSettings, syncUiSettings } from './uiSettings';

// 覆盖远端设置的校验读取、写回与防抖同步。

describe('uiSettings', () => {
  let stop: (() => void) | undefined;

  beforeEach(() => {
    localStorage.clear();
    useSettingsStore.getState().reset();
  });

  afterEach(() => {
    stop?.();
    stop = undefined;
    vi.unstubAllGlobals();
    localStorage.clear();
  });

  it('keeps known keys only and clamps the remote payload', async () => {
    const request = vi.fn().mockResolvedValue(new Response(JSON.stringify({
      uiFonts: ['', 'f1', 'f2', 'f3', 'f4', 'f5', 'f6', 'f7', 'f8', 'f9', 'f10', 'f11', '  '],
      gridFonts: 'Consolas',
      sqlFonts: ['Consolas'],
      uiFontSize: 99,
      gridFontSize: 9,
      gridRowHeight: 17.4,
      sqlFontSize: 13.5,
      sqlLineHeight: 60,
      sqlTabSize: 0,
      pageSize: 2,
      nullText: 'NULL',
      sqlLineNumbers: false,
      autocomplete: 'yes',
      minimap: true,
      confirmBeforeCommit: 1,
      unknownKey: 5,
    }), { status: 200 }));
    vi.stubGlobal('fetch', request);

    await expect(loadUiSettings()).resolves.toEqual({
      uiFonts: ['f1', 'f2', 'f3', 'f4', 'f5', 'f6', 'f7', 'f8', 'f9', 'f10'],
      sqlFonts: ['Consolas'],
      uiFontSize: 16,
      gridFontSize: 10,
      gridRowHeight: 18,
      sqlFontSize: 14,
      sqlLineHeight: 40,
      sqlTabSize: 1,
      pageSize: 10,
      nullText: 'NULL',
      sqlLineNumbers: false,
      minimap: true,
    });
    expect(request).toHaveBeenCalledWith('/api/ui-settings', { credentials: 'same-origin' });
  });

  it('returns undefined instead of throwing when the read fails', async () => {
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(new Response('', { status: 401 })));
    await expect(loadUiSettings()).resolves.toBeUndefined();

    vi.stubGlobal('fetch', vi.fn().mockRejectedValue(new Error('offline')));
    await expect(loadUiSettings()).resolves.toBeUndefined();

    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(new Response('not json', { status: 200 })));
    await expect(loadUiSettings()).resolves.toBeUndefined();

    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(new Response('[]', { status: 200 })));
    await expect(loadUiSettings()).resolves.toBeUndefined();
  });

  it('puts the whole settings object back to the endpoint', async () => {
    const request = vi.fn().mockResolvedValue(new Response(null, { status: 204 }));
    vi.stubGlobal('fetch', request);

    await expect(saveUiSettings(DEFAULT_SETTINGS)).resolves.toBe(true);
    expect(request).toHaveBeenCalledTimes(1);
    expect(request).toHaveBeenCalledWith('/api/ui-settings', {
      method: 'PUT',
      credentials: 'same-origin',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(DEFAULT_SETTINGS),
    });
  });

  it('reports a failed write as false', async () => {
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(new Response('', { status: 500 })));
    await expect(saveUiSettings(DEFAULT_SETTINGS)).resolves.toBe(false);

    vi.stubGlobal('fetch', vi.fn().mockRejectedValue(new Error('offline')));
    await expect(saveUiSettings(DEFAULT_SETTINGS)).resolves.toBe(false);
  });

  it('merges remote settings first and then debounces one write', async () => {
    const request = vi.fn()
      .mockResolvedValueOnce(new Response(JSON.stringify({ uiFonts: ['Consolas'], uiFontSize: 15, autocomplete: false }), { status: 200 }))
      .mockResolvedValue(new Response(null, { status: 204 }));
    vi.stubGlobal('fetch', request);
    stop = syncUiSettings();

    await vi.waitFor(() => expect(useSettingsStore.getState().settings.autocomplete).toBe(false));
    expect(useSettingsStore.getState().settings.uiFonts).toEqual(['Consolas']);
    expect(useSettingsStore.getState().settings.uiFontSize).toBe(15);
    expect(request).toHaveBeenCalledTimes(1);

    useSettingsStore.getState().setNumber('pageSize', 300);
    useSettingsStore.getState().setNumber('pageSize', 320);
    await vi.waitFor(() => expect(request).toHaveBeenCalledTimes(2), { timeout: 2000 });
    const [url, init] = request.mock.calls[1] as [string, RequestInit];
    expect(url).toBe('/api/ui-settings');
    expect(init.method).toBe('PUT');
    expect(JSON.parse(String(init.body)).pageSize).toBe(320);
  });
});
