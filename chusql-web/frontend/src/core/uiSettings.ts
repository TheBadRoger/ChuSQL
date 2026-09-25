import { useSettingsStore, type IdeSettings } from '../store/settingsStore';

// 远端 IDE 设置的读取、校验与防抖回写。

const endpoint = '/api/ui-settings';
const debounceMs = 400;
const fontKeys = ['uiFonts', 'gridFonts', 'sqlFonts'] as const;
const booleanKeys = ['sqlLineNumbers', 'autocomplete', 'minimap'] as const;
const numberKeys = ['uiFontSize', 'gridFontSize', 'gridRowHeight', 'sqlFontSize', 'sqlLineHeight', 'sqlTabSize', 'pageSize'] as const;

type NumberKey = (typeof numberKeys)[number];

const numberRanges: Record<NumberKey, [number, number]> = {
  uiFontSize: [11, 16],
  gridFontSize: [10, 18],
  gridRowHeight: [18, 40],
  sqlFontSize: [10, 20],
  sqlLineHeight: [16, 40],
  sqlTabSize: [1, 8],
  pageSize: [10, 500],
};

// 收敛字体链：去空串并截断到 10 项。
function readFonts(value: unknown): string[] | undefined {
  if (!Array.isArray(value)) return undefined;
  return value.filter((font): font is string => typeof font === 'string' && font.trim().length > 0).slice(0, 10);
}

// 只取已知键，数值夹紧，类型不符即丢弃。
function parseUiSettings(body: unknown): Partial<IdeSettings> {
  const parsed: Partial<IdeSettings> = {};
  if (!body || typeof body !== 'object' || Array.isArray(body)) return parsed;
  const source = body as Record<string, unknown>;
  for (const key of fontKeys) {
    const fonts = readFonts(source[key]);
    if (fonts) parsed[key] = fonts;
  }
  for (const key of numberKeys) {
    const value = source[key];
    if (typeof value !== 'number' || !Number.isFinite(value)) continue;
    const [min, max] = numberRanges[key];
    parsed[key] = Math.min(max, Math.max(min, Math.round(value)));
  }
  for (const key of booleanKeys) {
    const value = source[key];
    if (typeof value === 'boolean') parsed[key] = value;
  }
  if (typeof source.nullText === 'string') parsed.nullText = source.nullText;
  return parsed;
}

// 读取远端设置，失败时返回 undefined。
export async function loadUiSettings(): Promise<Partial<IdeSettings> | undefined> {
  try {
    const response = await fetch(endpoint, { credentials: 'same-origin' });
    if (!response.ok) return undefined;
    const body: unknown = await response.json();
    if (!body || typeof body !== 'object' || Array.isArray(body)) return undefined;
    return parseUiSettings(body);
  } catch {
    return undefined;
  }
}

// 全量写回远端设置，失败时返回 false。
export async function saveUiSettings(settings: IdeSettings): Promise<boolean> {
  try {
    const response = await fetch(endpoint, {
      method: 'PUT',
      credentials: 'same-origin',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(settings),
    });
    return response.ok;
  } catch {
    return false;
  }
}

// 先并入远端设置，再防抖回写后续变更。
export function syncUiSettings(): () => void {
  let stopped = false;
  let unsubscribe: (() => void) | undefined;
  let timer: ReturnType<typeof setTimeout> | undefined;
  const cancel = () => { if (timer !== undefined) { clearTimeout(timer); timer = undefined; } };
  void (async () => {
    const remote = await loadUiSettings();
    if (stopped) return;
    if (remote) useSettingsStore.setState((state) => ({ settings: { ...state.settings, ...remote } }));
    unsubscribe = useSettingsStore.subscribe((state, previous) => {
      if (state.settings === previous.settings) return;
      cancel();
      timer = setTimeout(() => {
        timer = undefined;
        void saveUiSettings(useSettingsStore.getState().settings);
      }, debounceMs);
    });
  })();
  return () => {
    stopped = true;
    unsubscribe?.();
    cancel();
  };
}
