import { create } from 'zustand';
import { persist } from 'zustand/middleware';

// IDE 设置状态：默认值、范围收敛与本地持久化。

export interface IdeSettings {
  uiFonts: string[];
  uiFontSize: number;
  gridFonts: string[];
  gridFontSize: number;
  gridRowHeight: number;
  sqlFonts: string[];
  sqlFontSize: number;
  sqlLineHeight: number;
  sqlTabSize: number;
  sqlLineNumbers: boolean;
  pageSize: number;
  nullText: string;
  autocomplete: boolean;
  minimap: boolean;
}

export const DEFAULT_SETTINGS: IdeSettings = Object.freeze({
  uiFonts: ['JetBrains Mono', 'Consolas', 'Cascadia Code'],
  uiFontSize: 13,
  gridFonts: ['JetBrains Mono', 'Consolas'],
  gridFontSize: 12,
  gridRowHeight: 24,
  sqlFonts: ['JetBrains Mono', 'Consolas'],
  sqlFontSize: 13,
  sqlLineHeight: 20,
  sqlTabSize: 2,
  sqlLineNumbers: true,
  pageSize: 200,
  nullText: 'NULL',
  autocomplete: true,
  minimap: false,
});

const ranges: Record<string, [number, number]> = {
  uiFontSize: [11, 16],
  gridFontSize: [10, 18],
  gridRowHeight: [18, 40],
  sqlFontSize: [10, 20],
  sqlLineHeight: [16, 40],
  sqlTabSize: [1, 8],
  pageSize: [10, 500],
};

export function fontFamilyValue(chain: string[]): string {
  const fonts = chain.map((font) => font.trim()).filter(Boolean).map((font) => `"${font.replaceAll('\\', '\\\\').replaceAll('"', '\\"')}"`);
  return [...fonts, 'monospace'].join(', ');
}

interface SettingsState {
  settings: IdeSettings;
  setNumber: (key: keyof IdeSettings, value: number) => void;
  setBoolean: (key: keyof IdeSettings, value: boolean) => void;
  setText: (key: keyof IdeSettings, value: string) => void;
  setFonts: (key: 'uiFonts' | 'gridFonts' | 'sqlFonts', value: string[]) => void;
  reset: () => void;
}

export const useSettingsStore = create<SettingsState>()(persist((set) => ({
  settings: { ...DEFAULT_SETTINGS },
  setNumber: (key, value) => set((state) => {
    const range = ranges[key];
    const next = range ? Math.min(range[1], Math.max(range[0], Math.round(value))) : value;
    return { settings: { ...state.settings, [key]: next } };
  }),
  setBoolean: (key, value) => set((state) => ({ settings: { ...state.settings, [key]: value } })),
  setText: (key, value) => set((state) => ({ settings: { ...state.settings, [key]: value } })),
  setFonts: (key, value) => set((state) => ({ settings: { ...state.settings, [key]: value.slice(0, 10) } })),
  reset: () => set({ settings: { ...DEFAULT_SETTINGS } }),
}), {
  name: 'chusql.ide.settings.v1',
  partialize: (state) => ({ settings: state.settings }),
}));
