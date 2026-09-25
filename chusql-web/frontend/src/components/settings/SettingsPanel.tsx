import { RotateCcw } from 'lucide-react';
import { useEffect, useState } from 'react';
import { zhCN } from '../../i18n/zh-CN';
import { fontFamilyValue, useSettingsStore } from '../../store/settingsStore';
import { IconButton } from '../common/IconButton';
import { FontChainEditor } from './FontChainEditor';
import styles from './settings.module.css';

// 设置面板：外观与行为两组选项及字体预览。

function NumberField({ label, value, min, max, onChange }: { label: string; value: number; min: number; max: number; onChange: (value: number) => void }) {
  const [draft, setDraft] = useState(String(value));
  useEffect(() => setDraft(String(value)), [value]);
  const commit = () => {
    if (!draft.trim() || !Number.isFinite(Number(draft))) { setDraft(String(value)); return; }
    const next = Math.min(max, Math.max(min, Math.round(Number(draft))));
    setDraft(String(next)); onChange(next);
  };
  return <label className={styles.field}><span>{label}</span><input type="number" min={min} max={max} value={draft} onChange={(event) => setDraft(event.target.value)} onBlur={commit} onKeyDown={(event) => { if (event.key === 'Enter') commit(); if (event.key === 'Escape') setDraft(String(value)); }} /></label>;
}
function Toggle({ label, checked, onChange }: { label: string; checked: boolean; onChange: (value: boolean) => void }) {
  return <label className={styles.toggle}><input type="checkbox" checked={checked} onChange={(event) => onChange(event.target.checked)} /><span>{label}</span></label>;
}

export function SettingsPanel() {
  const [section, setSection] = useState<'appearance' | 'behavior'>('appearance');
  const { settings, setNumber, setBoolean, setText, setFonts, reset } = useSettingsStore();
  return <div className={styles.settings}>
    <aside><h2>{zhCN.settings}</h2><button className={section === 'appearance' ? styles.active : ''} onClick={() => setSection('appearance')}>{zhCN.settingsAppearance}</button><button className={section === 'behavior' ? styles.active : ''} onClick={() => setSection('behavior')}>{zhCN.settingsBehavior}</button></aside>
    <section><header><h1>{section === 'appearance' ? zhCN.settingsAppearance : zhCN.settingsBehavior}</h1></header>
      {section === 'appearance' ? <>
        <FontChainEditor label={zhCN.interfaceFont} value={settings.uiFonts} onChange={(value) => setFonts('uiFonts', value)} previewStyle={{ fontFamily: fontFamilyValue(settings.uiFonts), fontSize: settings.uiFontSize }}><NumberField label={zhCN.fontSize} value={settings.uiFontSize} min={11} max={16} onChange={(value) => setNumber('uiFontSize', value)} /></FontChainEditor>
        <FontChainEditor label={zhCN.gridFont} value={settings.gridFonts} onChange={(value) => setFonts('gridFonts', value)} previewStyle={{ fontFamily: fontFamilyValue(settings.gridFonts), fontSize: settings.gridFontSize, lineHeight: `${settings.gridRowHeight}px` }}><NumberField label={zhCN.fontSize} value={settings.gridFontSize} min={10} max={18} onChange={(value) => setNumber('gridFontSize', value)} /><NumberField label={zhCN.rowHeight} value={settings.gridRowHeight} min={18} max={40} onChange={(value) => setNumber('gridRowHeight', value)} /></FontChainEditor>
        <FontChainEditor label={zhCN.sqlFont} value={settings.sqlFonts} onChange={(value) => setFonts('sqlFonts', value)} previewStyle={{ fontFamily: fontFamilyValue(settings.sqlFonts), fontSize: settings.sqlFontSize, lineHeight: `${settings.sqlLineHeight}px` }}><NumberField label={zhCN.fontSize} value={settings.sqlFontSize} min={10} max={20} onChange={(value) => setNumber('sqlFontSize', value)} /><NumberField label={zhCN.rowHeight} value={settings.sqlLineHeight} min={16} max={40} onChange={(value) => setNumber('sqlLineHeight', value)} /></FontChainEditor>
      </> : <div className={styles.behavior}>
        <NumberField label={zhCN.pageSize} value={settings.pageSize} min={10} max={500} onChange={(value) => setNumber('pageSize', value)} />
        <label className={styles.field}><span>{zhCN.nullDisplay}</span><input value={settings.nullText} onChange={(event) => setText('nullText', event.target.value)} /></label>
        <NumberField label={zhCN.tabSize} value={settings.sqlTabSize} min={1} max={8} onChange={(value) => setNumber('sqlTabSize', value)} />
        <Toggle label={zhCN.showLineNumbers} checked={settings.sqlLineNumbers} onChange={(value) => setBoolean('sqlLineNumbers', value)} />
        <Toggle label={zhCN.autocomplete} checked={settings.autocomplete} onChange={(value) => setBoolean('autocomplete', value)} />
        <Toggle label={zhCN.minimap} checked={settings.minimap} onChange={(value) => setBoolean('minimap', value)} />
      </div>}
      <footer><IconButton icon={<RotateCcw size={14} />} label={zhCN.restoreDefaults} onClick={reset} /></footer>
    </section>
  </div>;
}
