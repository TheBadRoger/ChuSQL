import { GripVertical, Plus, Trash2 } from 'lucide-react';
import { useState, type CSSProperties, type DragEvent, type ReactNode } from 'react';
import { zhCN } from '../../i18n/zh-CN';
import { IconButton } from '../common/IconButton';
import styles from './settings.module.css';

// 字体链编辑器：增删、拖动排序并实时预览。

interface FontChainEditorProps { label: string; value: string[]; onChange: (fonts: string[]) => void; previewStyle: CSSProperties; children?: ReactNode }

export function FontChainEditor({ label, value, onChange, previewStyle, children }: FontChainEditorProps) {
  const [dragged, setDragged] = useState<number>();
  const move = (event: DragEvent, target: number) => {
    event.preventDefault();
    if (dragged === undefined || dragged === target) return;
    const next = [...value]; const [font] = next.splice(dragged, 1); next.splice(target, 0, font); onChange(next); setDragged(target);
  };
  return <fieldset className={styles.fontGroup}><legend>{label}</legend>
    <div className={styles.fontList}>{value.map((font, index) => <div className={styles.fontRow} key={index} onDragOver={(event) => move(event, index)}><span draggable onDragStart={() => setDragged(index)} onDragEnd={() => setDragged(undefined)} title={zhCN.moveFont}><GripVertical size={13} /></span><input aria-label={`${label} ${zhCN.fontName} ${index + 1}`} value={font} onChange={(event) => onChange(value.map((item, itemIndex) => itemIndex === index ? event.target.value : item))} /><IconButton icon={<Trash2 size={13} />} compact label={zhCN.removeFont(font)} disabled={value.length === 1} onClick={() => onChange(value.filter((_, itemIndex) => itemIndex !== index))} /></div>)}</div>
    <IconButton icon={<Plus size={13} />} label={zhCN.addFallback} onClick={() => onChange([...value, ''])} />
    {children}
    <div className={styles.preview} style={previewStyle}><strong>{zhCN.preview}</strong><span>{zhCN.previewText}</span></div>
  </fieldset>;
}
