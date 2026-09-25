import type { ButtonHTMLAttributes, ReactNode } from 'react';
import styles from './common.module.css';

// 通用图标按钮：支持紧凑模式，标签兼作无障碍名。

interface IconButtonProps extends ButtonHTMLAttributes<HTMLButtonElement> {
  icon: ReactNode;
  label: string;
  compact?: boolean;
}

export function IconButton({ icon, label, compact = false, ...props }: IconButtonProps) {
  return (
    <button className={`${styles.iconButton} ${compact ? styles.compact : ''}`} aria-label={label} title={label} {...props}>
      {icon}
      {!compact && <span>{label}</span>}
    </button>
  );
}
