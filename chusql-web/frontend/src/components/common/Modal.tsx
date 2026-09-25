import { useEffect, useId, useRef, type ReactNode } from 'react';
import { X } from 'lucide-react';
import { zhCN } from '../../i18n/zh-CN';
import { IconButton } from './IconButton';
import styles from './common.module.css';

// 通用模态对话框：标题栏、内容区与可选页脚。

interface ModalProps {
  title: string;
  children: ReactNode;
  footer?: ReactNode;
  onClose: () => void;
}

export function Modal({ title, children, footer, onClose }: ModalProps) {
  const titleId = useId();
  const dialogRef = useRef<HTMLDialogElement>(null);
  useEffect(() => {
    const dialog = dialogRef.current;
    if (!dialog) return undefined;
    if (typeof dialog.showModal === 'function') dialog.showModal();
    else dialog.setAttribute('open', '');
    return () => {
      if (typeof dialog.close === 'function') dialog.close();
      else dialog.removeAttribute('open');
    };
  }, []);
  return (
    <dialog ref={dialogRef} aria-labelledby={titleId} className={styles.dialog} onCancel={(event) => { event.preventDefault(); onClose(); }}>
      <header className={styles.dialogHeader}>
        <h2 id={titleId}>{title}</h2>
        <IconButton icon={<X size={14} />} label={zhCN.close} compact onClick={onClose} />
      </header>
      <div className={styles.dialogBody}>{children}</div>
      {footer && <footer className={styles.dialogFooter}>{footer}</footer>}
    </dialog>
  );
}
