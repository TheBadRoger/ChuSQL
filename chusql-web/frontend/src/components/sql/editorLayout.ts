// 修复被 CSP 拦截的 Monaco 内联样式：用 CSSOM 重放布局属性。

const layoutProperties = new Set([
  'left', 'right', 'top', 'bottom', 'width', 'height', 'min-width', 'max-width',
  'position', 'display', 'line-height', 'font-size', 'font-family', 'font-weight',
  'font-style', 'letter-spacing', 'vertical-align', 'white-space', 'transform',
  'color', 'background-color', 'border-color', 'border-width', 'border-style',
  'padding-left', 'padding-right', 'margin-left', 'margin-right', 'opacity',
]);

export function restoreEditorLayout(host: HTMLElement): () => void {
  const parsed = document.createElement('span').style;
  const restore = (element: Element) => {
    if (!(element instanceof HTMLElement) || element.style.length) return;
    const raw = element.getAttribute('style');
    if (!raw) return;
    parsed.cssText = raw;
    for (const property of Array.from(parsed)) {
      const value = parsed.getPropertyValue(property);
      if (layoutProperties.has(property) && !/url\s*\(|expression\s*\(/i.test(value)) element.style.setProperty(property, value);
    }
  };
  const restoreTree = (element: Element) => {
    restore(element);
    element.querySelectorAll('[style]').forEach(restore);
  };
  const observer = new MutationObserver((records) => {
    for (const record of records) {
      if (record.type === 'attributes' && record.target instanceof Element) restore(record.target);
      else record.addedNodes.forEach((node) => { if (node instanceof Element) restoreTree(node); });
    }
  });
  observer.observe(host, { subtree: true, childList: true, attributes: true, attributeFilter: ['style'] });
  restoreTree(host);
  return () => observer.disconnect();
}
