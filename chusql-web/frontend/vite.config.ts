import { fileURLToPath, URL } from 'node:url';
import react from '@vitejs/plugin-react';
import { defineConfig } from 'vite';

// Vite 配置：React 插件、Monaco nonce 注入与构建输出。

export default defineConfig({
  plugins: [react(), {
    name: 'monaco-style-nonce',
    enforce: 'pre',
    transform(code, id) {
      if (!id.split('\\').join('/').includes('/monaco-editor/esm/')) return;
      const source = "document.createElement('style')";
      if (!code.includes(source)) return;
      return code.split(source).join(`Object.assign(document.createElement('style'), { nonce: document.querySelector('meta[name="chusql-style-nonce"]')?.content || '' })`);
    },
  }],
  base: '/static/',
  resolve: {
    alias: { '@': fileURLToPath(new URL('./src', import.meta.url)) },
  },
  build: {
    outDir: '../static',
    emptyOutDir: true,
    assetsDir: '',
    rollupOptions: {
      output: {
        entryFileNames: 'app.js',
        chunkFileNames: '[name].js',
        assetFileNames: (asset) => asset.names?.some((name) => name.endsWith('.css')) ? 'style.css' : '[name][extname]',
      },
    },
  },
});
