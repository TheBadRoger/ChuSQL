import { defineConfig } from 'vitest/config';

// Vitest 配置：jsdom 环境与测试初始化脚本。

export default defineConfig({
  test: {
    environment: 'jsdom',
    setupFiles: './src/test/setup.ts',
    restoreMocks: true,
  },
});
