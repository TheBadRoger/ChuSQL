import '@testing-library/jest-dom/vitest';
import { cleanup } from '@testing-library/react';
import { afterEach } from 'vitest';

// 测试环境初始化：注册 jest-dom 断言与自动清理。

afterEach(cleanup);
