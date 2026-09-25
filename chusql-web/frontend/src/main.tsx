import { StrictMode } from 'react';
import { createRoot } from 'react-dom/client';
import { App } from './App';
import { MockAdapter } from './adapters/MockAdapter';
import { RestAdapter } from './adapters/RestAdapter';
import { AuthGate } from './components/common/AuthGate';
import './styles/global.css';

// 前端入口：按环境选择 mock/rest 数据源并挂载根组件。

const adapterName = import.meta.env.VITE_CHUSQL_ADAPTER ?? (import.meta.env.DEV ? 'mock' : 'rest');
const adapter = adapterName === 'rest' ? new RestAdapter() : new MockAdapter();
const root = document.getElementById('root');
if (!root) throw new Error('Root element is missing.');
const application = <App adapter={adapter} />;
createRoot(root).render(<StrictMode>{adapterName === 'rest' ? <AuthGate>{application}</AuthGate> : application}</StrictMode>);
