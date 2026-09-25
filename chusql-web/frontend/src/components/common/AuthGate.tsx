import { type FormEvent, type ReactNode, useEffect, useState } from 'react';
import { Database } from 'lucide-react';
import { zhCN } from '../../i18n/zh-CN';
import styles from './auth.module.css';

// 登录门组件：先查会话状态，未登录时渲染登录表单。

type AuthState = 'checking' | 'signed-out' | 'signed-in';

export function AuthGate({ children }: { children: ReactNode }) {
  const [state, setState] = useState<AuthState>('checking');
  const [user, setUser] = useState('root');
  const [password, setPassword] = useState('');
  const [busy, setBusy] = useState(false);
  const [failed, setFailed] = useState(false);

  useEffect(() => {
    const controller = new AbortController();
    void fetch('/api/session', { credentials: 'same-origin', signal: controller.signal })
      .then((response) => setState(response.ok ? 'signed-in' : 'signed-out'))
      .catch((error: unknown) => {
        if (!(error instanceof DOMException && error.name === 'AbortError')) setState('signed-out');
      });
    return () => controller.abort();
  }, []);

  const submit = async (event: FormEvent) => {
    event.preventDefault();
    setBusy(true);
    setFailed(false);
    try {
      const response = await fetch('/api/login', {
        method: 'POST', credentials: 'same-origin', headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ user, password }),
      });
      if (response.ok) setState('signed-in');
      else setFailed(true);
    } catch {
      setFailed(true);
    } finally {
      setBusy(false);
    }
  };

  if (state === 'signed-in') return children;
  if (state === 'checking') return <main className={styles.center}><p>{zhCN.signingIn}</p></main>;
  return <main className={styles.center}>
    <form className={styles.login} onSubmit={(event) => void submit(event)}>
      <header><Database size={18} /><h1>{zhCN.signInTitle}</h1></header>
      <label><span>{zhCN.userName}</span><input autoFocus autoComplete="username" value={user} onChange={(event) => setUser(event.target.value)} /></label>
      <label><span>{zhCN.password}</span><input type="password" autoComplete="current-password" value={password} onChange={(event) => setPassword(event.target.value)} /></label>
      {failed && <p role="alert">{zhCN.signInFailed}</p>}
      <button disabled={busy || !user || !password}>{zhCN.signIn}</button>
    </form>
  </main>;
}
