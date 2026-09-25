import { fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';
import { AuthGate } from './AuthGate';

// 覆盖登录门的会话校验与登录流程。

describe('AuthGate', () => {
  afterEach(() => vi.unstubAllGlobals());

  it('keeps REST content private until a successful login', async () => {
    const request = vi.fn()
      .mockResolvedValueOnce(new Response('', { status: 401 }))
      .mockResolvedValueOnce(new Response(null, { status: 204 }));
    vi.stubGlobal('fetch', request);
    render(<AuthGate><p>protected content</p></AuthGate>);

    expect(await screen.findByRole('heading', { name: '登录 ChuSQL' })).toBeInTheDocument();
    expect(screen.queryByText('protected content')).not.toBeInTheDocument();
    fireEvent.change(screen.getByLabelText('密码'), { target: { value: 'chusql' } });
    fireEvent.click(screen.getByRole('button', { name: '登录' }));

    await waitFor(() => expect(screen.getByText('protected content')).toBeInTheDocument());
    expect(request).toHaveBeenLastCalledWith('/api/login', expect.objectContaining({ method: 'POST' }));
  });
});
