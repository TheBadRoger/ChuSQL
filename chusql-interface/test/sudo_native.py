import ctypes
import os
from pathlib import Path
import subprocess
import sys

# 验证 Unix 本机凭据的所有权、权限及符号链接拒绝。

# 执行本机凭据安全回归。
def main():
    library = ctypes.CDLL(sys.argv[1])
    port = int(sys.argv[2])
    library.chusql_sudo_create.argtypes = [ctypes.c_int, ctypes.c_char_p]
    library.chusql_sudo_read.argtypes = [ctypes.c_int, ctypes.c_void_p]
    secret = b'a' * 64
    buffer = ctypes.create_string_buffer(64)
    if len(sys.argv) > 3:
        assert library.chusql_sudo_privileged() == 0
        assert library.chusql_sudo_create(port, secret) != 0
        assert library.chusql_sudo_read(port, buffer) != 0
        print('non-root creation and reading refused')
        return
    assert library.chusql_sudo_privileged() == 1
    assert library.chusql_sudo_create(port, secret) == 0
    directory = Path(f'/var/run/chusql-sudo-{port}')
    file = directory / 'credential'
    try:
        assert directory.stat().st_uid == 0
        assert directory.stat().st_mode & 0o777 == 0o700
        assert file.stat().st_mode & 0o777 == 0o600
        assert library.chusql_sudo_read(port, buffer) == 0
        assert buffer.raw == secret
        assert library.chusql_sudo_create(port, secret) != 0
        subprocess.run([sys.executable, __file__, sys.argv[1], str(port), '--deny'],
                       user=65534, group=65534, check=True)
        file.chmod(0o644)
        assert library.chusql_sudo_read(port, buffer) != 0
        file.chmod(0o600)
        file.rename(directory / 'original')
        file.symlink_to(directory / 'original')
        assert library.chusql_sudo_read(port, buffer) != 0
        file.unlink()
        (directory / 'original').rename(file)
        file.write_bytes(secret + b'extra')
        assert library.chusql_sudo_read(port, buffer) != 0
        file.write_bytes(secret)
        os.chown(file, 65534, 65534)
        assert library.chusql_sudo_read(port, buffer) != 0
        os.chown(file, 0, 0)
        directory.chmod(0o755)
        assert library.chusql_sudo_read(port, buffer) != 0
        directory.chmod(0o700)
    finally:
        assert library.chusql_sudo_remove(port) == 0
    assert not directory.exists()
    print('root round trip, duplicate, permissions, ownership, symlink and cleanup passed')


if __name__ == '__main__':
    main()
