#ifdef _WIN32
#define _WIN32_WINNT 0x0600
#include <windows.h>
#include <sddl.h>
#include <stdio.h>
#else
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <errno.h>
#include <stdio.h>
#endif
#include <string.h>

/* 创建、读取和清理仅本机提升身份可访问的认证凭据。 */

#ifdef _WIN32
/* 检查当前提升令牌的管理员成员资格。 */
int chusql_sudo_privileged(void) {
    SID_IDENTIFIER_AUTHORITY authority = SECURITY_NT_AUTHORITY;
    PSID administrators = NULL;
    BOOL member = FALSE;
    if (!AllocateAndInitializeSid(&authority, 2, SECURITY_BUILTIN_DOMAIN_RID,
        DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0, &administrators)) return 0;
    if (!CheckTokenMembership(NULL, administrators, &member)) member = FALSE;
    FreeSid(administrators);
    return member != FALSE;
}

/* 生成系统保护目录中的端口专属路径。 */
static int credential_path(int port, wchar_t *directory, wchar_t *file) {
    wchar_t base[MAX_PATH];
    UINT length = GetSystemDirectoryW(base, MAX_PATH);
    if (!length || length >= MAX_PATH - 80 || port < 1 || port > 65535) return ERROR_INVALID_PARAMETER;
    swprintf(directory, MAX_PATH, L"%ls\\config\\ChuSQL-Sudo-%d", base, port);
    swprintf(file, MAX_PATH, L"%ls\\credential", directory);
    return 0;
}

/* 排除重解析目录与凭据文件。 */
static int plain_path(const wchar_t *path, int directory) {
    DWORD attributes = GetFileAttributesW(path);
    return attributes != INVALID_FILE_ATTRIBUTES && !(attributes & FILE_ATTRIBUTE_REPARSE_POINT)
        && (!!(attributes & FILE_ATTRIBUTE_DIRECTORY) == directory);
}

/* 用管理员与系统专属访问列表创建凭据。 */
int chusql_sudo_create(int port, const char *secret) {
    wchar_t directory[MAX_PATH], file[MAX_PATH];
    PSECURITY_DESCRIPTOR descriptor = NULL;
    SECURITY_ATTRIBUTES attributes = {sizeof(SECURITY_ATTRIBUTES), NULL, FALSE};
    DWORD written = 0, error;
    HANDLE handle;
    if (!chusql_sudo_privileged()) return ERROR_ACCESS_DENIED;
    error = credential_path(port, directory, file);
    if (error) return (int)error;
    if (!ConvertStringSecurityDescriptorToSecurityDescriptorW(
        L"D:P(A;OICI;FA;;;BA)(A;OICI;FA;;;SY)", SDDL_REVISION_1, &descriptor, NULL)) return (int)GetLastError();
    attributes.lpSecurityDescriptor = descriptor;
    if (!CreateDirectoryW(directory, &attributes)) {
        error = GetLastError(); LocalFree(descriptor); return (int)error;
    }
    handle = CreateFileW(file, GENERIC_WRITE, FILE_SHARE_READ, &attributes, CREATE_NEW,
        FILE_ATTRIBUTE_NORMAL | FILE_FLAG_OPEN_REPARSE_POINT, NULL);
    LocalFree(descriptor);
    if (handle == INVALID_HANDLE_VALUE) {
        error = GetLastError(); RemoveDirectoryW(directory); return (int)error;
    }
    error = 0;
    if (!WriteFile(handle, secret, 64, &written, NULL) || written != 64 || !FlushFileBuffers(handle))
        error = GetLastError() ? GetLastError() : ERROR_WRITE_FAULT;
    if (!CloseHandle(handle) && !error) error = GetLastError();
    if (error) { DeleteFileW(file); RemoveDirectoryW(directory); }
    return (int)error;
}

/* 从系统保护目录读取本机凭据。 */
int chusql_sudo_read(int port, char *secret) {
    wchar_t directory[MAX_PATH], file[MAX_PATH];
    DWORD received = 0, error;
    LARGE_INTEGER size;
    HANDLE handle;
    if (!chusql_sudo_privileged()) return ERROR_ACCESS_DENIED;
    error = credential_path(port, directory, file);
    if (error) return (int)error;
    if (!plain_path(directory, 1) || !plain_path(file, 0)) return ERROR_ACCESS_DENIED;
    handle = CreateFileW(file, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL | FILE_FLAG_OPEN_REPARSE_POINT, NULL);
    if (handle == INVALID_HANDLE_VALUE) return (int)GetLastError();
    error = 0;
    if (!GetFileSizeEx(handle, &size) || size.QuadPart != 64
        || !ReadFile(handle, secret, 64, &received, NULL) || received != 64) error = ERROR_INVALID_DATA;
    if (!CloseHandle(handle) && !error) error = GetLastError();
    return (int)error;
}

/* 删除本次服务启动创建的凭据目录。 */
int chusql_sudo_remove(int port) {
    wchar_t directory[MAX_PATH], file[MAX_PATH];
    int error = credential_path(port, directory, file);
    if (error) return error;
    if (!chusql_sudo_privileged()) return ERROR_ACCESS_DENIED;
    if (!DeleteFileW(file)) return (int)GetLastError();
    return RemoveDirectoryW(directory) ? 0 : (int)GetLastError();
}
#else
/* 检查当前进程的有效 root 身份。 */
int chusql_sudo_privileged(void) { return geteuid() == 0; }

/* 打开 root 专属目录并验证所有权与权限。 */
static int credential_directory(int port) {
    char path[96];
    struct stat status;
    int fd;
    if (port < 1 || port > 65535) { errno = EINVAL; return -1; }
    snprintf(path, sizeof(path), "/var/run/chusql-sudo-%d", port);
    fd = open(path, O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
    if (fd < 0) return -1;
    if (fstat(fd, &status) || status.st_uid != 0 || (status.st_mode & 0777) != 0700) {
        close(fd); errno = EACCES; return -1;
    }
    return fd;
}

/* 在 root 专属目录中独占创建凭据。 */
int chusql_sudo_create(int port, const char *secret) {
    char path[96];
    int dir, fd, error = 0;
    if (!chusql_sudo_privileged()) return EACCES;
    if (port < 1 || port > 65535) return EINVAL;
    snprintf(path, sizeof(path), "/var/run/chusql-sudo-%d", port);
    if (mkdir(path, 0700)) return errno;
    dir = credential_directory(port);
    if (dir < 0) { error = errno; rmdir(path); return error; }
    fd = openat(dir, "credential", O_WRONLY | O_CREAT | O_EXCL | O_NOFOLLOW | O_CLOEXEC, 0600);
    if (fd < 0) error = errno;
    else {
        if (write(fd, secret, 64) != 64 || fsync(fd)) error = errno ? errno : EIO;
        if (close(fd) && !error) error = errno;
    }
    if (error) unlinkat(dir, "credential", 0);
    if (close(dir) && !error) error = errno;
    if (error) rmdir(path);
    return error;
}

/* 验证 root 文件所有权后读取凭据。 */
int chusql_sudo_read(int port, char *secret) {
    struct stat status;
    int dir, fd, error = 0;
    if (!chusql_sudo_privileged()) return EACCES;
    dir = credential_directory(port);
    if (dir < 0) return errno;
    fd = openat(dir, "credential", O_RDONLY | O_NOFOLLOW | O_CLOEXEC);
    if (fd < 0) error = errno;
    else {
        if (fstat(fd, &status) || !S_ISREG(status.st_mode) || status.st_uid != 0
            || (status.st_mode & 0777) != 0600 || status.st_size != 64 || status.st_nlink != 1) error = EACCES;
        else if (read(fd, secret, 64) != 64) error = errno ? errno : EIO;
        if (close(fd) && !error) error = errno;
    }
    if (close(dir) && !error) error = errno;
    return error;
}

/* 删除本次服务启动创建的凭据目录。 */
int chusql_sudo_remove(int port) {
    char path[96];
    int dir, error = 0;
    if (!chusql_sudo_privileged()) return EACCES;
    dir = credential_directory(port);
    if (dir < 0) return errno;
    if (unlinkat(dir, "credential", 0)) error = errno;
    if (close(dir) && !error) error = errno;
    snprintf(path, sizeof(path), "/var/run/chusql-sudo-%d", port);
    if (!error && rmdir(path)) error = errno;
    return error;
}
#endif
