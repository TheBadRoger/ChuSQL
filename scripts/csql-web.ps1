$ErrorActionPreference = 'Stop'

# csql-web：先起独占数据目录的 server，再起 Web 服务，配置读 settings.toml。

# 打印错误并退出 1。
function Fail([string]$msg) { Write-Host "!! $msg" -ForegroundColor Red; exit 1 }
# 打印一行输出。
function Say([string]$msg) { Write-Host $msg }

$home_dir = Split-Path -Parent $MyInvocation.MyCommand.Path
$bin = Join-Path $home_dir 'bin'

$config_dir = if ($env:APPDATA) { Join-Path $env:APPDATA 'ChuSQL' } else { Join-Path $home_dir 'config' }
$config = Join-Path $config_dir 'settings.toml'
if (-not (Test-Path $config)) { Fail "config not found: $config" }

# 从配置里取某个分区的一个值。
function Get-SectionSetting([string]$Section, [string]$Key, [string]$Default) {
    $section = ''
    $pattern = '^\s*' + [regex]::Escape($Key) + '\s*='
    foreach ($raw in (Get-Content -Path $config -Encoding UTF8)) {
        if ($raw -match '^\s*\[') { $section = ($raw -replace '[\[\]\s]', ''); continue }
        if ($section -ne $Section) { continue }
        $line = $raw -replace '#.*$', ''
        if ($line -match $pattern) {
            return (($line -replace '^[^=]*=\s*', '').Trim().Trim('"'))
        }
    }
    return $Default
}

$server_exe = Join-Path $bin 'chusql-server.exe'
$web_exe = Join-Path $bin 'chusql-web.exe'
if (-not (Test-Path $server_exe)) { Fail "server binary not found in $bin" }
if (-not (Test-Path $web_exe)) { Fail "web binary not found in $bin" }

# 监听地址与端口取 [web] 段，日志目录取 [storage] log_files（相对路径以安装目录为基准）。
$host_addr = Get-SectionSetting 'web' 'listen_host' '127.0.0.1'
$port = Get-SectionSetting 'web' 'port' '7778'
$log_setting = Get-SectionSetting 'storage' 'log_files' './logs'
$log_dir = if ([System.IO.Path]::IsPathRooted($log_setting)) { $log_setting } else { Join-Path $home_dir $log_setting }
New-Item -ItemType Directory -Force -Path $log_dir | Out-Null

$server = $null
$web = $null
try {
    # server 先起来：它进程内装入存储库、独占数据目录，Web 的存储请求都经它转发
    $server = Start-Process -FilePath $server_exe -ArgumentList '--config', $config -WorkingDirectory $home_dir -PassThru -RedirectStandardOutput (Join-Path $log_dir 'server.log') -RedirectStandardError (Join-Path $log_dir 'server.err.log')
    $web = Start-Process -FilePath $web_exe -ArgumentList '--config', $config -WorkingDirectory $home_dir -PassThru -RedirectStandardOutput (Join-Path $log_dir 'web.log') -RedirectStandardError (Join-Path $log_dir 'web.err.log')

    Say 'starting web ...'
    Say "ready: http://${host_addr}:${port}/"
    Say "logs:  $log_dir"

    while (-not $web.HasExited) { Start-Sleep -Milliseconds 300 }
    Say "!! web exited with code $($web.ExitCode)"
}
finally {
    foreach ($proc in @($web, $server)) {
        if ($proc -and -not $proc.HasExited) { Stop-Process -Id $proc.Id -Force -ErrorAction SilentlyContinue }
    }
    Say 'stopped.'
}
