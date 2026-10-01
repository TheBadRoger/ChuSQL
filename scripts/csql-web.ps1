# csql-web：只负责启动 Web 前端（先拉起存储进程，再起 Web 服务）。
# 配置走 chusql.toml，两层各读各的分区；这里只取 [web] 的 host/port 拼地址。

$ErrorActionPreference = 'Stop'

function Fail([string]$msg) { Write-Host "!! $msg" -ForegroundColor Red; exit 1 }
function Say([string]$msg) { Write-Host $msg }

$home_dir = Split-Path -Parent $MyInvocation.MyCommand.Path
$bin = Join-Path $home_dir 'bin'

$config_dir = if ($env:APPDATA) { Join-Path $env:APPDATA 'ChuSQL' } else { Join-Path $home_dir 'config' }
$config = Join-Path $config_dir 'chusql.toml'
if (-not (Test-Path $config)) { Fail "config not found: $config" }

function Get-WebSetting([string]$Key, [string]$Default) {
    $section = ''
    $pattern = '^\s*' + [regex]::Escape($Key) + '\s*='
    foreach ($raw in (Get-Content -Path $config -Encoding UTF8)) {
        if ($raw -match '^\s*\[') { $section = ($raw -replace '[\[\]\s]', ''); continue }
        if ($section -ne 'web') { continue }
        $line = $raw -replace '#.*$', ''
        if ($line -match $pattern) {
            return (($line -replace '^[^=]*=\s*', '').Trim().Trim('"'))
        }
    }
    return $Default
}

$storage_exe = Join-Path $bin 'chusql-storage.exe'
$web_exe = Join-Path $bin 'chusql-web.exe'
if (-not (Test-Path $storage_exe)) { Fail "storage binary not found in $bin" }
if (-not (Test-Path $web_exe)) { Fail "web binary not found in $bin" }

$host_addr = Get-WebSetting 'host' '127.0.0.1'
$port = Get-WebSetting 'port' '7777'
$log_dir = Join-Path $home_dir 'logs'
New-Item -ItemType Directory -Force -Path $log_dir | Out-Null

$storage = $null
$web = $null
try {
    $storage = Start-Process -FilePath $storage_exe -WorkingDirectory $home_dir -PassThru -RedirectStandardOutput (Join-Path $log_dir 'storage.log') -RedirectStandardError (Join-Path $log_dir 'storage.err.log')
    $web = Start-Process -FilePath $web_exe -WorkingDirectory $home_dir -PassThru -RedirectStandardOutput (Join-Path $log_dir 'web.log') -RedirectStandardError (Join-Path $log_dir 'web.err.log')

    Say 'starting web ...'
    Say "ready: http://${host_addr}:${port}/"
    Say "logs:  $log_dir"

    while (-not $web.HasExited) { Start-Sleep -Milliseconds 300 }
    Say "!! web exited with code $($web.ExitCode)"
}
finally {
    foreach ($proc in @($web, $storage)) {
        if ($proc -and -not $proc.HasExited) { Stop-Process -Id $proc.Id -Force -ErrorAction SilentlyContinue }
    }
    Say 'stopped.'
}
