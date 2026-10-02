[CmdletBinding()]
param(
    [string]$InstallDir = '',
    [string]$DataDir = '',
    [switch]$KeepData,
    [switch]$KeepConfig,
    [switch]$Yes
)

$ErrorActionPreference = 'Stop'

# Windows 卸载脚本：停服务、删程序与配置、从用户 PATH 摘掉安装目录。
# 数据目录默认一起删，-KeepData 保留。

# 打印步骤标题。
function Step([string]$msg) { Write-Host ''; Write-Host "==> $msg" -ForegroundColor Cyan }
# 打印一行输出。
function Say([string]$msg) { Write-Host $msg }
# 报错退出。
function Fail([string]$msg) { Write-Host "!! $msg" -ForegroundColor Red; exit 1 }

if (-not $InstallDir) { $InstallDir = Join-Path $env:LOCALAPPDATA 'ChuSQL' }
if (-not $DataDir) { $DataDir = Join-Path $InstallDir 'data' }
$InstallDir = [System.IO.Path]::GetFullPath($InstallDir)
$DataDir = [System.IO.Path]::GetFullPath($DataDir)
$configDir = if ($env:APPDATA) { Join-Path $env:APPDATA 'ChuSQL' } else { Join-Path $InstallDir 'config' }
$configFile = Join-Path $configDir 'chusql.toml'

if (-not $Yes) {
    if ([Console]::IsInputRedirected) {
        Fail 'no terminal: rerun with -Yes to confirm'
    }
    Say 'ChuSQL uninstaller'
    Say ("  install dir  {0}" -f $InstallDir)
    Say ("  data dir     {0}" -f $DataDir)
    if ($KeepData) { Say '  data         kept' }
    $answer = (Read-Host 'Remove it? [y/N]').Trim()
    if ($answer -notmatch '^(y|yes)$') {
        Say 'nothing was removed'
        exit 0
    }
}

Step 'Stopping the service'
$running = @(Get-Process -Name 'chusql-server', 'chusql-web' -ErrorAction SilentlyContinue |
    Where-Object {
        try { $_.Path -and $_.Path.StartsWith($InstallDir, [StringComparison]::OrdinalIgnoreCase) }
        catch { $false }
    })
foreach ($proc in $running) {
    Stop-Process -Id $proc.Id -Force -ErrorAction SilentlyContinue
    Say ("  stopped {0} (pid {1})" -f $proc.ProcessName, $proc.Id)
}
if ($running.Count -eq 0) { Say '  nothing was running' }

Step 'Removing files'
if (-not (Test-Path $InstallDir)) {
    Say ("  not found: {0}" -f $InstallDir)
} elseif ($KeepData) {
    Get-ChildItem -LiteralPath $InstallDir -Force | ForEach-Object {
        if ($_.FullName -eq $DataDir) {
            Say ("  kept data: {0}" -f $_.FullName)
        } else {
            Remove-Item -LiteralPath $_.FullName -Recurse -Force -ErrorAction SilentlyContinue
        }
    }
    Say ("  kept {0} (data only)" -f $InstallDir)
} else {
    Remove-Item -LiteralPath $InstallDir -Recurse -Force
    if (-not $DataDir.StartsWith($InstallDir, [StringComparison]::OrdinalIgnoreCase)) {
        Remove-Item -LiteralPath $DataDir -Recurse -Force -ErrorAction SilentlyContinue
    }
    Say ("  removed {0}" -f $InstallDir)
}

Step 'Removing chusql.toml'
if ($KeepConfig) {
    Say ("  kept config: {0}" -f $configFile)
} else {
    if (Test-Path $configFile) {
        Remove-Item -LiteralPath $configFile -Force
        Say ("  removed config: {0}" -f $configFile)
    } else {
        Say ("  no config file: {0}" -f $configFile)
    }
    if (Test-Path $configDir) { Remove-Item -LiteralPath $configDir -Force -ErrorAction SilentlyContinue }
}

Step 'Removing the PATH entry'
$userPath = [Environment]::GetEnvironmentVariable('Path', 'User')
if (-not $userPath) { $userPath = '' }
$entries = @($userPath -split ';' | Where-Object { $_ })
$updated = @($entries | Where-Object { $_ -ne $InstallDir })
if ($updated.Count -ne $entries.Count) {
    [Environment]::SetEnvironmentVariable('Path', ($updated -join ';'), 'User')
    Say ("  removed {0} from the user PATH" -f $InstallDir)
} else {
    Say '  the user PATH has no install directory'
}

Step 'Done'
Say '  open a new terminal: the old PATH stays in the current one'
