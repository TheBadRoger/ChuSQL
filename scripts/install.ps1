# ChuSQL 安装脚本（Windows）。
#
# 包目录结构（解压后就是这个样子）：
#   bin/         chusql-storage.exe（必装）、chusql-web.exe 或 csql.exe（二选一）
#   static/      Web 前端静态资源（装 web 组件时带）
#   scripts/     chusql.toml、init.sql
#   csql-web.ps1
#   install.ps1
#
# 安装 = 释放到目标目录 + 配好环境变量 + 写全局配置，装完把包目录删掉。
# storage/engine 一定装；web 与 cli 二选一。

[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)][ValidateSet('web', 'cli')][string]$Component,
    [string]$InstallDir = '',
    [string]$DataDir = '',
    [string]$RootUser = 'root',
    [string]$RootPassword = '',
    [switch]$KeepPackage
)

$ErrorActionPreference = 'Stop'

function Fail([string]$msg) { Write-Host "!! $msg" -ForegroundColor Red; exit 1 }
function Step([string]$msg) { Write-Host ''; Write-Host "==> $msg" -ForegroundColor Cyan }
function Say([string]$msg) { Write-Host $msg }

$pack = Split-Path -Parent $MyInvocation.MyCommand.Path
if (-not $InstallDir) { $InstallDir = Join-Path $env:LOCALAPPDATA 'ChuSQL' }
if (-not $DataDir) { $DataDir = Join-Path $InstallDir 'data' }
$InstallDir = [System.IO.Path]::GetFullPath($InstallDir)
$DataDir = [System.IO.Path]::GetFullPath($DataDir)

# ---- 校验包内容 ----
$storageExe = Join-Path $pack 'bin\chusql-storage.exe'
if (-not (Test-Path $storageExe)) { Fail "package is incomplete: bin\chusql-storage.exe not found in $pack" }
if ($Component -eq 'web') {
    if (-not (Test-Path (Join-Path $pack 'bin\chusql-web.exe'))) { Fail 'package is incomplete: bin\chusql-web.exe not found' }
    if (-not (Test-Path (Join-Path $pack 'static'))) { Fail 'package is incomplete: static\ not found' }
} else {
    if (-not (Test-Path (Join-Path $pack 'bin\csql.exe'))) { Fail 'package is incomplete: bin\csql.exe not found' }
}
$template = Join-Path $pack 'scripts\chusql.toml'
if (-not (Test-Path $template)) { Fail 'package is incomplete: scripts\chusql.toml not found' }

Say "ChuSQL installer"
Say ("  component  {0}" -f $Component)
Say ("  install to {0}" -f $InstallDir)
Say ("  data dir   {0}" -f $DataDir)
Say ("  root user  {0}{1}" -f $RootUser, $(if ($RootPassword) { '' } else { ' (no password: administrator-only sign-in)' }))

# ---- 释放文件 ----
Step 'Installing files'
New-Item -ItemType Directory -Force -Path (Join-Path $InstallDir 'bin'), (Join-Path $InstallDir 'logs'), $DataDir | Out-Null
Copy-Item (Join-Path $pack 'bin\*') (Join-Path $InstallDir 'bin') -Recurse -Force
if ($Component -eq 'web') {
    New-Item -ItemType Directory -Force -Path (Join-Path $InstallDir 'static') | Out-Null
    Copy-Item (Join-Path $pack 'static\*') (Join-Path $InstallDir 'static') -Recurse -Force
    $launcher = Join-Path $pack 'csql-web.ps1'
    if (Test-Path $launcher) {
        Copy-Item $launcher $InstallDir -Force
    } else {
        Say '  note: csql-web.ps1 is not bundled yet; until it is, start bin\chusql-web.exe yourself'
    }
}

# ---- 写全局配置 ----
Step 'Writing chusql.toml'
$configDir = if ($env:APPDATA) { Join-Path $env:APPDATA 'ChuSQL' } else { Join-Path $InstallDir 'config' }
$configFile = Join-Path $configDir 'chusql.toml'
$toml = Get-Content -Path $template -Raw -Encoding UTF8
$toml = $toml -replace '(?m)^(\s*user\s*=\s*).*$', "`$1`"$RootUser`""
$toml = $toml -replace '(?m)^(\s*password\s*=\s*).*$', "`$1`"$RootPassword`""
$toml = $toml -replace '(?m)^(\s*data_dir\s*=\s*).*$', "`$1`"$($DataDir -replace '\\', '/')`""
New-Item -ItemType Directory -Force -Path $configDir | Out-Null
Set-Content -Path $configFile -Value $toml -Encoding UTF8
Say ("  config file  {0}" -f $configFile)
Copy-Item (Join-Path $pack 'scripts\init.sql') $InstallDir -Force -ErrorAction SilentlyContinue

# ---- 命令入口 ----
Step 'Creating commands'
if ($Component -eq 'web') {
    Set-Content -Path (Join-Path $InstallDir 'csql-web.cmd') -Encoding ASCII -Value @'
@echo off
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0csql-web.ps1" %*
'@
} else {
    Set-Content -Path (Join-Path $InstallDir 'csql.cmd') -Encoding ASCII -Value @'
@echo off
"%~dp0bin\csql.exe" %*
'@
}

# ---- 环境变量 ----
Step 'Updating PATH'
$userPath = [Environment]::GetEnvironmentVariable('Path', 'User')
if (-not $userPath) { $userPath = '' }
$entries = $userPath -split ';' | Where-Object { $_ }
if ($entries -notcontains $InstallDir) {
    $updated = (@($entries) + $InstallDir) -join ';'
    [Environment]::SetEnvironmentVariable('Path', $updated, 'User')
    Say ("added {0} to the user PATH (open a new terminal to pick it up)" -f $InstallDir)
} else {
    Say 'PATH already has the install directory'
}

# ---- 删掉安装包 ----
if (-not $KeepPackage) {
    Step 'Removing the package'
    $marker = Join-Path $pack 'bin\chusql-storage.exe'
    $safe = (Test-Path $marker) -and ($pack -ne $InstallDir) -and (-not $InstallDir.StartsWith($pack, [StringComparison]::OrdinalIgnoreCase))
    if ($safe) {
        Remove-Item -Recurse -Force $pack
        Say 'package removed'
    } else {
        Say ("kept the package (not safe to delete automatically): {0}" -f $pack)
    }
}

Step 'Done'
Say ("  chusql.toml  {0}" -f $configFile)
Say ("  data         {0}" -f $DataDir)
if ($Component -eq 'web') { Say '  start with   csql-web' } else { Say '  start with   csql' }
