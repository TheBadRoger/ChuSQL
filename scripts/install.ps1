[CmdletBinding()]
param(
    [string]$Component = '',
    [string]$InstallDir = '',
    [string]$DataDir = '',
    [string]$RootUser = 'root',
    [string]$RootPassword = '',
    [string]$Version = 'latest',
    [string]$Repo = 'TheBadRoger/ChuSQL',
    [switch]$ListVersions,
    [switch]$Interactive,
    [switch]$KeepPackage
)

$ErrorActionPreference = 'Stop'

# Windows 安装脚本：从包内或 GitHub 发行版装 cli/web，
# 写全局配置、改用户 PATH，装完删除安装包。

$script:tempDir = ''

# 报错退出，并清掉半截临时目录。
function Fail([string]$msg) {
    Write-Host "!! $msg" -ForegroundColor Red
    if ($script:tempDir -and (Test-Path $script:tempDir)) {
        Remove-Item -Recurse -Force $script:tempDir -ErrorAction SilentlyContinue
    }
    exit 1
}
# 打印步骤标题。
function Step([string]$msg) { Write-Host ''; Write-Host "==> $msg" -ForegroundColor Cyan }
# 打印一行输出。
function Say([string]$msg) { Write-Host $msg }
# 打印缩进提示行。
function Note([string]$msg) { Write-Host "  note: $msg" }

# ---- 发行站、平台标签 ----
$githubBase = if ($env:CHUSQL_GITHUB_BASE) { $env:CHUSQL_GITHUB_BASE.TrimEnd('/') } else { 'https://github.com' }
$apiBase = if ($githubBase -match '^https?://github\.com$') { 'https://api.github.com' } else { "$githubBase/api/v3" }
# 查版本走 API，未认证只有 60 次/小时；给了 token 就带上（CI 里 GITHUB_TOKEN 一般就有）
$githubToken = if ($env:CHUSQL_GITHUB_TOKEN) { $env:CHUSQL_GITHUB_TOKEN } elseif ($env:GITHUB_TOKEN) { $env:GITHUB_TOKEN } else { '' }
$apiHeaders = @{ 'User-Agent' = 'chusql-install'; 'Accept' = 'application/vnd.github+json' }
if ($githubToken) { $apiHeaders['Authorization'] = "Bearer $githubToken" }
$arch = if ($env:PROCESSOR_ARCHITECTURE -eq 'ARM64') { 'arm64' } else { 'x86_64' }
$labels = @("windows-$arch", 'windows')

# 取一份 release 的资源文件名列表。
function Get-AssetNames($release) { @($release.assets | ForEach-Object { $_.name }) }

# 这份 release 里有没有本平台的包。
function Test-HasPackage($release) {
    $names = Get-AssetNames $release
    foreach ($label in $labels) {
        if ($names -contains "chusql-$label.zip") { return $true }
    }
    return ($names -contains 'chusql.zip')
}

# 按正式版与带包两档条件挑第一个 tag。
function Select-Tag([bool]$stableOnly, [bool]$needPackage, $releases) {
    foreach ($release in $releases) {
        if ($stableOnly -and $release.prerelease) { continue }
        if ($needPackage -and -not (Test-HasPackage $release)) { continue }
        return $release.tag_name
    }
    return ''
}

# 取 URL 的文本内容（字节数组先转字符串）。
function Get-ResponseText([string]$url) {
    $response = Invoke-WebRequest -Uri $url -Headers $apiHeaders -UseBasicParsing
    if ($response.Content -is [byte[]]) { return [System.Text.Encoding]::UTF8.GetString($response.Content) }
    return [string]$response.Content
}

# 拉取 release 列表并逐个输出。
function Get-Releases {
    try { $text = Get-ResponseText "$apiBase/repos/$Repo/releases?per_page=100" } catch { return }
    try { $parsed = ConvertFrom-Json -InputObject $text } catch { return }
    if ($null -eq $parsed) { return }
    foreach ($release in @($parsed)) {
        if ($null -ne $release) { Write-Output $release }
    }
}

# ---- 列版本 ----
if ($ListVersions) {
    Say ("ChuSQL releases in {0}" -f $Repo)
    $releases = @(Get-Releases)
    if ($releases.Count -eq 0) {
        Fail "cannot list the releases of $Repo from $apiBase`n   (offline, rate limited, or the repository has no release yet)`n   open $githubBase/$Repo/releases and pass a tag: -Version <tag>"
    }
    foreach ($release in $releases) {
        $mark = ''
        if ($release.prerelease) { $mark += ' (prerelease)' }
        if (-not (Test-HasPackage $release)) { $mark += " (no package for windows-$arch)" }
        Say ("  {0}{1}" -f $release.tag_name, $mark)
    }
    Say ''
    Say 'install one with: .\install.ps1 -Version <tag>'
    exit 0
}

if ($Component -notin @('web', 'cli', 'both', '')) { Fail "pass -Component web, cli or both (got: $Component)" }

# ---- 交互：控制台里直接问答；-Interactive 时没有终端也从 stdin 逐行读（自动化用）----
$askFromStdin = $false
$canPrompt = $false
if ($Interactive) { $askFromStdin = $true; $canPrompt = $true }
elseif (-not [Console]::IsInputRedirected) { $canPrompt = $true }

# 读一行输入（可选从 stdin 读）。
function Ask-Line([string]$prompt) {
    if ($askFromStdin) {
        Write-Host $prompt -NoNewline
        $line = [Console]::In.ReadLine()
        if ($null -eq $line) { $line = '' }
        return $line
    }
    return (Read-Host $prompt)
}

# 问 y/n，回车用缺省值，返回布尔。
function Ask-YesNo([string]$prompt, [bool]$defaultYes) {
    while ($true) {
        $answer = (Ask-Line $prompt).Trim()
        if ($answer -eq '') { return $defaultYes }
        if ($answer -match '^(y|yes)$') { return $true }
        if ($answer -match '^(n|no)$') { return $false }
        Write-Host '  please answer y or n'
    }
}

# 读取隐藏输入的密码。
function Ask-Secret([string]$prompt) {
    if ($askFromStdin) { return (Ask-Line $prompt) }
    $secure = Read-Host $prompt -AsSecureString
    $bstr = [System.Runtime.InteropServices.Marshal]::SecureStringToBSTR($secure)
    try { return [System.Runtime.InteropServices.Marshal]::PtrToStringBSTR($bstr) }
    finally { [System.Runtime.InteropServices.Marshal]::ZeroFreeBSTR($bstr) }
}

$wantWeb = $false
$wantCli = $false
if ($Component -eq 'web') { $wantWeb = $true }
if ($Component -eq 'cli') { $wantCli = $true }
if ($Component -eq 'both') { $wantWeb = $true; $wantCli = $true }

if (-not $Component) {
    if (-not $canPrompt) {
        Fail '-Component is required (web, cli or both) when there is no terminal to ask on'
    }
    Write-Host ''
    Write-Host 'ChuSQL installer: which parts do you want?'
    $wantCli = Ask-YesNo 'Install the command line client (csql)? [Y/n]' $true
    $wantWeb = Ask-YesNo 'Install the web front end (browser UI)? [y/N]' $false
    if (-not $wantWeb -and -not $wantCli) {
        Say '  neither selected: only the storage library and the server get installed'
    }
}

if (-not $PSBoundParameters.ContainsKey('RootPassword') -and $canPrompt) {
    Write-Host ''
    Write-Host ('The administrator account is "{0}".' -f $RootUser)
    $firstPassword = Ask-Secret ('Root password for {0} (empty means no password) []:' -f $RootUser)
    if ($firstPassword) {
        $againPassword = Ask-Secret 'Repeat the root password:'
        if ($firstPassword -ne $againPassword) { Fail 'the two passwords do not match' }
        Write-Host 'root password set'
    } else {
        Write-Host 'no root password: administrator-only sign-in'
    }
    $RootPassword = $firstPassword
}

$wanted = @()
if ($wantCli) { $wanted += 'cli' }
if ($wantWeb) { $wanted += 'web' }
$componentLabel = ($wanted -join '+')
if (-not $componentLabel) { $componentLabel = 'none (storage only)' }

$pack = Split-Path -Parent $MyInvocation.MyCommand.Path
$downloaded = $false
$tempDir = ''

if (-not $InstallDir) { $InstallDir = Join-Path $env:LOCALAPPDATA 'ChuSQL' }
if (-not $DataDir) { $DataDir = Join-Path $InstallDir 'data' }
$InstallDir = [System.IO.Path]::GetFullPath($InstallDir)
$DataDir = [System.IO.Path]::GetFullPath($DataDir)

# ---- 取包：脚本旁边有包就装本地的，否则去发行站下载 ----
if (-not (Test-Path (Join-Path $pack 'bin\chusql_core_storage.dll'))) {
    Step 'Looking for the package'

    if ($Version -eq 'latest') {
        $releases = @(Get-Releases)
        if ($releases.Count -eq 0) {
            Note "version    latest (cannot reach $apiBase; using the latest release redirect)"
        } else {
            $picked = Select-Tag $true $true $releases
            if (-not $picked) { $picked = Select-Tag $false $true $releases }
            if (-not $picked) { $picked = Select-Tag $true $false $releases }
            if (-not $picked) { $picked = Select-Tag $false $false $releases }
            if ($picked) { $Version = $picked; Note "version    $Version" }
            else { Note 'no release lists a matching package; falling back to the latest release redirect' }
        }
    }

    $tempDir = Join-Path ([System.IO.Path]::GetTempPath()) ("chusql-install-" + [Guid]::NewGuid().ToString('N'))
    $script:tempDir = $tempDir
    New-Item -ItemType Directory -Force -Path $tempDir | Out-Null

    # 一个平台一个包：web 和 cli 都在里面，装哪几个由上面的选择决定
    Step "Looking for a chusql package for windows-$arch"
    $zip = Join-Path $tempDir 'chusql.zip'
    $asset = ''
    $assetUrl = ''
    $candidates = @()
    foreach ($label in $labels) { $candidates += "chusql-$label.zip" }
    $candidates += 'chusql.zip'
    foreach ($candidate in $candidates) {
        $tryUrl = if ($Version -eq 'latest') { "$githubBase/$Repo/releases/latest/download/$candidate" }
        else { "$githubBase/$Repo/releases/download/$Version/$candidate" }
        try {
            Invoke-WebRequest -Uri $tryUrl -Headers $apiHeaders -OutFile $zip -UseBasicParsing
            $asset = $candidate
            $assetUrl = $tryUrl
            break
        } catch {
            continue
        }
    }
    if (-not $asset) {
        Fail "no package for windows-$arch in $Repo ($Version); try -ListVersions, or install from an archive you downloaded yourself"
    }
    Note "asset      $asset"

    $published = ''
    try { $published = Get-ResponseText "$assetUrl.sha256" } catch { $published = '' }
    if ($published) {
        $expected = ($published.Trim() -split '\s+')[0].ToLower()
        $actual = (Get-FileHash -Path $zip -Algorithm SHA256).Hash.ToLower()
        if ($actual -ne $expected) { Fail "checksum mismatch: expected $expected, got $actual" }
        Note "checksum   $expected (ok)"
    } else {
        Note 'checksum   no .sha256 published for this asset (skipped)'
    }

    Step 'Unpacking the package'
    $expanded = Join-Path $tempDir 'package'
    New-Item -ItemType Directory -Force -Path $expanded | Out-Null
    Expand-Archive -Path $zip -DestinationPath $expanded -Force
    if (-not (Test-Path (Join-Path $expanded 'bin\chusql_core_storage.dll'))) {
        # 归档里多包了一层目录也能装
        $inner = Get-ChildItem -Path $expanded -Directory | Select-Object -First 1
        if ($inner -and (Test-Path (Join-Path $inner.FullName 'bin\chusql_core_storage.dll'))) { $expanded = $inner.FullName }
    }
    $pack = $expanded
    $downloaded = $true
}

# ---- 校验包内容 ----
$storageLib = Join-Path $pack 'bin\chusql_core_storage.dll'
if (-not (Test-Path $storageLib)) { Fail "package is incomplete: bin\chusql_core_storage.dll not found in $pack" }
if (-not (Test-Path (Join-Path $pack 'bin\chusql-server.exe'))) { Fail 'package is incomplete: bin\chusql-server.exe not found' }
if ($wantWeb) {
    if (-not (Test-Path (Join-Path $pack 'bin\chusql-web.exe'))) { Fail 'package is incomplete: bin\chusql-web.exe not found' }
    if (-not (Test-Path (Join-Path $pack 'static'))) { Fail 'package is incomplete: static\ not found' }
}
if ($wantCli) {
    if (-not (Test-Path (Join-Path $pack 'bin\csql.exe'))) { Fail 'package is incomplete: bin\csql.exe not found' }
}
$template = Join-Path $pack 'scripts\chusql.toml'
if (-not (Test-Path $template)) { Fail 'package is incomplete: scripts\chusql.toml not found' }

Say "ChuSQL installer"
Say ("  version    {0}" -f $Version)
Say ("  components {0}" -f $componentLabel)
if ($downloaded) { Say ("  release    {0}" -f $Version) }
Say ("  install to {0}" -f $InstallDir)
Say ("  data dir   {0}" -f $DataDir)
Say ("  root user  {0}{1}" -f $RootUser, $(if ($RootPassword) { '' } else { ' (no password: administrator-only sign-in)' }))

# ---- 释放文件 ----
# 包是合在一起的（web 和 cli 都在），这里按这次的选择逐个释放：没选的组件不落地
Step 'Installing files'
New-Item -ItemType Directory -Force -Path (Join-Path $InstallDir 'bin'), (Join-Path $InstallDir 'logs'), $DataDir | Out-Null
Copy-Item $storageLib (Join-Path $InstallDir 'bin') -Force
Copy-Item (Join-Path $pack 'bin\chusql-server.exe') (Join-Path $InstallDir 'bin') -Force
Get-ChildItem (Join-Path $pack 'bin') -Filter '*.dll' -File -ErrorAction SilentlyContinue |
    ForEach-Object { Copy-Item $_.FullName (Join-Path $InstallDir 'bin') -Force }
if ($wantCli) {
    Copy-Item (Join-Path $pack 'bin\csql.exe') (Join-Path $InstallDir 'bin') -Force
}
if ($wantWeb) {
    Copy-Item (Join-Path $pack 'bin\chusql-web.exe') (Join-Path $InstallDir 'bin') -Force
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
if ($wantWeb) {
    Set-Content -Path (Join-Path $InstallDir 'csql-web.cmd') -Encoding ASCII -Value @'
@echo off
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0csql-web.ps1" %*
'@
}
if ($wantCli) {
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
    if ($downloaded) {
        Remove-Item -Recurse -Force $tempDir -ErrorAction SilentlyContinue
        Say 'package removed'
    } else {
        $marker = Join-Path $pack 'bin\chusql_core_storage.dll'
        $safe = (Test-Path $marker) -and ($pack -ne $InstallDir) -and (-not $InstallDir.StartsWith($pack, [StringComparison]::OrdinalIgnoreCase))
        if ($safe) {
            Remove-Item -Recurse -Force $pack
            Say 'package removed'
        } else {
            Say ("kept the package (not safe to delete automatically): {0}" -f $pack)
        }
    }
} elseif ($downloaded) {
    Say ("kept the downloaded package: {0}" -f $tempDir)
}

Step 'Done'
Say ("  chusql.toml  {0}" -f $configFile)
Say ("  data         {0}" -f $DataDir)
$startWith = @()
if ($wantWeb) { $startWith += 'csql-web' }
if ($wantCli) { $startWith += 'csql' }
Say ("  start with   {0}" -f ($startWith -join ', '))
Say ("  tcp server   {0} (listens on [server] host/port, defaults 127.0.0.1:7777)" -f (Join-Path $InstallDir 'bin\chusql-server.exe'))
