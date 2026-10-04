[CmdletBinding()]
param(
    [string]$OutDir = '',
    [string]$Platform = '',
    [string]$Version = '',
    [switch]$NoBuild,
    [switch]$NoVerify
)

$ErrorActionPreference = 'Stop'

# Windows 打包脚本：构建、装配、打 zip、出 sha256，产物落 releases\，命名与 CI 一致。

# stack / GHC 的诊断带圆点等非 ASCII 字符，输出统一按 UTF-8，免得写管道时炸编码
$env:GHC_CHARENC = 'UTF-8'
try { [Console]::OutputEncoding = [System.Text.Encoding]::UTF8 } catch { }
$OutputEncoding = [System.Text.Encoding]::UTF8

$repoRoot = (Resolve-Path (Join-Path $PSScriptRoot '..')).Path
if (-not $OutDir) { $OutDir = Join-Path $repoRoot 'releases' }
$OutDir = [System.IO.Path]::GetFullPath($OutDir)

# 打印错误并退出 1。
function Fail([string]$msg) {
    Write-Host "!! $msg" -ForegroundColor Red
    exit 1
}
# 打印步骤标题。
function Step([string]$msg) { Write-Host ''; Write-Host "==> $msg" -ForegroundColor Cyan }
# 打印一行输出。
function Say([string]$msg) { Write-Host $msg }

if (-not $Platform) {
    $arch = if ($env:PROCESSOR_ARCHITECTURE -eq 'ARM64') { 'arm64' } else { 'x86_64' }
    $Platform = "windows-$arch"
}
if ($Platform -notlike 'windows-*') {
    Fail 'the Linux / macOS package is scripts/package.sh; this script only makes windows-* zips'
}
if (-not $Version) {
    $Version = ''
}

# 算文件的 sha256（小写）。
function Get-Sha256([string]$path) {
    (Get-FileHash -Path $path -Algorithm SHA256).Hash.ToLower()
}

# 归档里该有什么，这里先对一遍：两部分都得在，装配漏了东西别等发布出去才发现
# 核对归档里必需的文件都在。
function Assert-Stage([string]$stage) {
    $must = @(
        'bin\chusql_core_storage.dll',
        'bin\csql.exe',
        'bin\chusql-web.exe',
        'bin\chusql-server.exe',
        'bin\csql-bootstrap.exe',
        'static\index.html',
        'csql-web.ps1',
        'resources\settings.toml.windows',
        'install.ps1',
        'install.sh',
        'uninstall.ps1',
        'uninstall.sh'
    )
    foreach ($rel in $must) {
        if (-not (Test-Path (Join-Path $stage $rel))) {
            Fail "$rel is missing from the package"
        }
    }
    # bin 里带 chusql 名字的文件只能是已知产物；改名前的残留或别的垃圾都算装配错误
    $known = @('chusql_core_storage.dll', 'chusql-server.exe', 'chusql-web.exe', 'csql-bootstrap.exe')
    $unknown = @(Get-ChildItem (Join-Path $stage 'bin') -Filter 'chusql*' -File -ErrorAction SilentlyContinue |
        Where-Object { $known -notcontains $_.Name })
    if ($unknown.Count -gt 0) {
        $names = ($unknown | ForEach-Object Name) -join ', '
        Fail "unexpected artifacts in bin/: $names"
    }
}

# 真装一遍，核对落地与未选组件。
function Test-Installed([string]$name, [string]$comp) {
    $archive = Join-Path $OutDir "$name.zip"
    $vtmp = Join-Path ([System.IO.Path]::GetTempPath()) ("chusql-packcheck-" + [Guid]::NewGuid().ToString('N'))
    $pkg = Join-Path $vtmp 'pkg'
    $opt = Join-Path $vtmp 'opt'
    $data = Join-Path $vtmp 'data'
    $appdata = Join-Path $vtmp 'appdata'
    New-Item -ItemType Directory -Force -Path $pkg, $opt, $appdata | Out-Null

    $oldAppData = $env:APPDATA
    $oldLocalAppData = $env:LOCALAPPDATA
    $oldUserPath = [Environment]::GetEnvironmentVariable('Path', 'User')
    $log = Join-Path $vtmp 'install.log'
    try {
        Expand-Archive -Path $archive -DestinationPath $pkg -Force
        $env:APPDATA = $appdata
        $env:LOCALAPPDATA = $vtmp
        $pwsh = Join-Path $PSHOME 'pwsh.exe'
        if (-not (Test-Path $pwsh)) { $pwsh = 'powershell' }
        & $pwsh -NoProfile -File (Join-Path $pkg 'install.ps1') -Component $comp -InstallDir $opt `
            -DataDir $data -RootUser root -Password 'verify-package' -NoStart *>&1 | Out-File -FilePath $log -Encoding UTF8
        if ($LASTEXITCODE -ne 0) {
            Get-Content $log | Select-Object -First 120 | Write-Host
            Fail "$name`: installing the $comp part from the package failed (log above)"
        }
        $ok = $true
        $checks = @(
            (Join-Path $opt 'bin\chusql_core_storage.dll'),
            (Join-Path $opt 'bin\chusql-server.exe'),
            (Join-Path $opt 'bin\csql-bootstrap.exe'),
            (Join-Path $data 'system\catalog.json'),
            (Join-Path $data 'system\__system_identities.db'),
            (Join-Path $data 'system\__system_users.db'),
            (Join-Path $data 'system\__system_types.db'),
            (Join-Path $appdata 'ChuSQL\settings.toml')
        )
        $forbidden = @()
        if ($comp -eq 'web') {
            $checks += @(
                (Join-Path $opt 'bin\chusql-web.exe'),
                (Join-Path $opt 'static\index.html'),
                (Join-Path $opt 'csql-web.ps1'),
                (Join-Path $opt 'csql-web.cmd')
            )
            $forbidden = @((Join-Path $opt 'bin\csql.exe'), (Join-Path $opt 'csql.cmd'))
        } else {
            $checks += @(
                (Join-Path $opt 'bin\csql.exe'),
                (Join-Path $opt 'csql.cmd')
            )
            $forbidden = @((Join-Path $opt 'bin\chusql-web.exe'), (Join-Path $opt 'static'), (Join-Path $opt 'csql-web.cmd'))
        }
        foreach ($f in $checks) { if (-not (Test-Path $f)) { $ok = $false; Write-Host "  missing: $f" } }
        foreach ($f in $forbidden) { if (Test-Path $f) { $ok = $false; Write-Host "  should not be there: $f" } }
        $systemCatalog = Get-Content -LiteralPath (Join-Path $data 'system\catalog.json') -Raw | ConvertFrom-Json
        if (-not $systemCatalog.tables.__system_types.system -or $systemCatalog.tables.__system_types.row_count -lt 18) {
            Fail "$name`: preinstalled types were not initialized during installation"
        }
        $config = Join-Path $appdata 'ChuSQL\settings.toml'
        $expectDataDir = $data -replace '\\', '/'
        if (-not (Test-Path $config) -or -not (Select-String -Path $config -SimpleMatch "data_dir = ""$expectDataDir""" -Quiet)) {
            $ok = $false
            Write-Host "  config does not point at $expectDataDir"
        }
        if (-not $ok) {
            Get-Content $log | Select-Object -First 120 | Write-Host
            Fail "$name`: the $comp part is not what the installer promises (log above)"
        }
        # 装的时候往用户 PATH 里写过一行，收尾要原样写回
        $nowUserPath = [Environment]::GetEnvironmentVariable('Path', 'User')
        if ($nowUserPath -ne $oldUserPath) {
            [Environment]::SetEnvironmentVariable('Path', $oldUserPath, 'User')
            Write-Host "  note: the installer touched the user PATH; restored it"
        }
    } finally {
        $env:APPDATA = $oldAppData
        $env:LOCALAPPDATA = $oldLocalAppData
        Remove-Item -Recurse -Force $vtmp -ErrorAction SilentlyContinue
    }
    Say "  ok         $name installs the $comp part, and only the $comp part"
}

# ---------- 构建 ----------
if (-not $NoBuild) {
    Step 'Building storage (Rust)'
    Push-Location (Join-Path $repoRoot 'chusql-core\storage')
    try {
        & cargo build --release
        if ($LASTEXITCODE -ne 0) { Fail 'cargo build --release failed' }
    } finally { Pop-Location }

    foreach ($target in @('chusql-web:exe:chusql-web', 'chusql-cli:exe:csql', 'chusql-server:exe:chusql-server')) {
        Step "Building $target (Haskell)"
        Push-Location (Join-Path $repoRoot 'chusql-cli')
        try {
            & stack build --fast $target
            if ($LASTEXITCODE -ne 0) { Fail "stack build --fast $target failed" }
        } finally { Pop-Location }
    }

    Step 'Building chusql-bootstrap:exe:csql-bootstrap (Haskell)'
    Push-Location (Join-Path $repoRoot 'chusql-bootstrap')
    try {
        & stack build --fast chusql-bootstrap:exe:csql-bootstrap
        if ($LASTEXITCODE -ne 0) { Fail 'stack build --fast csql-bootstrap failed' }
    } finally { Pop-Location }
}

# ---------- 装配 + 打归档 ----------
$name = "chusql-$Platform"
$stageRoot = Join-Path $OutDir '.stage'
$stage = Join-Path $stageRoot $name
$zip = Join-Path $OutDir "$name.zip"
New-Item -ItemType Directory -Force -Path $OutDir | Out-Null

try {
    Step "Packing $name"
    if (Test-Path $stageRoot) { Remove-Item -Recurse -Force $stageRoot }
    New-Item -ItemType Directory -Force -Path (Join-Path $stage 'bin'), (Join-Path $stage 'resources') | Out-Null

    $storage = Join-Path $repoRoot 'chusql-core\storage\target\release\chusql_core_storage.dll'
    if (-not (Test-Path $storage)) { Fail 'chusql-core\storage\target\release\chusql_core_storage.dll is not built (run without -NoBuild)' }
    Copy-Item $storage (Join-Path $stage 'bin')

    # 从真正用于构建的工程目录取安装根（package.sh 同样锚在 chusql-cli），别靠调用者的当前目录
    Push-Location (Join-Path $repoRoot 'chusql-cli')
    try { $stackRoot = (& stack path --local-install-root | Select-Object -Last 1).Trim() } finally { Pop-Location }
    foreach ($frontName in @('chusql-web.exe', 'chusql-server.exe', 'csql.exe')) {
        $front = Join-Path $stackRoot "bin/$frontName"
        if (-not (Test-Path $front)) {
            $front = Get-ChildItem -Path (Join-Path $repoRoot 'chusql-web\.stack-work'), (Join-Path $repoRoot 'chusql-cli\.stack-work'), (Join-Path $repoRoot 'chusql-server\.stack-work') `
                -Recurse -Filter $frontName -File -ErrorAction SilentlyContinue |
                Sort-Object LastWriteTime -Descending | Select-Object -First 1 -ExpandProperty FullName
        }
        if (-not $front -or -not (Test-Path $front)) { Fail "$frontName is not built (run without -NoBuild)" }
        Copy-Item $front (Join-Path $stage 'bin')
    }

    # 引导程序住在自己的工程里，安装根也单独问它要
    Push-Location (Join-Path $repoRoot 'chusql-bootstrap')
    try { $bootstrapRoot = (& stack path --local-install-root | Select-Object -Last 1).Trim() } finally { Pop-Location }
    $bootstrapExe = Join-Path $bootstrapRoot 'bin/csql-bootstrap.exe'
    if (-not (Test-Path $bootstrapExe)) {
        $bootstrapExe = Get-ChildItem -Path (Join-Path $repoRoot 'chusql-bootstrap\.stack-work') `
            -Recurse -Filter 'csql-bootstrap.exe' -File -ErrorAction SilentlyContinue |
            Sort-Object LastWriteTime -Descending | Select-Object -First 1 -ExpandProperty FullName
    }
    if (-not $bootstrapExe -or -not (Test-Path $bootstrapExe)) { Fail 'csql-bootstrap.exe is not built (run without -NoBuild)' }
    Copy-Item $bootstrapExe (Join-Path $stage 'bin')

    # 第三方 dll 才要连带上：名字带 chusql 的是本 crate 产物或改名前的残留，上面已按名拷过
    Get-ChildItem (Join-Path $repoRoot 'chusql-core\storage\target\release') -Filter '*.dll' -File -ErrorAction SilentlyContinue |
        Where-Object { $_.Name -notlike 'chusql*' } |
        ForEach-Object { Copy-Item $_.FullName (Join-Path $stage 'bin') }

    Copy-Item (Join-Path $repoRoot 'resources\settings.toml.windows') (Join-Path $stage 'resources')
    Copy-Item (Join-Path $repoRoot 'scripts\install.ps1'), (Join-Path $repoRoot 'scripts\install.sh') $stage
    Copy-Item (Join-Path $repoRoot 'scripts\uninstall.ps1'), (Join-Path $repoRoot 'scripts\uninstall.sh') $stage
    New-Item -ItemType Directory -Force -Path (Join-Path $stage 'static') | Out-Null
    Copy-Item (Join-Path $repoRoot 'chusql-web\static\*') (Join-Path $stage 'static') -Recurse
    Copy-Item (Join-Path $repoRoot 'scripts\csql-web.sh') $stage
    $webPs1 = Join-Path $repoRoot 'scripts\csql-web.ps1'
    if (Test-Path $webPs1) { Copy-Item $webPs1 $stage }

    Assert-Stage $stage

    if (Test-Path $zip) { Remove-Item -Force $zip }
    if (Test-Path "$zip.sha256") { Remove-Item -Force "$zip.sha256" }
    Compress-Archive -Path (Join-Path $stage '*') -DestinationPath $zip -Force
    Remove-Item -Recurse -Force $stageRoot

    $hash = Get-Sha256 $zip
    Set-Content -Path "$zip.sha256" -Value ("{0}  {1}" -f $hash, "$name.zip") -NoNewline -Encoding ASCII
    Say "  wrote      $zip"
    Say "  checksum   $hash"

    if (-not $NoVerify) {
        Test-Installed $name 'cli'
        Test-Installed $name 'web'
    }
} finally {
    if (Test-Path $stageRoot) { Remove-Item -Recurse -Force $stageRoot }
}

# ---------- 收尾 ----------
Step 'Done'
Get-ChildItem (Join-Path $OutDir "$name.zip*") | Select-Object Name, Length | Format-Table
if ($Version) {
    Say ''
    Say "release $Version with (tag 必须是真实存在的 git tag):"
    Say "  gh release create $Version `"$OutDir\*`" --generate-notes"
} else {
    Say ''
    Say 'publish with (先用 -Version 指定 tag):'
    Say "  gh release create <tag> `"$OutDir\*`" --generate-notes"
}
