#Requires -Version 5.1

[CmdletBinding()]
param(
    [Parameter(Position = 0)][string]$Command = 'web',
    [Parameter(Position = 1)][string]$Action = '',
    [Parameter(Position = 2)][string]$Key = '',
    [Parameter(Position = 3)][string]$Value = '',
    [Parameter(ValueFromRemainingArguments = $true)][string[]]$Rest,

    [string]$Port = '',
    [string]$HostName = '',
    [string]$User = '',
    [string]$Password = '',
    [string]$PasswordHash = '',
    [string]$StaticDir = '',
    [string]$CookieSecure = '',
    [string]$BodyLimit = '',
    [string]$SessionIdle = '',
    [string]$SessionMax = '',
    [string]$LoginMaxAttempts = '',
    [string]$LoginWindow = '',
    [string]$RowsPerPage = '',
    [string]$MaxPageSize = '',
    [string]$MaxRows = '',
    [string]$MaxSqlLength = '',
    [string]$DataDir = '',
    [string]$PipeName = '',
    [string]$StoragePageSize = '',
    [string]$StorageBtreeOrder = '',
    [string]$StorageBufferPool = '',
    [string]$StorageLog = '',
    [string]$Seed = '',
    [switch]$NoBrowser,
    [switch]$SkipBuild
)

$ErrorActionPreference = 'Stop'
$ScriptVersion = '0.2.0'
$root = Split-Path -Parent $PSScriptRoot

# ChuSQL 启动器 / CLI 前端：解析命令与设置，
# 启动 Rust 存储进程与 Haskell Web 服务，
# 把子进程日志回显到本窗口。

function Say([string]$msg) { Write-Host $msg }
function Step([string]$msg) { Write-Host ''; Write-Host "==> $msg" -ForegroundColor Cyan }
function Warn([string]$msg) { Write-Host $msg -ForegroundColor Yellow }
function Fail([string]$msg) { Write-Host "!! $msg" -ForegroundColor Red; exit 1 }

$SettingKeys = @(
    'port', 'host', 'user', 'password', 'password-hash', 'static-dir', 'cookie-secure', 'body-limit',
    'session-idle', 'session-max', 'login-max-attempts', 'login-window',
    'rows-per-page', 'max-page-size', 'max-rows', 'max-sql-length',
    'data-dir', 'pipe-name',
    'storage-page-size', 'storage-btree-order', 'storage-buffer-pool', 'storage-log',
    'seed'
)

$Settings = [ordered]@{
    'port'                = @{ Env = 'CHUSQL_WEB_PORT';            Default = '7777';  Kind = 'int' }
    'host'                = @{ Env = 'CHUSQL_WEB_HOST';            Default = '127.0.0.1'; Kind = 'text' }
    'user'                = @{ Env = 'CHUSQL_WEB_USER';            Default = 'root';  Kind = 'text' }
    'password'            = @{ Env = 'CHUSQL_WEB_PASSWORD';        Default = '';      Kind = 'secret' }
    'password-hash'       = @{ Env = 'CHUSQL_WEB_PASSWORD_HASH';   Default = '';      Kind = 'secret' }
    'static-dir'          = @{ Env = 'CHUSQL_WEB_STATIC';          Default = 'static'; Kind = 'text' }
    'cookie-secure'       = @{ Env = 'CHUSQL_WEB_COOKIE_SECURE';   Default = '0';     Kind = 'bool' }
    'body-limit'          = @{ Env = 'CHUSQL_WEB_BODY_LIMIT';      Default = '65536'; Kind = 'int' }
    'session-idle'        = @{ Env = 'CHUSQL_WEB_SESSION_IDLE';    Default = '28800'; Kind = 'int' }
    'session-max'         = @{ Env = 'CHUSQL_WEB_SESSION_MAX';     Default = '86400'; Kind = 'int' }
    'login-max-attempts'  = @{ Env = 'CHUSQL_WEB_LOGIN_MAX_ATTEMPTS'; Default = '5';  Kind = 'int' }
    'login-window'        = @{ Env = 'CHUSQL_WEB_LOGIN_WINDOW';    Default = '300';   Kind = 'int' }
    'rows-per-page'       = @{ Env = 'CHUSQL_WEB_PAGE_SIZE';       Default = '25';    Kind = 'int' }
    'max-page-size'       = @{ Env = 'CHUSQL_WEB_MAX_PAGE_SIZE';   Default = '500';   Kind = 'int' }
    'max-rows'            = @{ Env = 'CHUSQL_WEB_MAX_ROWS';        Default = '1000';  Kind = 'int' }
    'max-sql-length'      = @{ Env = 'CHUSQL_WEB_MAX_SQL_LENGTH';  Default = '20000'; Kind = 'int' }
    'data-dir'            = @{ Env = 'CHUSQL_DATA_DIR';            Default = '<repo>\localdata'; Kind = 'text' }
    'pipe-name'           = @{ Env = 'CHUSQL_PIPE';                Default = '';      Kind = 'text' }
    'storage-page-size'   = @{ Env = 'CHUSQL_PAGE_SIZE';           Default = '4096';  Kind = 'int' }
    'storage-btree-order' = @{ Env = 'CHUSQL_BTREE_ORDER';         Default = '4';     Kind = 'int' }
    'storage-buffer-pool' = @{ Env = 'CHUSQL_BUFFER_POOL_SIZE';    Default = '1024';  Kind = 'int' }
    'storage-log'         = @{ Env = 'CHUSQL_LOG';                 Default = 'info';  Kind = 'text' }
    'seed'                = @{ Env = 'CHUSQL_WEB_SEED';           Default = '1';     Kind = 'bool' }
}

$FlagValues = @{
    'port'                = $Port
    'host'                = $HostName
    'user'                = $User
    'password'            = $Password
    'password-hash'       = $PasswordHash
    'static-dir'          = $StaticDir
    'cookie-secure'       = $CookieSecure
    'body-limit'          = $BodyLimit
    'session-idle'        = $SessionIdle
    'session-max'         = $SessionMax
    'login-max-attempts'  = $LoginMaxAttempts
    'login-window'        = $LoginWindow
    'rows-per-page'       = $RowsPerPage
    'max-page-size'       = $MaxPageSize
    'max-rows'            = $MaxRows
    'max-sql-length'      = $MaxSqlLength
    'data-dir'            = $DataDir
    'pipe-name'           = $PipeName
    'storage-page-size'   = $StoragePageSize
    'storage-btree-order' = $StorageBtreeOrder
    'storage-buffer-pool' = $StorageBufferPool
    'storage-log'         = $StorageLog
    'seed'                = $Seed
}

$SettingsPath = if ($env:CHUSQL_SETTINGS_FILE) { $env:CHUSQL_SETTINGS_FILE } else { Join-Path $PSScriptRoot 'chusql.settings.json' }

function Read-SavedSettings {
    if (-not (Test-Path $SettingsPath)) { return @{} }
    try {
        $raw = Get-Content -Path $SettingsPath -Raw -Encoding UTF8
        if (-not $raw.Trim()) { return @{} }
        $obj = $raw | ConvertFrom-Json
        $map = @{}
        foreach ($k in $SettingKeys) {
            if ($obj.PSObject.Properties.Name -contains $k) { $map[$k] = [string]$obj.$k }
        }
        return $map
    } catch {
        Warn "settings file $SettingsPath is not readable JSON; ignoring it ($($_.Exception.Message))"
        return @{}
    }
}

function Save-SavedSettings($map) {
    $ordered = [ordered]@{}
    foreach ($k in $SettingKeys) { if ($map.ContainsKey($k)) { $ordered[$k] = $map[$k] } }
    ($ordered | ConvertTo-Json) | Set-Content -Path $SettingsPath -Encoding UTF8
}

function Get-EffectiveSettings {
    $saved = Read-SavedSettings
    $result = [ordered]@{}
    foreach ($k in $SettingKeys) {
        $def = $Settings[$k].Default
        $flag = $FlagValues[$k]
        $envName = $Settings[$k].Env
        $envValue = [System.Environment]::GetEnvironmentVariable($envName)
        if ($flag) {
            $result[$k] = @{ Value = $flag; Source = 'command line' }
        } elseif ($saved.ContainsKey($k)) {
            $result[$k] = @{ Value = $saved[$k]; Source = 'settings file' }
        } elseif ($envValue) {
            $result[$k] = @{ Value = $envValue; Source = 'environment' }
        } else {
            $result[$k] = @{ Value = $def; Source = 'default' }
        }
    }
    return $result
}

function Assert-Value([string]$key, [string]$value) {
    if ($value -eq '' -or $value -like '<*') { return }
    switch ($Settings[$key].Kind) {
        'int' {
            $n = 0
            if (-not [int]::TryParse($value, [ref]$n)) { Fail "setting '$key' needs an integer, got: $value" }
        }
        'bool' {
            if ($value.ToLower() -notin @('0', '1', 'true', 'false', 'yes', 'no', 'on', 'off')) {
                Fail "setting '$key' needs a boolean (0/1/true/false), got: $value"
            }
        }
    }
}

function Assert-Effective($eff) {
    foreach ($k in $SettingKeys) { Assert-Value $k $eff[$k].Value }
}

function Show-Settings($eff) {
    $note = ''
    if (-not (Test-Path $SettingsPath)) { $note = '  (not created yet)' }
    Say ''
    Say 'effective settings (command line > settings file > environment > default)'
    Say ("  settings file: {0}{1}" -f $SettingsPath, $note)
    Say ''
    foreach ($k in $SettingKeys) {
        $shown = if ($Settings[$k].Kind -eq 'secret' -and $eff[$k].Value) { '********' } else { $eff[$k].Value }
        Say ("  {0,-20} {1,-24} [{2}]" -f $k, $shown, $eff[$k].Source)
    }
    Say ''
}

function Show-Help {
    Say ("ChuSQL launcher / CLI  (version {0})" -f $ScriptVersion)
    Say ''
    Say 'usage: chusql.ps1 [command] [options]'
    Say ''
    Say 'commands:'
    Say '  web                  start the browser console (default)'
    Say '  cli                  CLI mode (not implemented yet; starts web instead)'
    Say '  config               show effective settings'
    Say '  config set KEY VAL   save a setting'
    Say '  config unset KEY     drop a saved setting'
    Say '  config reset         drop all saved settings'
    Say '  config path          print the settings file path'
    Say '  version | help'
    Say ''
    Say 'options (this run only):'
    Say '  -Port N               HTTP port                        default 7777'
    Say '  -HostName S           listen address                   default 127.0.0.1'
    Say '  -User S               account name                     default root'
    Say '  -Password S           password (plain; hashed at start) default chusql (demo)'
    Say '  -PasswordHash S       password hash (pbkdf2)            default none'
    Say '  -StaticDir D          static asset directory           default static'
    Say '  -CookieSecure B       add Secure to the session cookie default false'
    Say '  -BodyLimit N          max request body bytes           default 65536'
    Say '  -SessionIdle N        session idle timeout, sec        default 28800'
    Say '  -SessionMax N         session max age, sec             default 86400'
    Say '  -LoginMaxAttempts N   failed logins before 429         default 5'
    Say '  -LoginWindow N        lockout window, sec              default 300'
    Say '  -RowsPerPage N        rows per page by default         default 25'
    Say '  -MaxPageSize N        rows per page ceiling            default 500'
    Say '  -MaxRows N            rows returned per query          default 1000'
    Say '  -MaxSqlLength N       max SQL characters               default 20000'
    Say '  -DataDir D            data directory                   default <repo>\localdata'
    Say '  -PipeName S           named pipe name                  default chusql-joint-<timestamp>'
    Say '  -StoragePageSize N    storage page size (Rust)         default 4096'
    Say '  -StorageBtreeOrder N  B+tree order (Rust)              default 4'
    Say '  -StorageBufferPool N  buffer pool pages (Rust)         default 1024'
    Say '  -StorageLog S         storage log level (Rust)         default info'
    Say '  -Seed B               load demo data when the db is empty  default 1 (on)'
    Say '  -NoBrowser            do not open the browser'
    Say '  -SkipBuild            skip cargo/stack builds'
    Say ''
    Say 'examples:'
    Say '  chusql.ps1 web -Port 9000            # temporary port'
    Say '  chusql.ps1 config set port 9000      # make it sticky'
    Say '  chusql.ps1 config                    # see what is in effect'
    Say '  chusql.ps1 cli                       # CLI placeholder -> starts web'
    Say '  chusql.ps1 web -Seed 0                # start with an empty database'
    Say ''
    Say 'the password can also be changed from the web console (Settings -> Account);'
    Say 'that writes password-hash into the settings file and drops the plain password.'
}

function Invoke-Config([string]$cmd) {
    switch ($cmd.ToLower()) {
        '' {
            Show-Settings (Get-EffectiveSettings)
        }
        'list' { Show-Settings (Get-EffectiveSettings) }
        'path' { Say $SettingsPath }
        'set' {
            if (-not $Key) { Fail 'usage: chusql.ps1 config set KEY VALUE   (chusql.ps1 config set -h lists keys)' }
            if ($Key -in @('-h', '--help', 'help')) { Say ('keys: ' + ($SettingKeys -join ', ')); return }
            $k = $Key.ToLower()
            if ($SettingKeys -notcontains $k) { Fail "unknown setting '$Key' (keys: $($SettingKeys -join ', '))" }
            if (-not $Value) { Fail "config set $Key needs a value" }
            Assert-Value $k $Value
            $saved = Read-SavedSettings
            $saved[$k] = $Value
            Save-SavedSettings $saved
            $shown = if ($Settings[$k].Kind -eq 'secret') { '********' } else { $Value }
            Say "saved $k = $shown   ($SettingsPath)"
        }
        'unset' {
            if (-not $Key) { Fail 'usage: chusql.ps1 config unset KEY' }
            $k = $Key.ToLower()
            $saved = Read-SavedSettings
            if (-not $saved.ContainsKey($k)) { Warn "$k was not saved"; return }
            $saved.Remove($k)
            Save-SavedSettings $saved
            Say "removed $k"
        }
        'reset' {
            if (Test-Path $SettingsPath) { Remove-Item $SettingsPath -Force }
            Say "settings cleared ($SettingsPath)"
        }
        default { Fail "unknown config action '$cmd' (use: set / unset / reset / path)" }
    }
}

function Test-PortFree([int]$candidate) {
    $listener = $null
    try {
        $listener = New-Object System.Net.Sockets.TcpListener([System.Net.IPAddress]::Loopback, $candidate)
        $listener.Start()
        return $true
    } catch {
        return $false
    } finally {
        if ($listener) { $listener.Stop() }
    }
}

function Find-WebExe {
    $installRoot = (& stack path --local-install-root 2>$null | Select-Object -Last 1)
    if ($installRoot) {
        $candidate = Join-Path $installRoot 'bin\chusql-web.exe'
        if (Test-Path $candidate) { return $candidate }
    }
    $found = Get-ChildItem -Path (Join-Path $root 'chusql-web\.stack-work\dist') -Recurse -Filter 'chusql-web.exe' -ErrorAction SilentlyContinue |
        Sort-Object LastWriteTime -Descending | Select-Object -First 1
    if ($found) { return $found.FullName }
    return $null
}

switch ($Command.ToLower()) {
    'help' { Show-Help; exit 0 }
    '-h' { Show-Help; exit 0 }
    '--help' { Show-Help; exit 0 }
    'version' { Say ("chusql launcher {0}" -f $ScriptVersion); exit 0 }
    'config' { Invoke-Config $Action; exit 0 }
    'set' { Invoke-Config 'set'; exit 0 }
    'unset' { Invoke-Config 'unset'; exit 0 }
    'web' { }
    'cli' { }
    default { Fail "unknown command '$Command' (try: chusql.ps1 help)" }
}

if ($Rest -and $Rest.Count -gt 0) { Fail ("unexpected extra arguments: " + ($Rest -join ' ')) }

if ($Command.ToLower() -eq 'cli') {
    Say 'CLI interface is under development.'
    Say 'Starting the web console instead (planned: see projectplan P4).'
    Say ''
}

$eff = Get-EffectiveSettings
Assert-Effective $eff

$listenPortWanted = [int]$eff['port'].Value
$listenHost = $eff['host'].Value
$account = $eff['user'].Value
$dataDir = $eff['data-dir'].Value
if ($dataDir -like '<*') { $dataDir = Join-Path $root 'localdata' }
$pipeName = $eff['pipe-name'].Value
$stamp = [DateTime]::Now.ToString('yyyyMMdd-HHmmss')
if (-not $pipeName -or $pipeName -like '<*') { $pipeName = "chusql-joint-$stamp" }
$accountPassword = $eff['password'].Value

Say ("==================== ChuSQL launcher {0} ====================" -f $ScriptVersion)

if (-not $SkipBuild) {
    Step 'Building the Rust storage server (cargo build --release)'
    Push-Location (Join-Path $root 'chusql-storage')
    try {
        & cargo build --release
        if ($LASTEXITCODE -ne 0) { Fail 'cargo build --release failed' }
    } finally { Pop-Location }
}

$storageExe = Join-Path $root 'chusql-storage\target\release\chusql-storage.exe'
if (-not (Test-Path $storageExe)) { $storageExe = Join-Path $root 'chusql-storage\target\release\chusql-storage' }
if (-not (Test-Path $storageExe)) { Fail 'storage binary not built; run: cd chusql-storage; cargo build --release' }

if (-not $SkipBuild) {
    Step 'Building the Haskell web server (stack build)'
    Push-Location (Join-Path $root 'chusql-web')
    try {
        & stack build
        if ($LASTEXITCODE -ne 0) { Fail 'stack build failed' }
    } finally { Pop-Location }
}

Push-Location (Join-Path $root 'chusql-web')
try { $webExe = Find-WebExe } finally { Pop-Location }
if (-not $webExe) { Fail 'chusql-web.exe not found; run: cd chusql-web; stack build' }

$listenPort = $listenPortWanted
if (-not (Test-PortFree $listenPort)) {
    $picked = 0
    for ($p = $listenPort + 1; $p -le $listenPort + 2000; $p++) {
        if (Test-PortFree $p) { $picked = $p; break }
    }
    if ($picked -eq 0) { Fail "port $listenPort is unusable and no free port was found in $listenPort..$($listenPort + 2000)" }
    Warn "port $listenPort is taken or reserved by Windows; using $picked instead"
    $listenPort = $picked
}

$logDir = Join-Path $dataDir 'logs'
New-Item -ItemType Directory -Force -Path $dataDir, $logDir | Out-Null

$env:CHUSQL_PIPE = $pipeName
$env:CHUSQL_DATA_DIR = $dataDir
$env:CHUSQL_WEB_HOST = $listenHost
$env:CHUSQL_WEB_PORT = "$listenPort"
$env:CHUSQL_WEB_USER = $account
foreach ($k in $SettingKeys) {
    if ($k -in @('port', 'host', 'user', 'data-dir', 'pipe-name')) { continue }
    $envName = $Settings[$k].Env
    $v = $eff[$k].Value
    if ($v -and -not ($v -like '<*')) { Set-Item -Path ("Env:\" + $envName) -Value $v }
    else { Remove-Item -Path ("Env:\" + $envName) -ErrorAction SilentlyContinue }
}

$storageLog = Join-Path $logDir 'storage.log'
$storageErrLog = Join-Path $logDir 'storage.err.log'
$webLog = Join-Path $logDir 'web.log'
$webErrLog = Join-Path $logDir 'web.err.log'

$storageProc = $null
$webProc = $null

function Stop-Children {
    foreach ($p in @($webProc, $storageProc)) {
        if ($p -and -not $p.HasExited) { Stop-Process -Id $p.Id -Force -ErrorAction SilentlyContinue }
    }
}

$SeenLines = @{}
# 把子进程新增日志回显到本窗口
function Show-NewLogLines([string]$tag, [string]$path, [string]$color) {
    if (-not (Test-Path $path)) { return }
    $lines = @(Get-Content -Path $path -Encoding UTF8 -ErrorAction SilentlyContinue)
    $seen = 0
    if ($SeenLines.ContainsKey($path)) { $seen = $SeenLines[$path] }
    if ($lines.Count -le $seen) { return }
    for ($i = $seen; $i -lt $lines.Count; $i++) {
        Write-Host ("[{0}] " -f $tag) -NoNewline -ForegroundColor $color
        Write-Host $lines[$i]
    }
    $SeenLines[$path] = $lines.Count
}

function Show-ChildLogs {
    Show-NewLogLines 'storage' $storageLog 'DarkCyan'
    Show-NewLogLines 'storage:err' $storageErrLog 'DarkYellow'
    Show-NewLogLines 'web' $webLog 'DarkGreen'
    Show-NewLogLines 'web:err' $webErrLog 'DarkYellow'
}

try {
    Step 'Starting the Rust storage process'
    $storageProc = Start-Process -FilePath $storageExe -WorkingDirectory $dataDir -PassThru -WindowStyle Hidden `
        -RedirectStandardOutput $storageLog -RedirectStandardError $storageErrLog
    Say ("storage pid {0}  pipe {1}  ({2})" -f $storageProc.Id, $pipeName, (Split-Path -Leaf $storageExe))

    Step 'Starting the Haskell web server'
    $webArgs = @('--port', "$listenPort", '--host', $listenHost, '--user', $account)
    $webProc = Start-Process -FilePath $webExe -WorkingDirectory (Join-Path $root 'chusql-web') -PassThru -WindowStyle Hidden `
        -ArgumentList $webArgs -RedirectStandardOutput $webLog -RedirectStandardError $webErrLog
    Say ("web pid {0}" -f $webProc.Id)

    $url = "http://${listenHost}:$listenPort/"
    $statusUrl = "http://${listenHost}:$listenPort/api/status"
    $ready = $false
    $deadline = (Get-Date).AddSeconds(40)
    while ((Get-Date) -lt $deadline) {
        Show-ChildLogs
        if ($webProc.HasExited) { Show-ChildLogs; Fail "the web server exited early; see $webLog" }
        try {
            $status = Invoke-RestMethod -Uri $statusUrl -TimeoutSec 3
            if ($status.storage -eq 'up') { $ready = $true; break }
        } catch { }
        Start-Sleep -Milliseconds 400
    }
    if (-not $ready) { Show-ChildLogs; Fail "the web server did not report storage up within 40s; see $webLog" }

    $shownPassword = if ($accountPassword) { '(saved in settings / environment)' } else { 'chusql (built-in demo password)' }
    Say ''
    Say '==================== ready ===================='
    Say ("  URL       {0}" -f $url)
    Say ("  account   {0}" -f $account)
    Say ("  password  {0}" -f $shownPassword)
    Say ("  storage   {0}" -f $storageExe)
    Say ("  data dir  {0}" -f $dataDir)
    Say ("  logs      {0}" -f $logDir)
    if (Test-Path $SettingsPath) { Say ("  settings  {0}" -f $SettingsPath) }
    Say '==============================================='
    Say ''

    if (-not $NoBrowser) { Start-Process $url | Out-Null }
    Say 'Streaming child logs below. Press Ctrl+C to stop everything.'

    while (-not $webProc.HasExited) {
        Show-ChildLogs
        Start-Sleep -Milliseconds 400
    }
    Show-ChildLogs
} finally {
    Step 'Stopping'
    Stop-Children
    Say 'stopped. data and logs were kept.'
}
