# 门禁入口：并行跑 Rust / Haskell / 注释三条车道，日志实时输出
# 用法见 README.md；-Clean 重建本地依赖，-Only 只跑一条车道
param(
    [string]$Root = (Split-Path -Parent $PSScriptRoot),
    [switch]$Clean,
    [switch]$Full,
    [switch]$NoLint,
    [string[]]$Only
)
$ErrorActionPreference = 'Stop'
# 车道里的 GHC / stack 诊断带非 ASCII 字符，父子进程统一按 UTF-8 收
$env:GHC_CHARENC = 'UTF-8'
try { [Console]::OutputEncoding = [System.Text.Encoding]::UTF8 } catch { }
$OutputEncoding = [System.Text.Encoding]::UTF8

$LogDir = Join-Path $PSScriptRoot 'logs'
$TmpDir = Join-Path $PSScriptRoot 'tmp'
New-Item -ItemType Directory -Force -Path $LogDir, $TmpDir | Out-Null
$env:PATH = (Join-Path $Root 'chusql-core\storage\target\release') + ';' + $env:PATH
$env:TEMP = $TmpDir
$env:TMP = $TmpDir
Set-Location -LiteralPath $Root

$colors = @{ build = 'DarkGray'; cargo = 'Yellow'; stack = 'Cyan'; comments = 'Green' }
$laneNames = @('cargo', 'stack', 'comments')
if ($Only) {
    $laneNames = @($laneNames | Where-Object { $Only -contains $_ })
    if ($laneNames.Count -eq 0) { throw "unknown lane: $($Only -join ',')" }
}

# 读日志里新出现的行并带车道前缀打印，只保留行数游标
function Show-New([object]$Lane) {
    foreach ($part in @('Log', 'ErrLog')) {
        $path = $Lane.$part
        $seen = [int]$Lane."Seen$part"
        $lines = @()
        try { $lines = @(Get-Content -LiteralPath $path -Encoding UTF8 -ErrorAction Stop) } catch { $lines = @() }
        if ($lines.Count -gt $seen) {
            foreach ($line in $lines[$seen..($lines.Count - 1)]) {
                Write-Host ("{0} {1}" -f $Lane.Tag, $line) -ForegroundColor $colors[$Lane.Name]
            }
            $Lane."Seen$part" = $lines.Count
        }
    }
}

# 起一条车道：独立进程写自己的日志，主进程只做流式转发
function Start-Lane([string]$Name) {
    $log = Join-Path $LogDir "$Name.log"
    $errLog = Join-Path $LogDir "$Name.err.log"
    Remove-Item -LiteralPath $log, $errLog -Force -ErrorAction SilentlyContinue
    $stepArgs = @('-NoProfile', '-File', (Join-Path $PSScriptRoot 'run-step.ps1'), '-Root', $Root, '-Kind', $Name)
    if ($NoLint) { $stepArgs += '-NoLint' }
    $proc = Start-Process -FilePath 'pwsh' -ArgumentList $stepArgs -RedirectStandardOutput $log `
        -RedirectStandardError $errLog -NoNewWindow -PassThru
    return [pscustomobject]@{
        Name = $Name; Proc = $proc; Log = $log; ErrLog = $errLog
        Tag = "[$Name]"; SeenLog = 0; SeenErrLog = 0; Exit = $null
    }
}

# 等一批车道跑完，边等边转发输出
function Wait-Lanes($Lanes) {
    while ($true) {
        $running = 0
        foreach ($lane in $Lanes) {
            $lane.Proc.Refresh()
            Show-New $lane
            if (-not $lane.Proc.HasExited) { $running++ }
        }
        if ($running -eq 0) { break }
        Start-Sleep -Milliseconds 300
    }
    Start-Sleep -Milliseconds 400
    foreach ($lane in $Lanes) { Show-New $lane; $lane.Exit = $lane.Proc.ExitCode }
}

# 清理本地依赖工件：-Clean 只清各项目自己的，-Full 连依赖副本一起清
function Invoke-Clean {
    foreach ($dir in @('chusql-core\engine', 'chusql-server', 'chusql-web', 'chusql-cli')) {
        $argv = @('clean')
        if ($Full) { $argv += '--full' }
        Write-Host "== stack $($argv -join ' ') in $dir" -ForegroundColor DarkGray
        Push-Location -LiteralPath (Join-Path $Root $dir)
        try { & stack @argv 2>&1 | ForEach-Object { Write-Host "   $_" -ForegroundColor DarkGray } }
        finally { Pop-Location }
    }
}

$started = Get-Date
if ($Clean -or $Full) { Invoke-Clean }

# 先单独建好 Rust 动态库：Haskell 测试要从 target\release 加载它
$build = Start-Lane 'build'
Write-Host '== 先建 Rust 动态库' -ForegroundColor DarkGray
Wait-Lanes @($build)

Write-Host "== 车道并行：$($laneNames -join ' / ')" -ForegroundColor DarkGray
$lanes = @()
foreach ($name in $laneNames) { $lanes += Start-Lane $name }
Wait-Lanes $lanes

'--- 门禁结果 ---'
$allExit = @($build.Exit)
$failed = 0
foreach ($lane in $lanes) {
    $codes = @()
    $steps = @(Select-String -LiteralPath $lane.Log -Pattern '^STEP_EXIT (\S+) (-?\d+)$' -ErrorAction SilentlyContinue)
    foreach ($m in $steps) {
        $codes += "$($m.Matches[0].Groups[1].Value)=$($m.Matches[0].Groups[2].Value)"
    }
    $allExit += $lane.Exit
    if ($lane.Exit -ne 0) { $failed++ }
    '车道 {0,-9} exit={1,-3} {2}' -f $lane.Name, $lane.Exit, ($codes -join ' ')
    $tail = @(Get-Content -LiteralPath $lane.Log -Encoding UTF8 -ErrorAction SilentlyContinue | Select-Object -Last 5)
    foreach ($line in $tail) { "    $line" }
}
$elapsed = [int]((Get-Date) - $started).TotalSeconds
"用时: ${elapsed}s"
"STEPS=$($allExit -join ',')"
if ($failed -gt 0 -or $build.Exit -ne 0) { 'GATE_EXIT=1'; exit 1 }
'GATE_EXIT=0'
exit 0
