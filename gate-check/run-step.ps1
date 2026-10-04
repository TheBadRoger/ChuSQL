# 单条车道：按 -Kind 依次跑步骤，每步打印 STEP_EXIT，全零才退出 0
param(
    [string]$Root = (Split-Path -Parent $PSScriptRoot),
    [Parameter(Mandatory)][ValidateSet('build', 'cargo', 'stack', 'comments')][string]$Kind,
    [switch]$NoLint
)
$ErrorActionPreference = 'Continue'
# GHC / stack 的诊断带圆点等非 ASCII 字符，输出统一按 UTF-8，免得写管道时炸编码
$env:GHC_CHARENC = 'UTF-8'
try { [Console]::OutputEncoding = [System.Text.Encoding]::UTF8 } catch { }
$OutputEncoding = [System.Text.Encoding]::UTF8
$codes = @()

# 跑一个步骤：切目录、输出原样透传、记录退出码
function Invoke-Step([string]$Label, [string]$Dir, [string[]]$Argv) {
    Write-Host "=== $Label ==="
    $code = 1
    Push-Location -LiteralPath $Dir
    try {
        $exe = $Argv[0]
        $rest = @($Argv | Select-Object -Skip 1)
        & $exe @rest
        if ($null -ne $global:LASTEXITCODE) { $code = [int]$global:LASTEXITCODE }
    }
    catch {
        Write-Host "--- $Label raised: $_"
        $code = 1
    }
    finally { Pop-Location }
    Write-Host "--- $Label exit $code"
    "STEP_EXIT $Label $code"
    $script:codes += $code
}

$storage = Join-Path $Root 'chusql-core\storage'
$engine = Join-Path $Root 'chusql-core\engine'
switch ($Kind) {
    'build' {
        Invoke-Step 'cargo-build' $storage @('cargo', 'build', '--release')
    }
    'cargo' {
        Invoke-Step 'cargo-test' $storage @('cargo', 'test', '--release')
        if (-not $NoLint) {
            Invoke-Step 'cargo-clippy' $storage @('cargo', 'clippy', '--all-targets', '--', '-D', 'warnings')
        }
    }
    'stack' {
        Invoke-Step 'engine' $engine @('stack', 'test', 'chusql-core-engine', '--fast')
        Invoke-Step 'server' (Join-Path $Root 'chusql-server') @('stack', 'test', 'chusql-server', '--fast')
        Invoke-Step 'web' (Join-Path $Root 'chusql-web') @('stack', 'test', 'chusql-web', '--fast')
        Invoke-Step 'cli' (Join-Path $Root 'chusql-cli') @('stack', 'test', 'chusql-cli', '--fast')
    }
    'comments' {
        $check = Join-Path $PSScriptRoot 'comment-check.ps1'
        $purity = Join-Path $PSScriptRoot 'comment-purity.ps1'
        Invoke-Step 'comment-check' $Root @('pwsh', '-NoProfile', '-File', $check, '-Root', $Root)
        Invoke-Step 'comment-purity' $Root @('pwsh', '-NoProfile', '-File', $purity, '-Root', $Root)
    }
}

$failed = @($codes | Where-Object { $_ -ne 0 })
if ($failed.Count -gt 0) { exit 1 }
exit 0
