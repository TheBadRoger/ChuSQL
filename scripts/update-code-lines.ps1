param([string]$Root = (Split-Path -Parent $PSScriptRoot))

$ErrorActionPreference = 'Stop'

# 统计物理行数并生成 README 统一风格的本地 SVG 徽章。

# 生成带文字和色块的本地徽章。
function Write-Badge([string]$Directory, [string]$Name, [string]$Label, [string]$Value, [string]$Color, [string]$Description) {
    $leftWidth = [int]($Label.Length * 7 + 24)
    $rightWidth = [int]($Value.Length * 7 + 24)
    $width = $leftWidth + $rightWidth
    $leftCenter = $leftWidth / 2
    $rightCenter = $leftWidth + $rightWidth / 2
    $safeLabel = [Security.SecurityElement]::Escape($Label)
    $safeValue = [Security.SecurityElement]::Escape($Value)
    $safeDescription = [Security.SecurityElement]::Escape($Description)
    $svg = @"
<svg xmlns="http://www.w3.org/2000/svg" width="$width" height="24" viewBox="0 0 $width 24" role="img" aria-label="$safeLabel`: $safeValue">
  <title>$safeDescription</title>
  <defs><clipPath id="rounded"><rect width="$width" height="24" rx="5"/></clipPath></defs>
  <g clip-path="url(#rounded)"><rect width="$width" height="24" fill="#1e293b"/><rect x="$leftWidth" width="$rightWidth" height="24" fill="$Color"/></g>
  <g fill="#fff" text-anchor="middle" font-family="Verdana,DejaVu Sans,sans-serif" font-size="11"><text x="$leftCenter" y="16">$safeLabel</text><text x="$rightCenter" y="16">$safeValue</text></g>
</svg>
"@
    Set-Content -LiteralPath (Join-Path $Directory "$Name.svg") -Value $svg -Encoding utf8
}

Push-Location -LiteralPath $Root
try {
    $paths = @(& git ls-files --cached --others --exclude-standard)
    if ($LASTEXITCODE -ne 0) { throw 'git ls-files failed' }
    $lines = 0
    $files = 0
    foreach ($path in ($paths | Sort-Object -Unique)) {
        if ($path -notmatch '\.(hs|rs|js|css|html|ps1|sh)$') { continue }
        if (-not (Test-Path -LiteralPath $path -PathType Leaf)) { continue }
        $lines += @(Get-Content -LiteralPath $path).Count
        $files++
    }
    $label = $lines.ToString('N0', [Globalization.CultureInfo]::InvariantCulture)
    $badgeDir = Join-Path $Root 'docs/badges'
    [IO.Directory]::CreateDirectory($badgeDir) | Out-Null
    $revision = & git rev-parse --short HEAD
    if ($LASTEXITCODE -ne 0) { throw 'git rev-parse failed' }
    Write-Badge $badgeDir 'lines' 'source lines' $label '#0369a1' 'Physical source lines; includes tests, comments and blank lines.'
    Write-Badge $badgeDir 'release' 'release' 'browse' '#6d28d9' 'Open GitHub releases to view published versions.'
    Write-Badge $badgeDir 'commit' 'commit' $revision '#475569' 'Local HEAD snapshot when the badges were generated; open commit history for latest updates.'
    Write-Badge $badgeDir 'languages' 'languages' 'Haskell + Rust' '#6d28d9' 'ChuSQL implementation languages: Haskell and Rust.'
    Write-Badge $badgeDir 'build' 'build' 'view status' '#0369a1' 'Open the build workflow for live status; this local badge is a navigation link.'
    Write-Badge $badgeDir 'tests' 'tests' 'view status' '#0369a1' 'Open the test workflow for live status; this local badge is a navigation link.'
    Write-Badge $badgeDir 'gate' 'gate' 'manual status' '#b45309' 'Open the owner-triggered gate workflow for status; this local badge is a navigation link.'
    Write-Output "source_lines=$lines source_files=$files"
} finally {
    Pop-Location
}
