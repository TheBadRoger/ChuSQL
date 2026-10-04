# 注释规范合规检查：文件级注释 ≤3 行且 ≤90 字，函数级注释 ≤30 字
# 有问题的文件不静默：问题数大于 0 时退出码为 1
param([string]$Root = (Split-Path -Parent $PSScriptRoot), [string[]]$Only)

$codeDirs = @('chusql-core','chusql-server','chusql-web','chusql-cli','chusql-bootstrap','chusql-interface','benchmark','scripts','gate-check')
$files = @()
foreach ($d in $codeDirs) {
    $p = Join-Path $Root $d
    if (Test-Path -LiteralPath $p) {
        $files += Get-ChildItem -LiteralPath $p -Recurse -File -Include *.hs,*.rs,*.js,*.css,*.html,*.ps1,*.sh |
            Where-Object { $_.FullName -notmatch '\\\.stack-work\\|\\dist-newstyle\\|\\target\\|\\node_modules\\|\\logs\\|\\tmp\\' }
    }
}
if ($Only) { $files = $files | Where-Object { $n = $_.Name; ($Only | Where-Object { $n -like $_ }).Count -gt 0 } }
$files = $files | Sort-Object FullName

$problems = @()
$scanned = 0
$fnCount = 0
$fileBlockCount = 0

foreach ($f in $files) {
    $lines = @(Get-Content -LiteralPath $f.FullName)
    $ext = $f.Extension.ToLower()
    $scanned++

    # ---- 函数级注释长度（只查每个函数/方法的“摘要行”：连续 doc 注释组第一行；# Safety 小节与非函数声明不计）----
    $docRe = '^\s*(--\s*\|\s?|///\s?|//!\s?)(.*)$'
    for ($i = 0; $i -lt $lines.Count; $i++) {
        $l = $lines[$i]
        $body = $null
        if ($l -notmatch $docRe) {
            if ($ext -eq '.js' -and $l -match '^\s*//\s?(.*)$') { $body = $Matches[1] } else { continue }
        }
        else { $body = $Matches[2] }
        if ($i -gt 0) {
            $prev = $lines[$i - 1]
            $prevDoc = ($prev -match $docRe) -or ($ext -eq '.js' -and $prev -match '^\s*//\s?')
            if ($prevDoc) { continue }
        }
        $k = $i
        while ($k -lt $lines.Count -and (($lines[$k] -match $docRe) -or ($lines[$k] -match '^\s*#\[') -or ($ext -eq '.js' -and $lines[$k] -match '^\s*//'))) { $k++ }
        $decl = if ($k -lt $lines.Count) { $lines[$k] } else { '' }
        $isFn = switch ($ext) {
            '.rs' { $decl -match '\bfn\s' }
            '.hs' { $decl -match '^\s*[a-z_][A-Za-z0-9_'']*\s*::' }
            '.js' { ($decl -match '^\s*(export\s+)?(async\s+)?function\b') -or ($decl -match '^\s*(export\s+)?(const|let|var)\s+[A-Za-z_$][\w$]*\s*=\s*(async\s*)?(function\b|\()') }
            default { $false }
        }
        if (-not $isFn) { continue }
        $t = $body.Trim()
        if ($t -eq '' -or $t[0] -eq '#') { continue }
        $fnCount++
        if ($t.Length -gt 30) {
            $problems += [pscustomobject]@{ Kind = 'FN-LONG'; File = $f.FullName; Line = $i + 1; Info = "$($t.Length)字 $t" }
        }
    }

    # ---- 找出“外部引用”最后一行（含跨行的 import{...} / param(...) 块）----
    $lastRef = -1
    for ($i = 0; $i -lt $lines.Count; $i++) {
        $l = $lines[$i]
        $hit = $false
        if ($l -match '^(import|use)\s' -or $l -match '^#\[macro_use\]' -or $l -match '^\s*#include\s' -or
            $l -match '^@import\s' -or $l -match '^\s*<link\b' -or $l -match '^\s*<script\b.*\bsrc=') { $hit = $true }
        if ($ext -ne '.hs' -and $l -match '^(param\s*\(|Set-StrictMode)') { $hit = $true }
        if ($ext -eq '.ps1' -and $l -match '^\$ErrorActionPreference\s*=') { $hit = $true }
        if ($ext -eq '.sh' -and ($l -match '^set -[eoux]' -or $l -match '^\.\s+"?[^"]*source' -or $l -match '^source\s')) { $hit = $true }
        if (-not $hit) { continue }
        $depth = ([regex]::Matches($l, '\(')).Count - ([regex]::Matches($l, '\)')).Count
        $k = $i
        while ($depth -gt 0 -and $k + 1 -lt $lines.Count) {
            $k++
            $depth += ([regex]::Matches($lines[$k], '\(')).Count - ([regex]::Matches($lines[$k], '\)')).Count
        }
        if ($ext -eq '.js' -and $l -match '^\s*import\b') {
            while ($k + 1 -lt $lines.Count -and $lines[$k] -notmatch ';\s*$') { $k++ }
        }
        $lastRef = $k
        $i = $k
    }

    # ---- Haskell：module 之前不该再有注释残留 ----
    if ($ext -eq '.hs') {
        for ($i = 0; $i -lt $lines.Count; $i++) {
            if ($lines[$i] -match '^module\s') { break }
            if ($lines[$i] -match '^\s*--' -and $lines[$i] -notmatch '^\{-#') {
                $problems += [pscustomobject]@{ Kind = 'COMMENT-BEFORE-MODULE'; File = $f.FullName; Line = $i + 1; Info = $lines[$i].Trim() }
            }
        }
    }

    # ---- 文件级注释块 ----
    $start = $lastRef + 1
    # 跳过紧跟在引用后的空行
    while ($start -lt $lines.Count -and $lines[$start].Trim() -eq '') { $start++ }
    $block = @()
    $j = $start
    while ($j -lt $lines.Count) {
        $l = $lines[$j]
        if ($l.Trim() -eq '') { if ($block.Count -gt 0) { break } else { $j++; continue } }
        if ($l -match '^\s*(--|//|#|/\*|\*|<!--|;)') { $block += $l.Trim(); $j++ } else { break }
    }
    if ($block.Count -eq 0) {
        # 退一步：文件最顶部（跳过 shebang 与 {-# ... #-}）的注释块也算文件级注释（Rust //! / 脚本头部）
        $t = 0
        while ($t -lt $lines.Count -and ($lines[$t] -match '^#!' -or $lines[$t] -match '^\{-#' -or $lines[$t].Trim() -eq '')) { $t++ }
        $top = @()
        while ($t -lt $lines.Count -and $lines[$t] -match '^\s*(--|//|#|/\*|\*|<!--|;)') { $top += $lines[$t].Trim(); $t++ }
        if ($top.Count -eq 0) {
            $problems += [pscustomobject]@{ Kind = 'NO-FILE-COMMENT'; File = $f.FullName; Line = $start + 1; Info = $(if ($start -lt $lines.Count) { $lines[$start].Trim() } else { '<EOF>' }) }
        }
        else { $fileBlockCount++ }
    }
    else {
        $fileBlockCount++
        $chars = 0
        foreach ($b in $block) {
            $t = $b -replace '^\s*(--|//|#|/\*|\*|<!--|;)\s?', ''
            $t = $t -replace '\s*\*/\s*$', ''
            $chars += $t.Trim().Length
        }
        # 只统计“文件级”块（连续注释行中，如果第一行是 -- | 说明它其实是给紧随声明的 haddock，不算文件头）
        $isHaddock = $block[0] -match '^--\s*\|\s' -or $block[0] -match '^///'
        if (-not $isHaddock -and ($block.Count -gt 3 -or $chars -gt 90)) {
            $problems += [pscustomobject]@{ Kind = 'FILE-BLOCK-LONG'; File = $f.FullName; Line = $start + 1; Info = "$($block.Count)行/$chars字" }
        }
    }
}

"扫描文件: $scanned"
"函数级注释条数: $fnCount"
"检出文件级注释块: $fileBlockCount"
"问题数: $($problems.Count)"
if ($problems.Count -gt 0) {
    $problems | Format-Table -AutoSize -Wrap | Out-String -Width 220
    exit 1
}
exit 0
