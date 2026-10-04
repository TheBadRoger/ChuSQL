# 注释纯度审计：去掉纯注释行后把工作区与 git index 逐行比对
# 相同说明改动只有注释；有差异的文件会被列出，仅作人工判断不设退出码
param([string]$Root = (Split-Path -Parent $PSScriptRoot))

Set-Location $Root
$dirs = @('chusql-core','chusql-server','chusql-web','chusql-cli','chusql-bootstrap','chusql-interface','benchmark','scripts','gate-check')
$files = @()
foreach ($d in $dirs) {
    $p = Join-Path $Root $d
    if (Test-Path -LiteralPath $p) {
        $files += Get-ChildItem -LiteralPath $p -Recurse -File -Include *.hs,*.rs,*.js,*.css,*.html,*.ps1,*.sh |
            Where-Object { $_.FullName -notmatch '\\\.stack-work\\|\\dist-newstyle\\|\\target\\|\\node_modules\\|\\logs\\|\\tmp\\' }
    }
}
$files = $files | Sort-Object FullName

function Is-CommentLine([string]$l, [string]$ext) {
    switch ($ext) {
        '.hs' { return $l -match '^\s*--' }
        '.rs' { return $l -match '^\s*//' }
        '.js' { return $l -match '^\s*(//|/\*|\*)' }
        '.css' { return $l -match '^\s*(/\*|\*|//)' }
        '.html' { return $l -match '^\s*(<!--|-->)' }
        default { return $l -match '^\s*#' }
    }
}
function Strip-Comments([string[]]$lines, [string]$ext) {
    $out = New-Object System.Collections.Generic.List[string]
    foreach ($l in $lines) { if (-not (Is-CommentLine $l $ext)) { $out.Add($l.TrimEnd()) } }
    return , $out
}

$pure = 0; $new = 0; $diffFiles = @()
foreach ($f in $files) {
    $rel = (Resolve-Path -LiteralPath $f.FullName -Relative) -replace '^\.\\', '' -replace '\\', '/'
    git cat-file -e ":$rel" 2>$null | Out-Null
    $tracked = ($LASTEXITCODE -eq 0)
    if (-not $tracked) { $new++; continue }
    $wt = Strip-Comments @(Get-Content -LiteralPath $f.FullName) $f.Extension.ToLower()
    $idx = Strip-Comments @(git show ":$rel") $f.Extension.ToLower()
    if ($wt.Count -eq $idx.Count) {
        $same = $true
        for ($i = 0; $i -lt $wt.Count; $i++) { if ($wt[$i] -cne $idx[$i]) { $same = $false; break } }
        if ($same) { $pure++; continue }
    }
    # 统计差异行
    $set = @{}
    foreach ($l in $idx) { if ($set.ContainsKey($l)) { $set[$l]++ } else { $set[$l] = 1 } }
    $extra = @()
    foreach ($l in $wt) {
        if ($set.ContainsKey($l) -and $set[$l] -gt 0) { $set[$l]-- } else { $extra += $l }
    }
    $removed = @()
    foreach ($k in $set.Keys) { if ($set[$k] -gt 0) { $removed += ("{0}x {1}" -f $set[$k], $k) } }
    $diffFiles += [pscustomobject]@{ File = $rel; Added = $extra.Count; Removed = $removed.Count; Sample = (($extra | Select-Object -First 3) -join ' | ') }
}

"扫描文件: $($files.Count)"
"仅注释变更（去注释后与 index 完全相同）: $pure"
"index 无基线（新文件，跳过）: $new"
"含非注释差异的文件: $($diffFiles.Count)"
if ($diffFiles.Count -gt 0) { $diffFiles | Format-Table -AutoSize -Wrap | Out-String -Width 240 }
