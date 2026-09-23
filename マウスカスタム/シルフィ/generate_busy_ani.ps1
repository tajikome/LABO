Add-Type -AssemblyName System.Drawing

$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
if (-not $scriptDir) { $scriptDir = (Get-Location).Path }

$sheetPath = Join-Path $scriptDir "hotspot_reference_sheet.png"
$aniOutPath = Join-Path $scriptDir "03_Busy.ani"
$backupPath = Join-Path $scriptDir "03_Busy_old.ani"

if (-not (Test-Path $sheetPath)) {
    Write-Error "hotspot_reference_sheet.png not found: $sheetPath"
    exit 1
}

Write-Output "Loading sheet image..."
$sheet = [System.Drawing.Bitmap]::FromFile($sheetPath)

# Cell 03: col 2 (0, 1, 2 out of 5), row 0
$colWidth = [int]($sheet.Width / 5) # 307
$colLeft = 2 * $colWidth # 614
$cellHeight = 246

$cell = New-Object System.Drawing.Bitmap($colWidth, $cellHeight)
$g = [System.Drawing.Graphics]::FromImage($cell)
$g.InterpolationMode = [System.Drawing.Drawing2D.InterpolationMode]::HighQualityBicubic
$g.PixelOffsetMode = [System.Drawing.Drawing2D.PixelOffsetMode]::HighQuality
$g.DrawImage($sheet, (New-Object System.Drawing.Rectangle(0, 0, $colWidth, $cellHeight)), (New-Object System.Drawing.Rectangle($colLeft, 0, $colWidth, $cellHeight)), [System.Drawing.GraphicsUnit]::Pixel)
$g.Dispose()
$sheet.Dispose()

Write-Output "Extracted cell image ($($cell.Width) x $($cell.Height))"

# Clear top header (red circle '03' and text '待機中' at y < 68)
for ($y = 0; $y -lt 68; $y++) {
    for ($x = 0; $x -lt $colWidth; $x++) {
        $cell.SetPixel($x, $y, [System.Drawing.Color]::FromArgb(0, 0, 0, 0))
    }
}

# Flood fill from boundaries to remove checkerboard background (y >= 68)
$visited = New-Object "bool[,]" $colWidth, $cellHeight
$queue = New-Object System.Collections.Generic.Queue[System.Drawing.Point]

for ($x = 0; $x -lt $colWidth; $x++) {
    $queue.Enqueue((New-Object System.Drawing.Point($x, 68)))
    $queue.Enqueue((New-Object System.Drawing.Point($x, ($cellHeight - 1))))
}
for ($y = 68; $y -lt $cellHeight; $y++) {
    $queue.Enqueue((New-Object System.Drawing.Point(0, $y)))
    $queue.Enqueue((New-Object System.Drawing.Point(($colWidth - 1), $y)))
}

function IsBackgroundPixel($c) {
    $maxDiff = [Math]::Max([Math]::Abs($c.R - $c.G), [Math]::Max([Math]::Abs($c.G - $c.B), [Math]::Abs($c.R - $c.B)))
    if ($maxDiff -le 15 -and $c.R -ge 215 -and $c.G -ge 215 -and $c.B -ge 215) {
        return $true
    }
    return $false
}

while ($queue.Count -gt 0) {
    $p = $queue.Dequeue()
    $px = $p.X
    $py = $p.Y
    if ($px -lt 0 -or $px -ge $colWidth -or $py -lt 68 -or $py -ge $cellHeight) { continue }
    if ($visited[$px, $py]) { continue }
    $visited[$px, $py] = $true
    
    $c = $cell.GetPixel($px, $py)
    if (IsBackgroundPixel $c) {
        $cell.SetPixel($px, $py, [System.Drawing.Color]::FromArgb(0, 0, 0, 0))
        if ($px -gt 0) { $queue.Enqueue((New-Object System.Drawing.Point(($px - 1), $py))) }
        if ($px -lt $colWidth - 1) { $queue.Enqueue((New-Object System.Drawing.Point(($px + 1), $py))) }
        if ($py -gt 68) { $queue.Enqueue((New-Object System.Drawing.Point($px, ($py - 1)))) }
        if ($py -lt $cellHeight - 1) { $queue.Enqueue((New-Object System.Drawing.Point($px, ($py + 1)))) }
    }
}

Write-Output "Background transparency applied."

# Find bounding box
$minX = $colWidth; $maxX = 0; $minY = $cellHeight; $maxY = 0
for ($y = 68; $y -lt $cellHeight; $y++) {
    for ($x = 0; $x -lt $colWidth; $x++) {
        $c = $cell.GetPixel($x, $y)
        if ($c.A -gt 20) {
            if ($x -lt $minX) { $minX = $x }
            if ($x -gt $maxX) { $maxX = $x }
            if ($y -lt $minY) { $minY = $y }
            if ($y -gt $maxY) { $maxY = $y }
        }
    }
}

Write-Output "Bounding box: ($minX, $minY) to ($maxX, $maxY)"

# Separate Body and Stars
$bodyBmp = New-Object System.Drawing.Bitmap($colWidth, $cellHeight)
$starsBmp = New-Object System.Drawing.Bitmap($colWidth, $cellHeight)

for ($y = $minY; $y -le $maxY; $y++) {
    for ($x = $minX; $x -le $maxX; $x++) {
        $c = $cell.GetPixel($x, $y)
        if ($c.A -gt 20) {
            $isRedStar = ($c.R -gt 160 -and ($c.R - $c.G) -gt 35 -and ($c.R - $c.B) -gt 35)
            if ($isRedStar -and ($x -lt 110 -or $x -gt 200 -or $y -lt 115)) {
                $starsBmp.SetPixel($x, $y, $c)
            } else {
                $bodyBmp.SetPixel($x, $y, $c)
            }
        }
    }
}

# 4 Star regions (LeftTop, LeftBottom, RightTop, RightBottom)
$starRegions = @(
    @{ Name="LeftTop";     MinX=0;   MaxX=120; MinY=68;  MaxY=145 },
    @{ Name="LeftBottom";  MinX=0;   MaxX=120; MinY=145; MaxY=245 },
    @{ Name="RightTop";    MinX=180; MaxX=307; MinY=68;  MaxY=155 },
    @{ Name="RightBottom"; MinX=180; MaxX=307; MinY=155; MaxY=245 }
)

$starBmps = @()
foreach ($sr in $starRegions) {
    $sBmp = New-Object System.Drawing.Bitmap($colWidth, $cellHeight)
    $hasPx = $false
    for ($y = $sr.MinY; $y -lt $sr.MaxY; $y++) {
        for ($x = $sr.MinX; $x -lt $sr.MaxX; $x++) {
            $c = $starsBmp.GetPixel($x, $y)
            if ($c.A -gt 10) {
                $sBmp.SetPixel($x, $y, $c)
                $hasPx = $true
            }
        }
    }
    $starBmps += @{ Bitmap=$sBmp; HasPixels=$hasPx; Region=$sr }
}

# Supplement feet under skirt (fixes cut-off at bottom)
$bgBody = [System.Drawing.Graphics]::FromImage($bodyBmp)
$feetBrush = New-Object System.Drawing.SolidBrush([System.Drawing.Color]::FromArgb(230, 45, 45, 55))
$feetHighlight = New-Object System.Drawing.SolidBrush([System.Drawing.Color]::FromArgb(200, 220, 220, 230))
$bgBody.FillEllipse($feetBrush, 142, 233, 11, 7)
$bgBody.FillEllipse($feetHighlight, 144, 233, 7, 4)
$bgBody.FillEllipse($feetBrush, 160, 233, 11, 7)
$bgBody.FillEllipse($feetHighlight, 162, 233, 7, 4)
$feetBrush.Dispose()
$feetHighlight.Dispose()
$bgBody.Dispose()

# Recompute full bounding box
$contentMinX = 307; $contentMaxX = 0; $contentMinY = 250; $contentMaxY = 0
for ($y = 68; $y -lt $cellHeight; $y++) {
    for ($x = 0; $x -lt $colWidth; $x++) {
        $cA = $bodyBmp.GetPixel($x, $y).A
        $sA = $starsBmp.GetPixel($x, $y).A
        if ($cA -gt 15 -or $sA -gt 15) {
            if ($x -lt $contentMinX) { $contentMinX = $x }
            if ($x -gt $contentMaxX) { $contentMaxX = $x }
            if ($y -lt $contentMinY) { $contentMinY = $y }
            if ($y -gt $contentMaxY) { $contentMaxY = $y }
        }
    }
}

$srcW = $contentMaxX - $contentMinX + 1
$srcH = $contentMaxY - $contentMinY + 1
Write-Output "Total content size: ${srcW} x ${srcH}"

# Fit into 64x64 with 4px margin on all sides (dest max: 56x56)
$targetMax = 56.0
$scale = [Math]::Min($targetMax / $srcW, $targetMax / $srcH)
$destW = [int]($srcW * $scale)
$destH = [int]($srcH * $scale)

$offsetX = [int]((64 - $destW) / 2)
$offsetY = [int]((64 - $destH) / 2)

Write-Output "64x64 layout: scale=$scale, destSize=${destW}x${destH}, offset=($offsetX, $offsetY)"

$frameCount = 10
$frames = @()
$pngByteArrays = @()

for ($frameIdx = 0; $frameIdx -lt $frameCount; $frameIdx++) {
    $frameBmp = New-Object System.Drawing.Bitmap(64, 64)
    $fg = [System.Drawing.Graphics]::FromImage($frameBmp)
    $fg.InterpolationMode = [System.Drawing.Drawing2D.InterpolationMode]::HighQualityBicubic
    $fg.PixelOffsetMode = [System.Drawing.Drawing2D.PixelOffsetMode]::HighQuality
    $fg.SmoothingMode = [System.Drawing.Drawing2D.SmoothingMode]::HighQuality

    # Draw body (fixed)
    $destRect = New-Object System.Drawing.Rectangle($offsetX, $offsetY, $destW, $destH)
    $srcRect = New-Object System.Drawing.Rectangle($contentMinX, $contentMinY, $srcW, $srcH)
    $fg.DrawImage($bodyBmp, $destRect, $srcRect, [System.Drawing.GraphicsUnit]::Pixel)

    # 4 Stars animation (clockwise: LeftTop(0), RightTop(2.5), RightBottom(5), LeftBottom(7.5))
    $phases = @(0.0, 7.5, 2.5, 5.0)

    for ($sIdx = 0; $sIdx -lt 4; $sIdx++) {
        $st = $starBmps[$sIdx]
        if (-not $st.HasPixels) { continue }

        $phase = $phases[$sIdx]
        $t = ($frameIdx + $phase) % $frameCount
        $rad = ($t / $frameCount) * [Math]::PI * 2.0
        $pulse = ([Math]::Sin($rad) + 1.0) / 2.0
        $starAlpha = 0.30 + (0.70 * $pulse)

        $cm = New-Object System.Drawing.Imaging.ColorMatrix
        $cm.Matrix33 = [float]$starAlpha
        if ($pulse -gt 0.7) {
            $glow = [float](($pulse - 0.7) / 0.3 * 0.35)
            $cm.Matrix00 = 1.0 + $glow
            $cm.Matrix11 = 1.0 + $glow
            $cm.Matrix22 = 1.0 + $glow
        }
        $ia = New-Object System.Drawing.Imaging.ImageAttributes
        $ia.SetColorMatrix($cm)

        $fg.DrawImage($st.Bitmap, $destRect, $contentMinX, $contentMinY, $srcW, $srcH, [System.Drawing.GraphicsUnit]::Pixel, $ia)
        $ia.Dispose()
    }

    $fg.Dispose()
    $frames += $frameBmp

    $ms = New-Object System.IO.MemoryStream
    $frameBmp.Save($ms, [System.Drawing.Imaging.ImageFormat]::Png)
    $pngByteArrays += ,($ms.ToArray())
    $ms.Dispose()
}

Write-Output "Generated $frameCount frames."

# Cut-off edge verification
$allOk = $true
for ($i = 0; $i -lt $frameCount; $i++) {
    $fb = $frames[$i]
    $edgeTouched = $false
    for ($y = 0; $y -lt 64; $y++) {
        if ($fb.GetPixel(0, $y).A -gt 10 -or $fb.GetPixel(63, $y).A -gt 10) { $edgeTouched = $true }
    }
    for ($x = 0; $x -lt 64; $x++) {
        if ($fb.GetPixel($x, 0).A -gt 10 -or $fb.GetPixel($x, 63).A -gt 10) { $edgeTouched = $true }
    }
    if ($edgeTouched) {
        Write-Warning "Frame $i touches canvas boundary!"
        $allOk = $false
    }
}
if ($allOk) {
    Write-Output "VERIFICATION SUCCESS: Edge cutoff is 0 (all elements perfectly inside 64x64)!"
}

# Backup old 03_Busy.ani if needed
if ((Test-Path $backupPath) -eq $false -and (Test-Path $aniOutPath)) {
    Copy-Item $aniOutPath $backupPath -Force
    Write-Output "Backup created: 03_Busy_old.ani"
}

# Write RIFF ACON
$hotspotX = 32
$hotspotY = 14
$jifRate = 8

$aniStream = New-Object System.IO.MemoryStream
$writer = New-Object System.IO.BinaryWriter($aniStream)

# RIFF
$writer.Write([System.Text.Encoding]::ASCII.GetBytes("RIFF"))
$writer.Write([uint32]0)
$writer.Write([System.Text.Encoding]::ASCII.GetBytes("ACON"))

# anih
$writer.Write([System.Text.Encoding]::ASCII.GetBytes("anih"))
$writer.Write([uint32]36)
$writer.Write([uint32]36)
$writer.Write([uint32]$frameCount)
$writer.Write([uint32]$frameCount)
$writer.Write([uint32]0)
$writer.Write([uint32]0)
$writer.Write([uint32]0)
$writer.Write([uint32]0)
$writer.Write([uint32]$jifRate)
$writer.Write([uint32]1)

# LIST fram
$framListStart = $aniStream.Position
$writer.Write([System.Text.Encoding]::ASCII.GetBytes("LIST"))
$writer.Write([uint32]0)
$writer.Write([System.Text.Encoding]::ASCII.GetBytes("fram"))

for ($i = 0; $i -lt $frameCount; $i++) {
    $pngBytes = $pngByteArrays[$i]
    $curResSize = 6 + 16 + $pngBytes.Length
    
    $writer.Write([System.Text.Encoding]::ASCII.GetBytes("icon"))
    $writer.Write([uint32]$curResSize)
    
    $writer.Write([uint16]0)
    $writer.Write([uint16]2)
    $writer.Write([uint16]1)
    
    $writer.Write([byte]64)
    $writer.Write([byte]64)
    $writer.Write([byte]0)
    $writer.Write([byte]0)
    $writer.Write([uint16]$hotspotX)
    $writer.Write([uint16]$hotspotY)
    $writer.Write([uint32]$pngBytes.Length)
    $writer.Write([uint32]22)
    
    $writer.Write($pngBytes)
}

$totalAniSize = $aniStream.Length
$aniStream.Position = 4
$writer.Write([uint32]($totalAniSize - 8))
$aniStream.Position = $framListStart + 4
$writer.Write([uint32]($totalAniSize - $framListStart - 8))

[System.IO.File]::WriteAllBytes($aniOutPath, $aniStream.ToArray())
$writer.Dispose()
$aniStream.Dispose()

$newFile = Get-Item $aniOutPath
Write-Output "Successfully wrote 03_Busy.ani ($($newFile.Length) bytes)"

# Save preview frames
$previewDir = Join-Path $scriptDir "preview_frames"
if (-not (Test-Path $previewDir)) { New-Item -ItemType Directory -Path $previewDir -Force | Out-Null }
for ($i = 0; $i -lt $frameCount; $i++) {
    $pPath = Join-Path $previewDir ("frame_{0:D2}.png" -f $i)
    $frames[$i].Save($pPath, [System.Drawing.Imaging.ImageFormat]::Png)
}
Write-Output "Saved preview frames in: preview_frames"

$cell.Dispose()
$bodyBmp.Dispose()
$starsBmp.Dispose()
foreach ($st in $starBmps) { $st.Bitmap.Dispose() }
foreach ($f in $frames) { $f.Dispose() }

Write-Output "DONE!"
