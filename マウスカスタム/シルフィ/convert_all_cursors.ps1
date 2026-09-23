param(
    [string]$InputPath = "",
    [int]$CanvasSize = 64,
    [int]$HotspotX = -1,
    [int]$HotspotY = -1
)

Add-Type -AssemblyName System.Drawing

$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
if (-not $scriptDir) { $scriptDir = (Get-Location).Path }

Write-Host "==========================================================" -ForegroundColor Cyan
Write-Host "       Custom Cursor Batch Converter (64px / 32px)        " -ForegroundColor Cyan
Write-Host "==========================================================" -ForegroundColor Cyan
Write-Host ""

function Build-CurBinary($pngBytes, [int]$hx, [int]$hy, [int]$width = 64, [int]$height = 64) {
    $ms = New-Object System.IO.MemoryStream
    $w = New-Object System.IO.BinaryWriter($ms)
    
    $w.Write([uint16]0)
    $w.Write([uint16]2)
    $w.Write([uint16]1)
    
    $w.Write([byte]($width -band 0xFF))
    $w.Write([byte]($height -band 0xFF))
    $w.Write([byte]0)
    $w.Write([byte]0)
    $w.Write([uint16]$hx)
    $w.Write([uint16]$hy)
    $w.Write([uint32]$pngBytes.Length)
    $w.Write([uint32]22)
    
    $w.Write($pngBytes)
    
    $bytes = $ms.ToArray()
    $w.Dispose()
    $ms.Dispose()
    return $bytes
}

function Build-AniBinary($pngByteArrays, [int]$hx, [int]$hy, [int]$jifRate = 8) {
    $frameCount = $pngByteArrays.Count
    $ms = New-Object System.IO.MemoryStream
    $w = New-Object System.IO.BinaryWriter($ms)
    
    $w.Write([System.Text.Encoding]::ASCII.GetBytes("RIFF"))
    $w.Write([uint32]0)
    $w.Write([System.Text.Encoding]::ASCII.GetBytes("ACON"))
    
    $w.Write([System.Text.Encoding]::ASCII.GetBytes("anih"))
    $w.Write([uint32]36)
    $w.Write([uint32]36)
    $w.Write([uint32]$frameCount)
    $w.Write([uint32]$frameCount)
    $w.Write([uint32]0)
    $w.Write([uint32]0)
    $w.Write([uint32]0)
    $w.Write([uint32]0)
    $w.Write([uint32]$jifRate)
    $w.Write([uint32]1)
    
    $framPos = $ms.Position
    $w.Write([System.Text.Encoding]::ASCII.GetBytes("LIST"))
    $w.Write([uint32]0)
    $w.Write([System.Text.Encoding]::ASCII.GetBytes("fram"))
    
    for ($i = 0; $i -lt $frameCount; $i++) {
        $curBytes = Build-CurBinary $pngByteArrays[$i] $hx $hy 64 64
        $w.Write([System.Text.Encoding]::ASCII.GetBytes("icon"))
        $w.Write([uint32]$curBytes.Length)
        $w.Write($curBytes)
    }
    
    $totalSize = $ms.Length
    $ms.Position = 4
    $w.Write([uint32]($totalSize - 8))
    $ms.Position = $framPos + 4
    $w.Write([uint32]($totalSize - $framPos - 8))
    
    $bytes = $ms.ToArray()
    $w.Dispose()
    $ms.Dispose()
    return $bytes
}

function Fit-BitmapToCanvas($srcBmp, [int]$canvasSize = 64, [int]$margin = 4) {
    $minX = $srcBmp.Width; $maxX = 0; $minY = $srcBmp.Height; $maxY = 0
    for ($y = 0; $y -lt $srcBmp.Height; $y++) {
        for ($x = 0; $x -lt $srcBmp.Width; $x++) {
            if ($srcBmp.GetPixel($x, $y).A -gt 15) {
                if ($x -lt $minX) { $minX = $x }
                if ($x -gt $maxX) { $maxX = $x }
                if ($y -lt $minY) { $minY = $y }
                if ($y -gt $maxY) { $maxY = $y }
            }
        }
    }
    if ($minX -gt $maxX) {
        return New-Object System.Drawing.Bitmap($canvasSize, $canvasSize)
    }
    
    $bw = $maxX - $minX + 1
    $bh = $maxY - $minY + 1
    $targetMax = [double]($canvasSize - ($margin * 2))
    $scale = [Math]::Min($targetMax / $bw, $targetMax / $bh)
    $dw = [int]($bw * $scale)
    $dh = [int]($bh * $scale)
    $ox = [int](($canvasSize - $dw) / 2)
    $oy = [int](($canvasSize - $dh) / 2)
    
    $outBmp = New-Object System.Drawing.Bitmap($canvasSize, $canvasSize)
    $g = [System.Drawing.Graphics]::FromImage($outBmp)
    $g.InterpolationMode = [System.Drawing.Drawing2D.InterpolationMode]::HighQualityBicubic
    $g.PixelOffsetMode = [System.Drawing.Drawing2D.PixelOffsetMode]::HighQuality
    $g.SmoothingMode = [System.Drawing.Drawing2D.SmoothingMode]::HighQuality
    
    $srcRect = New-Object System.Drawing.Rectangle($minX, $minY, $bw, $bh)
    $destRect = New-Object System.Drawing.Rectangle($ox, $oy, $dw, $dh)
    $g.DrawImage($srcBmp, $destRect, $srcRect, [System.Drawing.GraphicsUnit]::Pixel)
    $g.Dispose()
    
    return $outBmp
}

# Drag and Drop mode
if ($InputPath -and (Test-Path $InputPath) -and ((Get-Item $InputPath).Attributes -band [System.IO.FileAttributes]::Directory) -eq 0) {
    $srcFile = Get-Item $InputPath
    Write-Host "Converting single file: $($srcFile.Name)" -ForegroundColor Yellow
    
    $srcBmp = [System.Drawing.Bitmap]::FromFile($srcFile.FullName)
    $destBmp = Fit-BitmapToCanvas $srcBmp $CanvasSize 4
    
    $ms = New-Object System.IO.MemoryStream
    $destBmp.Save($ms, [System.Drawing.Imaging.ImageFormat]::Png)
    $pngBytes = $ms.ToArray()
    $ms.Dispose()
    
    $hx = if ($HotspotX -ge 0) { $HotspotX } else { [int]($CanvasSize / 2) }
    $hy = if ($HotspotY -ge 0) { $HotspotY } else { [int]($CanvasSize / 2) }
    
    $curBytes = Build-CurBinary $pngBytes $hx $hy $CanvasSize $CanvasSize
    $outCurPath = [System.IO.Path]::ChangeExtension($srcFile.FullName, ".cur")
    [System.IO.File]::WriteAllBytes($outCurPath, $curBytes)
    
    Write-Host "Done: $outCurPath (Hotspot: $hx, $hy)" -ForegroundColor Green
    $destBmp.Dispose()
    $srcBmp.Dispose()
    exit 0
}

# Batch mode from hotspot_reference_sheet.png
$defaultSheet = Join-Path $scriptDir "hotspot_reference_sheet.png"
if (-not (Test-Path $defaultSheet)) {
    Write-Host "hotspot_reference_sheet.png not found: $defaultSheet" -ForegroundColor Red
    exit 1
}

Write-Host "Batch converting all 17 cursors from reference sheet..." -ForegroundColor Cyan
$sheet = [System.Drawing.Bitmap]::FromFile($defaultSheet)

$cursors = @(
    @{ Id="01"; Name="01_Normal_Select.cur";    Row=0; Col=0; Hx=20; Hy=20; Type="CUR"; Desc="Normal Select" },
    @{ Id="02"; Name="02_Help_Select.cur";      Row=0; Col=1; Hx=24; Hy=16; Type="CUR"; Desc="Help Select" },
    @{ Id="03"; Name="03_Busy.ani";             Row=0; Col=2; Hx=32; Hy=14; Type="ANI"; Desc="Busy" },
    @{ Id="04"; Name="04_Working.ani";          Row=0; Col=3; Hx=32; Hy=14; Type="ANI"; Desc="Working" },
    @{ Id="05"; Name="05_Area_Select.cur";      Row=0; Col=4; Hx=48; Hy=20; Type="CUR"; Desc="Area Select" },
    @{ Id="06"; Name="06_Text_Select.cur";      Row=1; Col=0; Hx=16; Hy=40; Type="CUR"; Desc="Text Select" },
    @{ Id="07"; Name="07_Handwriting.cur";      Row=1; Col=1; Hx=34; Hy=36; Type="CUR"; Desc="Handwriting" },
    @{ Id="08"; Name="08_Link_Select.cur";      Row=1; Col=2; Hx=24; Hy=20; Type="CUR"; Desc="Link Select" },
    @{ Id="09"; Name="09_Move.cur";             Row=1; Col=3; Hx=20; Hy=32; Type="CUR"; Desc="Move" },
    @{ Id="10"; Name="10_Unavailable.cur";      Row=1; Col=4; Hx=20; Hy=32; Type="CUR"; Desc="Unavailable" },
    @{ Id="11"; Name="11_Vertical_Resize.cur";  Row=2; Col=0; Hx=16; Hy=32; Type="CUR"; Desc="Vertical Resize" },
    @{ Id="12"; Name="12_Horizontal_Resize.cur";Row=2; Col=1; Hx=24; Hy=32; Type="CUR"; Desc="Horizontal Resize" },
    @{ Id="13"; Name="13_Diagonal_Resize_1.cur";Row=2; Col=2; Hx=24; Hy=24; Type="CUR"; Desc="Diagonal Resize 1" },
    @{ Id="14"; Name="14_Diagonal_Resize_2.cur";Row=2; Col=3; Hx=24; Hy=24; Type="CUR"; Desc="Diagonal Resize 2" },
    @{ Id="15"; Name="15_Precision_Select.cur"; Row=2; Col=4; Hx=20; Hy=32; Type="CUR"; Desc="Precision Select" },
    @{ Id="16"; Name="16_Location_Select.cur";  Row=3; Col=0; Hx=24; Hy=34; Type="CUR"; Desc="Location Select" },
    @{ Id="17"; Name="17_Person_Select.cur";    Row=3; Col=1; Hx=24; Hy=34; Type="CUR"; Desc="Person Select" }
)

$colW = [int]($sheet.Width / 5)
$rowH = 246

foreach ($c in $cursors) {
    $outPath = Join-Path $scriptDir $c.Name
    
    if ($c.Id -eq "03" -and (Test-Path (Join-Path $scriptDir "generate_busy_ani.ps1"))) {
        Write-Host "[$($c.Id)] $($c.Desc) -> Running specialized generator..." -ForegroundColor Yellow
        & (Join-Path $scriptDir "generate_busy_ani.ps1") | Out-Null
        Write-Host "  -> Done: $($c.Name)" -ForegroundColor Green
        continue
    }
    
    $cellLeft = $c.Col * $colW
    $cellTop = $c.Row * $rowH
    
    $cellBmp = New-Object System.Drawing.Bitmap($colW, $rowH)
    $g = [System.Drawing.Graphics]::FromImage($cellBmp)
    $g.DrawImage($sheet, (New-Object System.Drawing.Rectangle(0, 0, $colW, $rowH)), (New-Object System.Drawing.Rectangle($cellLeft, $cellTop, $colW, $rowH)), [System.Drawing.GraphicsUnit]::Pixel)
    $g.Dispose()
    
    # Clear header
    for ($y = 0; $y -lt 68; $y++) {
        for ($x = 0; $x -lt $colW; $x++) {
            $cellBmp.SetPixel($x, $y, [System.Drawing.Color]::FromArgb(0, 0, 0, 0))
        }
    }
    
    # FloodFill transparency
    $visited = New-Object "bool[,]" $colW, $rowH
    $queue = New-Object System.Collections.Generic.Queue[System.Drawing.Point]
    for ($x = 0; $x -lt $colW; $x++) {
        $queue.Enqueue((New-Object System.Drawing.Point($x, 68)))
        $queue.Enqueue((New-Object System.Drawing.Point($x, ($rowH - 1))))
    }
    for ($y = 68; $y -lt $rowH; $y++) {
        $queue.Enqueue((New-Object System.Drawing.Point(0, $y)))
        $queue.Enqueue((New-Object System.Drawing.Point(($colW - 1), $y)))
    }
    while ($queue.Count -gt 0) {
        $p = $queue.Dequeue()
        $px = $p.X; $py = $p.Y
        if ($px -lt 0 -or $px -ge $colW -or $py -lt 68 -or $py -ge $rowH) { continue }
        if ($visited[$px, $py]) { continue }
        $visited[$px, $py] = $true
        
        $col = $cellBmp.GetPixel($px, $py)
        $maxDiff = [Math]::Max([Math]::Abs($col.R - $col.G), [Math]::Max([Math]::Abs($col.G - $col.B), [Math]::Abs($col.R - $col.B)))
        if ($maxDiff -le 18 -and $col.R -ge 210 -and $col.G -ge 210 -and $col.B -ge 210) {
            $cellBmp.SetPixel($px, $py, [System.Drawing.Color]::FromArgb(0, 0, 0, 0))
            if ($px -gt 0) { $queue.Enqueue((New-Object System.Drawing.Point(($px - 1), $py))) }
            if ($px -lt $colW - 1) { $queue.Enqueue((New-Object System.Drawing.Point(($px + 1), $py))) }
            if ($py -gt 68) { $queue.Enqueue((New-Object System.Drawing.Point($px, ($py - 1)))) }
            if ($py -lt $rowH - 1) { $queue.Enqueue((New-Object System.Drawing.Point($px, ($py + 1)))) }
        }
    }
    
    $destBmp = Fit-BitmapToCanvas $cellBmp 64 4
    
    $ms = New-Object System.IO.MemoryStream
    $destBmp.Save($ms, [System.Drawing.Imaging.ImageFormat]::Png)
    $pngBytes = $ms.ToArray()
    $ms.Dispose()
    
    if ($c.Type -eq "ANI") {
        $frames = @()
        for ($f = 0; $f -lt 10; $f++) { $frames += ,$pngBytes }
        $aniBytes = Build-AniBinary $frames $c.Hx $c.Hy 8
        [System.IO.File]::WriteAllBytes($outPath, $aniBytes)
    } else {
        $curBytes = Build-CurBinary $pngBytes $c.Hx $c.Hy 64 64
        [System.IO.File]::WriteAllBytes($outPath, $curBytes)
    }
    
    Write-Host "[$($c.Id)] $($c.Desc) -> Done: $($c.Name)" -ForegroundColor Green
    
    $destBmp.Dispose()
    $cellBmp.Dispose()
}

$sheet.Dispose()

Write-Host ""
Write-Host "==========================================================" -ForegroundColor Cyan
Write-Host "   All 17 cursors successfully converted and generated!   " -ForegroundColor Cyan
Write-Host "==========================================================" -ForegroundColor Cyan
