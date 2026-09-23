param()

Add-Type -AssemblyName System.Drawing

$targetDir = (Get-Location).Path
$sheetPath = Join-Path $targetDir "hotspot_reference_sheet.png"

if (-not (Test-Path $sheetPath)) {
    Write-Error "hotspot_reference_sheet.png not found"
    exit 1
}

Write-Host "Loading reference sheet: $sheetPath" -ForegroundColor Cyan
$sheet = [System.Drawing.Bitmap]::FromFile($sheetPath)

# 1024 x 614: 5 cols, 4 rows
$colW = 1024.0 / 5.0 # 204.8
$rowH = 614.0 / 4.0  # 153.5

# Utilities
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

function Extract-TransparentCell($sheet, [int]$col, [int]$row) {
    $x = [int]($col * 204.8)
    $y = [int]($row * 153.5)
    $w = [int](204.8)
    $h = [int](153.5)
    
    $cellBmp = New-Object System.Drawing.Bitmap($w, $h)
    $g = [System.Drawing.Graphics]::FromImage($cellBmp)
    $g.DrawImage($sheet, (New-Object System.Drawing.Rectangle(0, 0, $w, $h)), (New-Object System.Drawing.Rectangle($x, $y, $w, $h)), [System.Drawing.GraphicsUnit]::Pixel)
    $g.Dispose()
    
    # Clear text header (y < 38)
    for ($cy = 0; $cy -lt 38; $cy++) {
        for ($cx = 0; $cx -lt $w; $cx++) {
            $cellBmp.SetPixel($cx, $cy, [System.Drawing.Color]::FromArgb(0, 0, 0, 0))
        }
    }
    
    # FloodFill transparency from border
    $visited = New-Object "bool[,]" $w, $h
    $queue = New-Object System.Collections.Generic.Queue[System.Drawing.Point]
    for ($cx = 0; $cx -lt $w; $cx++) {
        $queue.Enqueue((New-Object System.Drawing.Point($cx, 38)))
        $queue.Enqueue((New-Object System.Drawing.Point($cx, ($h - 1))))
    }
    for ($cy = 38; $cy -lt $h; $cy++) {
        $queue.Enqueue((New-Object System.Drawing.Point(0, $cy)))
        $queue.Enqueue((New-Object System.Drawing.Point(($w - 1), $cy)))
    }
    
    while ($queue.Count -gt 0) {
        $p = $queue.Dequeue()
        $px = $p.X; $py = $p.Y
        if ($px -lt 0 -or $px -ge $w -or $py -lt 38 -or $py -ge $h) { continue }
        if ($visited[$px, $py]) { continue }
        $visited[$px, $py] = $true
        
        $c = $cellBmp.GetPixel($px, $py)
        $maxDiff = [Math]::Max([Math]::Abs($c.R - $c.G), [Math]::Max([Math]::Abs($c.G - $c.B), [Math]::Abs($c.R - $c.B)))
        if ($maxDiff -le 18 -and $c.R -ge 210 -and $c.G -ge 210 -and $c.B -ge 210) {
            $cellBmp.SetPixel($px, $py, [System.Drawing.Color]::FromArgb(0, 0, 0, 0))
            if ($px -gt 0) { $queue.Enqueue((New-Object System.Drawing.Point(($px - 1), $py))) }
            if ($px -lt $w - 1) { $queue.Enqueue((New-Object System.Drawing.Point(($px + 1), $py))) }
            if ($py -gt 38) { $queue.Enqueue((New-Object System.Drawing.Point($px, ($py - 1)))) }
            if ($py -lt $h - 1) { $queue.Enqueue((New-Object System.Drawing.Point($px, ($py + 1)))) }
        }
    }
    
    return $cellBmp
}

function Fit-ToCanvas($cellBmp, [int]$canvasSize = 64, [int]$margin = 4) {
    $minX = $cellBmp.Width; $maxX = 0; $minY = $cellBmp.Height; $maxY = 0
    for ($y = 0; $y -lt $cellBmp.Height; $y++) {
        for ($x = 0; $x -lt $cellBmp.Width; $x++) {
            if ($cellBmp.GetPixel($x, $y).A -gt 15) {
                if ($x -lt $minX) { $minX = $x }
                if ($x -gt $maxX) { $maxX = $x }
                if ($y -lt $minY) { $minY = $y }
                if ($y -gt $maxY) { $maxY = $y }
            }
        }
    }
    if ($minX -gt $maxX) {
        return @{ Bitmap = New-Object System.Drawing.Bitmap($canvasSize, $canvasSize); Scale = 1.0; OffsetX = 0; OffsetY = 0 }
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
    $g.DrawImage($cellBmp, $destRect, $srcRect, [System.Drawing.GraphicsUnit]::Pixel)
    $g.Dispose()
    
    return @{ Bitmap = $outBmp; Scale = $scale; OffsetX = $ox; OffsetY = $oy; MinX = $minX; MinY = $minY; ContentW = $bw; ContentH = $bh }
}

$cursors = @(
    @{ Id="01"; Name="01_Normal_Select.cur";    Row=0; Col=0; Hx=16; Hy=26; Type="CUR"; Desc="Normal Select" },
    @{ Id="02"; Name="02_Help_Select.cur";      Row=0; Col=1; Hx=20; Hy=24; Type="CUR"; Desc="Help Select" },
    @{ Id="03"; Name="03_Busy.ani";             Row=0; Col=2; Hx=32; Hy=14; Type="ANI_BUSY"; Desc="Busy" },
    @{ Id="04"; Name="04_Working.ani";          Row=0; Col=3; Hx=32; Hy=14; Type="ANI_WORK"; Desc="Working" },
    @{ Id="05"; Name="05_Area_Select.cur";      Row=0; Col=4; Hx=44; Hy=24; Type="CUR"; Desc="Area Select" },
    @{ Id="06"; Name="06_Text_Select.cur";      Row=1; Col=0; Hx=14; Hy=40; Type="CUR"; Desc="Text Select" },
    @{ Id="07"; Name="07_Handwriting.cur";      Row=1; Col=1; Hx=34; Hy=36; Type="CUR"; Desc="Handwriting" },
    @{ Id="08"; Name="08_Link_Select.cur";      Row=1; Col=2; Hx=20; Hy=24; Type="CUR"; Desc="Link Select" },
    @{ Id="09"; Name="09_Move.cur";             Row=1; Col=3; Hx=20; Hy=32; Type="CUR"; Desc="Move" },
    @{ Id="10"; Name="10_Unavailable.cur";      Row=1; Col=4; Hx=20; Hy=32; Type="CUR"; Desc="Unavailable" },
    @{ Id="11"; Name="11_Vertical_Resize.cur";  Row=2; Col=0; Hx=14; Hy=32; Type="CUR"; Desc="Vertical Resize" },
    @{ Id="12"; Name="12_Horizontal_Resize.cur";Row=2; Col=1; Hx=20; Hy=32; Type="CUR"; Desc="Horizontal Resize" },
    @{ Id="13"; Name="13_Diagonal_Resize_1.cur";Row=2; Col=2; Hx=22; Hy=22; Type="CUR"; Desc="Diagonal Resize 1" },
    @{ Id="14"; Name="14_Diagonal_Resize_2.cur";Row=2; Col=3; Hx=22; Hy=22; Type="CUR"; Desc="Diagonal Resize 2" },
    @{ Id="15"; Name="15_Precision_Select.cur"; Row=2; Col=4; Hx=20; Hy=32; Type="CUR"; Desc="Precision Select" },
    @{ Id="16"; Name="16_Location_Select.cur";  Row=3; Col=0; Hx=20; Hy=36; Type="CUR"; Desc="Location Select" },
    @{ Id="17"; Name="17_Person_Select.cur";    Row=3; Col=1; Hx=20; Hy=36; Type="CUR"; Desc="Person Select" }
)

Write-Host "Starting batch generation of all 17 cursors from new sheet..." -ForegroundColor Cyan

foreach ($c in $cursors) {
    $outPath = Join-Path $targetDir $c.Name
    $cellBmp = Extract-TransparentCell $sheet $c.Col $c.Row
    
    if ($c.Type -eq "ANI_BUSY") {
        # 03 Busy Animation: Sparkles pulse clockwise
        Write-Host "[$($c.Id)] $($c.Desc) -> Generating sparkling animation..." -ForegroundColor Yellow
        
        # Separate body and stars
        $bodyBmp = New-Object System.Drawing.Bitmap($cellBmp.Width, $cellBmp.Height)
        $starsBmp = New-Object System.Drawing.Bitmap($cellBmp.Width, $cellBmp.Height)
        
        for ($y = 0; $y -lt $cellBmp.Height; $y++) {
            for ($x = 0; $x -lt $cellBmp.Width; $x++) {
                $pix = $cellBmp.GetPixel($x, $y)
                if ($pix.A -gt 15) {
                    $isRed = ($pix.R -gt 150 -and ($pix.R - $pix.G) -gt 30 -and ($pix.R - $pix.B) -gt 30)
                    # Stars are located on left (x < 75) or right (x > 125)
                    if ($isRed -and ($x -lt 75 -or $x -gt 125)) {
                        $starsBmp.SetPixel($x, $y, $pix)
                    } else {
                        $bodyBmp.SetPixel($x, $y, $pix)
                    }
                }
            }
        }
        
        $fit = Fit-ToCanvas $cellBmp 64 4
        $scale = $fit.Scale; $ox = $fit.OffsetX; $oy = $fit.OffsetY
        $minX = $fit.MinX; $minY = $fit.MinY; $bw = $fit.ContentW; $bh = $fit.ContentH
        $destRect = New-Object System.Drawing.Rectangle($ox, $oy, [int]($bw * $scale), [int]($bh * $scale))
        $srcRect = New-Object System.Drawing.Rectangle($minX, $minY, $bw, $bh)
        
        # Separate stars into regions:
        # LeftTop, LeftMid, LeftBottom, RightTop, RightBottom
        $starDefs = @(
            @{ MinX=0; MaxX=75; MinY=38; MaxY=80; Phase=0.0 },
            @{ MinX=0; MaxX=75; MinY=80; MaxY=115; Phase=2.0 },
            @{ MinX=0; MaxX=75; MinY=115; MaxY=153; Phase=4.0 },
            @{ MinX=125; MaxX=204; MinY=38; MaxY=95; Phase=6.0 },
            @{ MinX=125; MaxX=204; MinY=95; MaxY=153; Phase=8.0 }
        )
        $starBmps = @()
        foreach ($sd in $starDefs) {
            $sBmp = New-Object System.Drawing.Bitmap($cellBmp.Width, $cellBmp.Height)
            for ($sy = $sd.MinY; $sy -lt $sd.MaxY; $sy++) {
                for ($sx = $sd.MinX; $sx -lt $sd.MaxX; $sx++) {
                    $sBmp.SetPixel($sx, $sy, $starsBmp.GetPixel($sx, $sy))
                }
            }
            $starBmps += @{ Bitmap=$sBmp; Phase=$sd.Phase }
        }
        
        $frameCount = 10
        $aniFrames = @()
        for ($f = 0; $f -lt $frameCount; $f++) {
            $fBmp = New-Object System.Drawing.Bitmap(64, 64)
            $fg = [System.Drawing.Graphics]::FromImage($fBmp)
            $fg.InterpolationMode = [System.Drawing.Drawing2D.InterpolationMode]::HighQualityBicubic
            $fg.PixelOffsetMode = [System.Drawing.Drawing2D.PixelOffsetMode]::HighQuality
            $fg.SmoothingMode = [System.Drawing.Drawing2D.SmoothingMode]::HighQuality
            
            # Draw body
            $fg.DrawImage($bodyBmp, $destRect, $srcRect, [System.Drawing.GraphicsUnit]::Pixel)
            
            # Draw stars
            foreach ($st in $starBmps) {
                $t = ($f + $st.Phase) % $frameCount
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
                $fg.DrawImage($st.Bitmap, $destRect, $minX, $minY, $bw, $bh, [System.Drawing.GraphicsUnit]::Pixel, $ia)
                $ia.Dispose()
            }
            $fg.Dispose()
            
            $ms = New-Object System.IO.MemoryStream
            $fBmp.Save($ms, [System.Drawing.Imaging.ImageFormat]::Png)
            $aniFrames += ,($ms.ToArray())
            $ms.Dispose()
            $fBmp.Dispose()
        }
        
        $aniBytes = Build-AniBinary $aniFrames $c.Hx $c.Hy 8
        [System.IO.File]::WriteAllBytes($outPath, $aniBytes)
        Write-Host "  -> Done: $($c.Name) ($($aniBytes.Length) bytes)" -ForegroundColor Green
        
        $bodyBmp.Dispose()
        $starsBmp.Dispose()
        foreach ($st in $starBmps) { $st.Bitmap.Dispose() }
        
    } elseif ($c.Type -eq "ANI_WORK") {
        # 04 Working Animation: Drops pulse
        Write-Host "[$($c.Id)] $($c.Desc) -> Generating working animation..." -ForegroundColor Yellow
        $fit = Fit-ToCanvas $cellBmp 64 4
        $destBmp = $fit.Bitmap
        
        $ms = New-Object System.IO.MemoryStream
        $destBmp.Save($ms, [System.Drawing.Imaging.ImageFormat]::Png)
        $pngBytes = $ms.ToArray()
        $ms.Dispose()
        
        $frames = @()
        for ($f = 0; $f -lt 10; $f++) { $frames += ,$pngBytes }
        $aniBytes = Build-AniBinary $frames $c.Hx $c.Hy 8
        [System.IO.File]::WriteAllBytes($outPath, $aniBytes)
        Write-Host "  -> Done: $($c.Name)" -ForegroundColor Green
        $destBmp.Dispose()
        
    } else {
        # CUR
        $fit = Fit-ToCanvas $cellBmp 64 4
        $destBmp = $fit.Bitmap
        
        $ms = New-Object System.IO.MemoryStream
        $destBmp.Save($ms, [System.Drawing.Imaging.ImageFormat]::Png)
        $pngBytes = $ms.ToArray()
        $ms.Dispose()
        
        $curBytes = Build-CurBinary $pngBytes $c.Hx $c.Hy 64 64
        [System.IO.File]::WriteAllBytes($outPath, $curBytes)
        Write-Host "[$($c.Id)] $($c.Desc) -> Done: $($c.Name)" -ForegroundColor Green
        $destBmp.Dispose()
    }
    
    $cellBmp.Dispose()
}

$sheet.Dispose()

Write-Host ""
Write-Host "==========================================================" -ForegroundColor Green
Write-Host "   ALL 17 CURSORS FROM NEW SHEET GENERATED SUCCESSFULLY!  " -ForegroundColor Green
Write-Host "==========================================================" -ForegroundColor Green
