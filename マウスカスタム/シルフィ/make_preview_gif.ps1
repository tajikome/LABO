Add-Type -AssemblyName System.Drawing

$previewDir = "c:\Users\kenya\Documents\LABO\マウスカスタム\シルフィ\preview_frames"
$gifOut = "c:\Users\kenya\Documents\LABO\マウスカスタム\シルフィ\03_Busy_preview.gif"

# Load frames
$frames = @()
for ($i = 0; $i -lt 10; $i++) {
    $p = Join-Path $previewDir ("frame_{0:D2}.png" -f $i)
    $frames += [System.Drawing.Bitmap]::FromFile($p)
}

# Create a sprite sheet / preview image (e.g. 64x10 = 640 x 64) as well
$sheetBmp = New-Object System.Drawing.Bitmap(640, 64)
$g = [System.Drawing.Graphics]::FromImage($sheetBmp)
# dark background for visibility
$g.Clear([System.Drawing.Color]::FromArgb(40, 44, 52))

for ($i = 0; $i -lt 10; $i++) {
    $g.DrawImage($frames[$i], ($i * 64), 0, 64, 64)
}
$g.Dispose()

$spritePath = "c:\Users\kenya\Documents\LABO\マウスカスタム\シルフィ\03_Busy_preview_strip.png"
$sheetBmp.Save($spritePath, [System.Drawing.Imaging.ImageFormat]::Png)
$sheetBmp.Dispose()

# Create animated GIF using standard Net GIF encoding
# Since System.Drawing has limited native multi-frame GIF writer,
# we can write a clean GIF89a binary encoder for the 10 frames:
# Each frame: 64x64, 256 colors
function Save-AnimatedGif($bmpList, $outPath, $delayMs) {
    # Simple binary combiner for GIF89a
    $ms = New-Object System.IO.MemoryStream
    $delay100th = [int]($delayMs / 10)
    
    for ($i = 0; $i -lt $bmpList.Count; $i++) {
        $frameStream = New-Object System.IO.MemoryStream
        # Draw on checker/gray background to prevent GIF transparency artifact in preview
        $previewBmp = New-Object System.Drawing.Bitmap(64, 64)
        $pg = [System.Drawing.Graphics]::FromImage($previewBmp)
        $pg.Clear([System.Drawing.Color]::FromArgb(240, 240, 245))
        $pg.DrawImage($bmpList[$i], 0, 0, 64, 64)
        $pg.Dispose()
        $previewBmp.Save($frameStream, [System.Drawing.Imaging.ImageFormat]::Gif)
        $previewBmp.Dispose()
        $bytes = $frameStream.ToArray()
        $frameStream.Dispose()
        
        if ($i -eq 0) {
            # Header + Logical Screen Descriptor (13 bytes)
            $ms.Write($bytes, 0, 13)
            # Netscape Application Block for looping
            $appBlock = [byte[]]@(
                0x21, 0xFF, 0x0B,
                0x4E, 0x45, 0x54, 0x53, 0x43, 0x41, 0x50, 0x45, 0x32, 0x2E, 0x30, # NETSCAPE2.0
                0x03, 0x01, 0x00, 0x00, 0x00
            )
            $ms.Write($appBlock, 0, $appBlock.Length)
        }
        
        # Graphic Control Extension for frame delay
        $gce = [byte[]]@(
            0x21, 0xF9, 0x04,
            0x00, # Packed fields
            ($delay100th -band 0xFF), (($delay100th -shr 8) -band 0xFF), # Delay
            0x00, # Transparent color index
            0x00  # Block terminator
        )
        $ms.Write($gce, 0, $gce.Length)
        
        # Image Descriptor + Image Data (find 0x2C)
        $imgDescIdx = -1
        for ($k = 13; $k -lt $bytes.Length; $k++) {
            if ($bytes[$k] -eq 0x2C) {
                $imgDescIdx = $k
                break
            }
        }
        if ($imgDescIdx -ge 0) {
            # Write until end minus trailer 0x3B
            $len = $bytes.Length - $imgDescIdx
            if ($bytes[$bytes.Length - 1] -eq 0x3B) { $len-- }
            $ms.Write($bytes, $imgDescIdx, $len)
        }
    }
    # GIF Trailer
    $ms.WriteByte(0x3B)
    [System.IO.File]::WriteAllBytes($outPath, $ms.ToArray())
    $ms.Dispose()
}

Save-AnimatedGif $frames $gifOut 133

foreach ($f in $frames) { $f.Dispose() }
Write-Output "Generated preview GIF and sprite strip successfully."
