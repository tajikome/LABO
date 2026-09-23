param(
    [switch]$Restore = $false
)

Add-Type -TypeDefinition @"
using System;
using System.Runtime.InteropServices;

public class CursorHelper {
    [DllImport("user32.dll", SetLastError = true)]
    public static extern bool SystemParametersInfo(uint uiAction, uint uiParam, IntPtr pvParam, uint fWinIni);

    public const uint SPI_SETCURSORS = 0x0057;
    public const uint SPIF_UPDATEINIFILE = 0x01;
    public const uint SPIF_SENDCHANGE = 0x02;

    public static void ReloadCursors() {
        SystemParametersInfo(SPI_SETCURSORS, 0, IntPtr.Zero, SPIF_UPDATEINIFILE | SPIF_SENDCHANGE);
    }
}
"@

$regPath = "HKCU:\Control Panel\Cursors"

if ($Restore) {
    Write-Host "Restoring default Windows cursors..." -ForegroundColor Yellow
    Set-ItemProperty -Path $regPath -Name "(Default)" -Value "Windows Default"
    Set-ItemProperty -Path $regPath -Name "Scheme Source" -Value 0
    $cursorNames = @("Arrow", "Help", "AppStarting", "Wait", "Crosshair", "IBeam", "NWPen", "No", "SizeNS", "SizeWE", "SizeNWSE", "SizeNESW", "SizeAll", "UpArrow", "Hand", "Pin", "Person")
    foreach ($c in $cursorNames) {
        Set-ItemProperty -Path $regPath -Name $c -Value ""
    }
    [CursorHelper]::ReloadCursors()
    Write-Host "Restored Windows default cursors successfully!" -ForegroundColor Green
    exit 0
}

$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
if (-not $scriptDir) { $scriptDir = (Get-Location).Path }

Write-Host "==========================================================" -ForegroundColor Cyan
Write-Host "      Applying Silphy Cursors to Windows Scheme           " -ForegroundColor Cyan
Write-Host "==========================================================" -ForegroundColor Cyan
Write-Host ""

$mapping = @{
    "Arrow"       = Join-Path $scriptDir "01_Normal_Select.cur"
    "Help"        = Join-Path $scriptDir "02_Help_Select.cur"
    "Wait"        = Join-Path $scriptDir "03_Busy.ani"
    "AppStarting" = Join-Path $scriptDir "04_Working.ani"
    "UpArrow"     = Join-Path $scriptDir "05_Area_Select.cur"
    "IBeam"       = Join-Path $scriptDir "06_Text_Select.cur"
    "NWPen"       = Join-Path $scriptDir "07_Handwriting.cur"
    "Hand"        = Join-Path $scriptDir "08_Link_Select.cur"
    "SizeAll"     = Join-Path $scriptDir "09_Move.cur"
    "No"          = Join-Path $scriptDir "10_Unavailable.cur"
    "SizeNS"      = Join-Path $scriptDir "11_Vertical_Resize.cur"
    "SizeWE"      = Join-Path $scriptDir "12_Horizontal_Resize.cur"
    "SizeNWSE"    = Join-Path $scriptDir "13_Diagonal_Resize_1.cur"
    "SizeNESW"    = Join-Path $scriptDir "14_Diagonal_Resize_2.cur"
    "Crosshair"   = Join-Path $scriptDir "15_Precision_Select.cur"
    "Pin"         = Join-Path $scriptDir "16_Location_Select.cur"
    "Person"      = Join-Path $scriptDir "17_Person_Select.cur"
}

$missing = @()
foreach ($key in $mapping.Keys) {
    if (-not (Test-Path $mapping[$key])) {
        $missing += $mapping[$key]
    }
}
if ($missing.Count -gt 0) {
    Write-Warning "Missing cursor files:"
    $missing | ForEach-Object { Write-Warning "  - $_" }
    exit 1
}

Set-ItemProperty -Path $regPath -Name "(Default)" -Value "Silphy Cursor"
Set-ItemProperty -Path $regPath -Name "Scheme Source" -Value 1

foreach ($key in $mapping.Keys) {
    Set-ItemProperty -Path $regPath -Name $key -Value $mapping[$key]
    Write-Host "  Set: $key -> $(Split-Path -Leaf $mapping[$key])" -ForegroundColor Gray
}

[CursorHelper]::ReloadCursors()

Write-Host ""
Write-Host "==========================================================" -ForegroundColor Green
Write-Host "  Silphy cursor set applied successfully to Windows!      " -ForegroundColor Green
Write-Host "==========================================================" -ForegroundColor Green
