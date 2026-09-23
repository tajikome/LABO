@echo off
title Custom Cursor Converter
echo ========================================================
echo  Custom Cursor Batch Converter
echo ========================================================
echo.

if "%~1"=="" (
    powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0convert_all_cursors.ps1"
) else (
    powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0convert_all_cursors.ps1" -InputPath "%~1"
)

echo.
pause
