@echo off
title Restore Default Cursors
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0apply_cursors.ps1" -Restore
echo.
pause
