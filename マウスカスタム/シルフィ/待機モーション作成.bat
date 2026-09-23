@echo off
title Create Busy Animation
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0generate_busy_ani.ps1"
echo.
pause
