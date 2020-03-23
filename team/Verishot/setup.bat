@echo off
REM Setup file for Windows
dotnet publish -c release -r win-x64 || echo "Unable to build project"
echo Please now add the folder containing `Verishot.exe` in your `PATH` environment variables.
echo See README.md for more info

