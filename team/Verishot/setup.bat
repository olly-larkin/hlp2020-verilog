@echo off
REM Setup file for Windows
dotnet publish -c release -r win-x64 || (echo "Unable to build project" && exit 1)

echo "===== INSTALLATION SUCCESSFUL ====="

verishot || "Please add the folder containing the `Verishot` executable to your PATH variables."

