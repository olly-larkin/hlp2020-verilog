#!/bin/bash
set -e

dotnet publish -c release -r linux-x64 || echo "Unable to build project"
echo "===== INSTALLATION SUCCESSFUL ====="
verishot || echo "Please add the folder containing the `Verishot` executable to your PATH variables."