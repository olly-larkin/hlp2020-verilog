#!/bin/bash
set -e

dotnet publish -c release -r linux-x64 || "Unable to build project"
echo "Please now add the folder containing `Verishot` executable in your `PATH` environment variables."
echo "See README.md for more info"