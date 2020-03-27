#!/bin/sh

BASEDIR="$(dirname "$0")"
INSTALL_DIR="$HOME/.local/share/verishot/"
EXECUTABLE_PATH="$HOME/.local/bin/verishot"

[ $# -lt 1 ] || {
    echo "Usage: $0"
}

command -v dotnet >/dev/null 2>&1 || {
    echo "Could not find dotnet executable make sure you have it installed and in your PATH"
    exit 1
}

prompt() {
    while [ -z "$PROMPT_ANSWER" ]; do
        printf "%s [Y/n] " "$1"
        read -r ANSWER
        case "$ANSWER" in
            n|N) return 1;;
            y|Y|'') return 0;;
        esac
    done
}

install() {
    echo "==== Installing Verishot ===="
    # Delete old version if it exists
    rm -rf "$INSTALL_DIR"
    rm -f "$EXECUTABLE_PATH"

    mkdir -p ~/.local/share/
    mkdir -p ~/.local/bin/

    echo "> Copying program files to $INSTALL_DIR"
    cp -r "$BASEDIR/bin/release/netcoreapp3.1/linux-x64/publish/" "$HOME/.local/share/verishot/"

    echo "> Creating verishot executable in $EXECUTABLE_PATH"
    ln -s "$INSTALL_DIR/Verishot" "$EXECUTABLE_PATH"
    chmod u+x "$EXECUTABLE_PATH"

    echo "==== Installation Successful! ===="
}

echo "==== Building Verishot ===="
dotnet publish -c release -r linux-x64
echo "==== Building Verishot complete! ===="
echo

echo "This script can install the files required for verishot in ~/.local"
prompt "Would you like to install verishot to ~/.local/share?" && {
    echo
    if [ -e "$INSTALL_DIR" ]; then
        prompt "Verishot is already installed. Overwrite?" && install
    else
        install
    fi
}

echo
echo "All done!"
