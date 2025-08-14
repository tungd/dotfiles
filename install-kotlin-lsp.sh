#!/bin/bash

set -e

echo "🔍 Fetching latest kotlin-lsp release information..."

# Get latest release info from GitHub API
RELEASE_INFO=$(curl -s https://api.github.com/repos/Kotlin/kotlin-lsp/releases/latest)
TAG_NAME=$(echo "$RELEASE_INFO" | grep '"tag_name"' | sed 's/.*"tag_name": "\([^"]*\)".*/\1/')
VERSION=$(echo "$TAG_NAME" | sed 's/kotlin-lsp\/v//')

echo "📦 Latest version: $VERSION"

# Construct download URL
DOWNLOAD_URL="https://download-cdn.jetbrains.com/kotlin-lsp/$VERSION/kotlin-$VERSION.zip"
ZIP_FILE="kotlin-$VERSION.zip"

echo "⬇️  Downloading kotlin-lsp $VERSION..."
curl -L -o "$ZIP_FILE" "$DOWNLOAD_URL"

echo "📂 Creating ~/.local directory if it doesn't exist..."
mkdir -p ~/.local

echo "🗜️  Creating installation directory..."
KOTLIN_DIR="kotlin-$VERSION"
INSTALL_DIR="$HOME/.local/$KOTLIN_DIR"

# Remove existing installation if it exists
if [ -d "$INSTALL_DIR" ]; then
    echo "🗑️  Removing existing installation..."
    rm -rf "$INSTALL_DIR"
fi

mkdir -p "$INSTALL_DIR"

echo "📦 Extracting to $INSTALL_DIR..."
unzip -q "$ZIP_FILE" -d "$INSTALL_DIR"

echo "🔗 Creating alias for kotlin-language-server..."

# Remove existing symlink if it exists
if [ -L ~/.local/bin/kotlin-language-server ]; then
    rm ~/.local/bin/kotlin-language-server
fi

# Create ~/.local/bin directory if it doesn't exist
mkdir -p ~/.local/bin

# Create symlink
ln -sf "../$KOTLIN_DIR/kotlin-lsp.sh" ~/.local/bin/kotlin-language-server

echo "🧹 Cleaning up downloaded zip file..."
rm "$ZIP_FILE"

echo "✅ kotlin-lsp $VERSION installed successfully!"
echo "📍 Installed to: ~/.local/$KOTLIN_DIR"
echo "🔗 Symlink created: ~/.local/bin/kotlin-language-server -> ../$KOTLIN_DIR/kotlin-lsp.sh"
echo ""
echo "Make sure ~/.local/bin is in your PATH to use the kotlin-language-server command."