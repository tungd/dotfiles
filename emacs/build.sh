#!/usr/bin/env bash
#
# build.sh - Setup dependencies and build latest Emacs from git
#
# Usage:
#   ./emacs/build.sh             # Build / update Emacs
#   FORCE_CONFIG=1 ./emacs/build.sh  # Force re-running ./configure
#
set -euo pipefail

BUILD_DIR="${BUILD_DIR:-$HOME/.local/src/emacs}"
APP_DIR="${APP_DIR:-$HOME/Applications/Emacs.app}"
INSTALL_PREFIX="${INSTALL_PREFIX:-$HOME/.local}"
REPO_URL="${REPO_URL:-https://github.com/emacs-mirror/emacs.git}"
BRANCH="${BRANCH:-master}"
NPROC="$(sysctl -n hw.ncpu 2>/dev/null || echo 4)"

echo "============================================="
echo " Emacs Bootstrap & Build Script"
echo " Build Directory : $BUILD_DIR"
echo " Install Target  : $APP_DIR"
echo " CLI Prefix      : $INSTALL_PREFIX/bin"
echo " Branch          : $BRANCH"
echo "============================================="

# 1. Install / check MacPorts dependencies
DEPS=(
    autoconf
    automake
    libtool
    texinfo
    pkgconfig
    gmp
    gnutls
    libxml2
    ncurses
    sqlite3
    lcms2
    gcc15
    tree-sitter
)

if command -v port >/dev/null 2>&1; then
    echo "==> Checking MacPorts dependencies..."
    missing=()
    for dep in "${DEPS[@]}"; do
        if ! port installed "$dep" 2>/dev/null | grep -q 'active'; then
            missing+=("$dep")
        fi
    done

    if [ ${#missing[@]} -gt 0 ]; then
        echo "==> Installing missing MacPorts dependencies: ${missing[*]}"
        sudo port install "${missing[@]}"
    else
        echo "==> All required MacPorts packages are active."
    fi
else
    echo "WARNING: MacPorts ('port') command not found in PATH." >&2
fi

# 2. Clone or update Emacs repository
if [ ! -d "$BUILD_DIR/.git" ]; then
    echo "==> Cloning Emacs ($BRANCH) into $BUILD_DIR..."
    mkdir -p "$(dirname "$BUILD_DIR")"
    git clone --filter=blob:none -b "$BRANCH" "$REPO_URL" "$BUILD_DIR"
else
    echo "==> Updating Emacs in $BUILD_DIR..."
    cd "$BUILD_DIR"
    git fetch origin "$BRANCH"
    git checkout "$BRANCH"
    git pull --ff-only origin "$BRANCH"
fi

cd "$BUILD_DIR"

# 3. Environment configuration for MacPorts headers & libraries
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
export PKG_CONFIG_PATH="/opt/local/lib/pkgconfig${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}"
export CC="clang"
export CFLAGS="-O2 -I/opt/local/include -I/opt/local/include/gcc15"
export LDFLAGS="-L/opt/local/lib -L/opt/local/lib/gcc15 -Wl,-rpath,/opt/local/lib -Wl,-rpath,/opt/local/lib/gcc15"

# 4. Generate configure script if needed
if [ ! -f "configure" ]; then
    echo "==> Running autogen.sh..."
    ./autogen.sh
fi

# 5. Configure Emacs
if [ ! -f "Makefile" ] || [ "${FORCE_CONFIG:-0}" = "1" ]; then
    echo "==> Running ./configure..."
    ./configure \
        --prefix="$INSTALL_PREFIX" \
        --with-ns \
        --with-native-compilation=aot \
        --with-tree-sitter \
        --with-sqlite3 \
        --with-gnutls \
        --with-xml2 \
        --with-lcms2 \
        --without-rsvg \
        --without-gif \
        --without-x \
        --without-dbus \
        --without-imagemagick \
        --disable-silent-rules
fi

# 6. Compile
echo "==> Compiling Emacs ($NPROC jobs)..."
make -j"$NPROC"
make install

# 7. Install Emacs.app
echo "==> Installing Emacs.app to $APP_DIR..."
if [ -d "$APP_DIR" ] || [ -L "$APP_DIR" ]; then
    rm -rf "$APP_DIR"
fi
mkdir -p "$(dirname "$APP_DIR")"
cp -R nextstep/Emacs.app "$APP_DIR"
chmod -R u+w "$APP_DIR"

# Ad-hoc codesign to avoid macOS gatekeeper warnings
echo "==> Ad-hoc codesigning $APP_DIR..."
/usr/bin/codesign --force --deep --sign - "$APP_DIR"

# 8. Setup CLI wrapper scripts in ~/.local/bin
echo "==> Setting up CLI wrappers in $INSTALL_PREFIX/bin..."
mkdir -p "$INSTALL_PREFIX/bin"
rm -f "$INSTALL_PREFIX/bin/emacs" "$INSTALL_PREFIX/bin/emacsclient"

cat << EOF > "$INSTALL_PREFIX/bin/emacs"
#!/bin/sh
exec "$APP_DIR/Contents/MacOS/Emacs" "\$@"
EOF
chmod +x "$INSTALL_PREFIX/bin/emacs"

cat << EOF > "$INSTALL_PREFIX/bin/emacsclient"
#!/bin/sh
exec "$APP_DIR/Contents/MacOS/bin/emacsclient" "\$@"
EOF
chmod +x "$INSTALL_PREFIX/bin/emacsclient"

# 9. Link for MacPorts compatibility if directory exists
if [ -d "/Applications/MacPorts" ] && [ "$APP_DIR" != "/Applications/MacPorts/Emacs.app" ]; then
    if [ ! -d "/Applications/MacPorts/Emacs.app" ] || [ -L "/Applications/MacPorts/Emacs.app" ]; then
        ln -sfn "$APP_DIR" "/Applications/MacPorts/Emacs.app" 2>/dev/null || true
    fi
fi

echo "============================================="
echo " Emacs build complete!"
echo " App location : $APP_DIR"
echo " CLI command  : $INSTALL_PREFIX/bin/emacs"
echo " Version info : $("$INSTALL_PREFIX/bin/emacs" --version | head -1)"
echo "============================================="
