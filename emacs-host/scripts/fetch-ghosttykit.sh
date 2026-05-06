#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
GHOSTTY_SHA="22fa801f88f96fa842e54ecce6c34a5d36003d19"
EXPECTED_SHA256="8d7da0bb11627c8cbe98f73f47ab5a92ec1576a7043f3976a0f107343c724a65"
URL="https://github.com/manaflow-ai/ghostty/releases/download/xcframework-${GHOSTTY_SHA}/GhosttyKit.xcframework.tar.gz"

TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/emacs-host-ghosttykit.XXXXXX")"
trap 'rm -rf "$TMP_DIR"' EXIT

ARCHIVE="$TMP_DIR/GhosttyKit.xcframework.tar.gz"
EXTRACT="$TMP_DIR/extract"
mkdir -p "$EXTRACT"

curl --fail --show-error --location \
  --connect-timeout 10 \
  --max-time 300 \
  --retry 3 \
  --retry-delay 2 \
  --retry-all-errors \
  -o "$ARCHIVE" \
  "$URL"

ACTUAL_SHA256="$(shasum -a 256 "$ARCHIVE" | awk '{print $1}')"
if [[ "$ACTUAL_SHA256" != "$EXPECTED_SHA256" ]]; then
  echo "GhosttyKit checksum mismatch" >&2
  echo "expected: $EXPECTED_SHA256" >&2
  echo "actual:   $ACTUAL_SHA256" >&2
  exit 1
fi

tar --no-same-owner -xzf "$ARCHIVE" -C "$EXTRACT"
rm -rf "$ROOT/GhosttyKit.xcframework"
mv "$EXTRACT/GhosttyKit.xcframework" "$ROOT/GhosttyKit.xcframework"

MACOS_DIR="$ROOT/GhosttyKit.xcframework/macos-arm64_x86_64"
if [[ -f "$MACOS_DIR/ghostty-internal.a" ]]; then
  mv "$MACOS_DIR/ghostty-internal.a" "$MACOS_DIR/libghostty-internal.a"
  plutil -replace AvailableLibraries.1.BinaryPath -string libghostty-internal.a "$ROOT/GhosttyKit.xcframework/Info.plist"
  plutil -replace AvailableLibraries.1.LibraryPath -string libghostty-internal.a "$ROOT/GhosttyKit.xcframework/Info.plist"
fi

echo "GhosttyKit.xcframework ready at $ROOT/GhosttyKit.xcframework"
