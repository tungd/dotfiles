# emacs-host

Native macOS prototype for hosting terminal Emacs in app-managed PTYs.

This is intentionally Emacs-only. It does not use tmux and it is not the
coding-agent remote-control app.

The minimum target is macOS 15, matching `td@100.84.248.34` on macOS 15.7.4.

## Dependencies

The renderer uses the cmux-pinned GhosttyKit/libghostty build. The binary
artifact is intentionally ignored because it is large.

```sh
bash scripts/fetch-ghosttykit.sh
```

## Run

```sh
swift run emacs-host
```

Environment overrides:

```sh
EMACS_HOST_EMACS=/Users/tung/.local/bin/emacs \
EMACS_HOST_EMACSCLIENT=/Users/tung/.local/bin/emacsclient \
EMACS_HOST_SERVER=emacs-host \
swift run emacs-host
```

## Package App Bundle

The raw SwiftPM executable is useful for development, but Dock/Finder identity,
the app icon, notification identity, and macOS privacy usage descriptions require
an `.app` bundle:

```sh
make app
```

The `app` target copies the icon from the current Emacs.app discovered through
`EMACS_HOST_EMACS` or `emacs`. Override it explicitly when needed:

```sh
EMACS_HOST_ICON=/Applications/MacPorts/Emacs.app/Contents/Resources/Emacs.icns \
make app
```

The output is `dist/Emacs Host.app`, ad-hoc signed by default. Set
`EMACS_HOST_CODESIGN_IDENTITY` to use a Developer ID or Apple Development
identity.

Install it into `~/Applications`:

```sh
make install
```

Use a different location when needed:

```sh
make install INSTALL_DIR=/Applications
```

## Status

The process model is implemented and the terminal surface uses GhosttyKit with
manual IO, so Emacs still runs in the app-owned PTY while Ghostty handles VT
parsing, font shaping, Metal rendering, mouse input, and clipboard sequences.
