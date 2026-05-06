# Emacs Host Spec

## Goal

Create a native macOS app whose primary UI is terminal Emacs, without the
keyboard conflicts and AppKit rendering cost of the standard GUI Emacs frame.

The app owns one Emacs daemon and opens one terminal frame per tab:

```text
EmacsHost.app
  Emacs daemon: emacs --daemon=<server-name>
  Tab 1: pty -> emacsclient -t -s <server-name>
  Tab 2: pty -> emacsclient -t -s <server-name>
```

## Non-Goals

- Running coding agents.
- Acting as a general-purpose terminal.
- Using tmux as a required session layer.
- Recreating macOS-native Emacs menus, file dialogs, or toolbar workflows.
- Building an OCaml-owned UI DSL before the native Emacs host is usable.

## Why No Tmux In The Core

Tmux is useful for durable shared shell sessions, but it is not required for an
Emacs-only host. The app already owns the PTY and can fan out terminal output to
native and web views directly. Keeping tmux out of the core avoids another layer
of terminal negotiation, key translation, OSC filtering, clipboard behavior, and
resize policy.

Tmux can remain an optional user command inside Emacs or inside a shell buffer,
but it is not part of the app's Emacs frame model.

## Tabs

Tabs are app-owned chrome in a thick title bar, closer to Warp than to macOS
document tabs. They are not Emacs `tab-bar-mode` tabs.

Each app tab is an Emacs TTY frame, not an Emacs buffer. Tabs share daemon state:
buffers, packages, projects, kill ring, and server-side variables. Tabs do not
share cursor position, window layout, minibuffer activity, or terminal size.

## Process Lifecycle

On launch:

1. Resolve `emacs` and `emacsclient` paths.
2. Check whether `emacsclient -s <server-name> -e t` succeeds.
3. If not, start `emacs --daemon=<server-name>`.
4. Open the first tab by spawning `emacsclient -t -s <server-name>` in a PTY.

On tab close, close the PTY and terminate only that `emacsclient` process. The
daemon keeps running until app quit. Later versions may make daemon shutdown a
preference.

## Keyboard

The app should keep its own command map intentionally small and should avoid
default menu shortcuts for tab/window operations. Emacs should own almost every
key chord. The renderer/input layer must support:

- `Ctrl` control characters.
- `Option` as Meta by emitting `ESC` prefix.
- `Command` pass-through policy, eventually via a configurable terminal keyboard
  protocol.
- Function keys, arrows, page keys, delete, home, end.
- IME text insertion.

## Renderer Boundary

The intended production renderer is a libghostty-backed terminal view. It owns
VT parsing, font shaping, Metal drawing, mouse reporting, focus reporting,
clipboard sequences, and terminal resize behavior.

The current prototype implements the app/process/PTY boundary with a GhosttyKit
renderer. It uses cmux's forked manual IO surface mode:

```text
PTY output -> ghostty_surface_process_output(surface, bytes)
input      <- ghostty io_write_cb <- ghostty_surface_key/text/mouse
```

This keeps the host-owned PTY model while moving terminal emulation, shaping,
font rendering, clipboard sequences, mouse reporting, focus reporting, and Metal
drawing into Ghostty.

cmux's libghostty integration uses this shape:

1. Keep Ghostty as a submodule.
2. Build or download `GhosttyKit.xcframework`.
3. Import `GhosttyKit` from Swift.
4. Create one global `ghostty_app_t` with `ghostty_app_new`.
5. Create one `ghostty_surface_t` per terminal view with
   `ghostty_surface_new`.
6. Forward AppKit focus, resize, keyboard, IME, mouse, clipboard, and display
   changes into the surface API.

The current prototype does not vendor cmux's full `SurfaceView`; it implements a
small AppKit `NSView` wrapper around one `ghostty_surface_t` per Emacs tab.

Ghostty's embedded surface API is not a Rust `vt100`-style `state_diff` API. The
newer `libghostty-vt` headers do expose render-state dirty tracking, including
global full/partial dirty state, per-row dirty flags, and row/cell iteration.
That is useful for a custom renderer or web mirror later, but the native app
currently consumes the higher-level Ghostty surface renderer.

## OSC Host Events

Emacs Lisp can send app events from terminal Emacs:

```elisp
(send-string-to-terminal "\e]777;emacs;tab-title;notes\a")
```

The host reserves OSC 777 with the namespace `emacs`:

```text
ESC ] 777 ; emacs ; <command> ; <payload> BEL
ESC ] 777 ; emacs ; <command> ; <payload> ESC \
```

Initial commands:

- `tab-title`: set the native tab title.
- `notify`: show a native notification later.
- `share-tab`: start a read-only web share later.

Payloads are delimiter-sensitive. Structured payloads should use base64url JSON.

## Web Sharing

Web sharing does not require tmux. The app-owned session is the source of truth:

```text
PTY output -> native terminal view
           -> optional browser viewers

input <- exactly one controller
```

Default web mode is read-only. A single browser client can take control; all
other clients remain viewers. The native app should use the same controller gate
when sharing is active.

## Prototype Scope

- SwiftPM AppKit executable.
- Emacs daemon manager.
- PTY spawn, resize, read, write, and termination.
- Native window with tabbed Emacs sessions.
- Minimal app command surface.
- OSC 777 parser in the session layer.
- GhosttyKit renderer using manual IO over the app-owned PTY.

## Future: OCaml UI DSL

An OCaml DSL for app UI/state is a possible later direction, but it should not
block the first usable native host. The practical shape would be OCaml owning a
reactive app model and emitting a declarative virtual UI tree, while Swift owns
the actual SwiftUI/AppKit rendering and sends UI events back into OCaml.

Directly constructing arbitrary SwiftUI views from OCaml is not the preferred
path because SwiftUI's public programming model depends on Swift generics,
opaque `some View` types, result builders, property wrappers, identity rules,
and `MainActor` behavior. Revisit this after the libghostty renderer, Warp-style
tabs, lifecycle, and sharing model are stable.
