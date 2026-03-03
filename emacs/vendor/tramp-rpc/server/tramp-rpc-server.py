#!/usr/bin/env python3
"""TRAMP-RPC Python server (MessagePack framed over stdin/stdout)."""

import asyncio
import concurrent.futures
import errno
import fcntl
import grp
import os
import pty
import pwd
import select
import shutil
import signal
import socket
import stat
import struct
import subprocess
import sys
import termios
import threading
import time
from typing import Any, Dict, List, Optional, Tuple

import msgpack

VERSION = "0.6.0"
MAX_MESSAGE_SIZE = 100 * 1024 * 1024
WATCH_DEBOUNCE_SEC = 0.2
WATCH_POLL_SEC = 0.2

PARSE_ERROR = -32700
INVALID_REQUEST = -32600
METHOD_NOT_FOUND = -32601
INVALID_PARAMS = -32602
INTERNAL_ERROR = -32603
FILE_NOT_FOUND = -32001
PERMISSION_DENIED = -32002
IO_ERROR = -32003
PROCESS_ERROR = -32004

_stdout_lock = threading.Lock()

processes: Dict[int, Dict[str, Any]] = {}
next_process_pid = 1

pty_processes: Dict[int, Dict[str, Any]] = {}
next_pty_pid = 10000

state_lock = asyncio.Lock()

watch_lock = threading.Lock()
watched_paths: Dict[str, bool] = {}
watch_snapshots: Dict[str, Dict[str, Tuple[int, int]]] = {}
watch_pending_paths: set[str] = set()
watch_last_change_time: float = 0.0
watch_thread: Optional[threading.Thread] = None
watch_stop_event = threading.Event()


class RpcMethodError(Exception):
    def __init__(self, code: int, message: str, data: Any = None):
        super().__init__(message)
        self.code = code
        self.message = message
        self.data = data


# ============================================================================
# Protocol helpers
# ============================================================================


def _pack_message(obj: Dict[str, Any]) -> bytes:
    payload = msgpack.packb(obj, use_bin_type=True)
    return struct.pack(">I", len(payload)) + payload


def send_message(obj: Dict[str, Any]) -> None:
    data = _pack_message(obj)
    with _stdout_lock:
        sys.stdout.buffer.write(data)
        sys.stdout.buffer.flush()


def success_response(request_id: Any, result: Any) -> Dict[str, Any]:
    return {"version": "2.0", "id": request_id, "result": result}


def error_response(request_id: Any, code: int, message: str, data: Any = None) -> Dict[str, Any]:
    err: Dict[str, Any] = {"code": code, "message": message}
    if data is not None:
        err["data"] = data
    return {"version": "2.0", "id": request_id, "error": err}


def notification(method: str, params: Dict[str, Any]) -> Dict[str, Any]:
    return {"version": "2.0", "method": method, "params": params}


def _dget(d: Dict[Any, Any], key: str, default: Any = None) -> Any:
    if not isinstance(d, dict):
        return default
    if key in d:
        return d[key]
    bkey = key.encode("utf-8")
    if bkey in d:
        return d[bkey]
    return default


def _to_text(value: Any, field: str = "value") -> str:
    if isinstance(value, bytes):
        return value.decode("utf-8", "surrogateescape")
    if isinstance(value, str):
        return value
    raise ValueError(f"{field} must be string/bytes")


def _to_path(value: Any, field: str = "path") -> str:
    return os.path.expanduser(_to_text(value, field))


def _to_bytes(value: Any, field: str = "data") -> bytes:
    if isinstance(value, bytes):
        return value
    if isinstance(value, str):
        return value.encode("utf-8")
    raise ValueError(f"{field} must be bytes/string")


def _map_os_error(exc: OSError, path: str) -> Tuple[int, str, Optional[Dict[str, Any]]]:
    if exc.errno == errno.ENOENT:
        return FILE_NOT_FOUND, f"File not found: {path}", None
    if exc.errno in (errno.EPERM, errno.EACCES):
        return PERMISSION_DENIED, f"Permission denied: {path}", None
    data = {"os_errno": exc.errno} if exc.errno is not None else None
    return IO_ERROR, str(exc), data


def _get_file_type(mode: int) -> str:
    if stat.S_ISREG(mode):
        return "file"
    if stat.S_ISDIR(mode):
        return "directory"
    if stat.S_ISLNK(mode):
        return "symlink"
    if stat.S_ISCHR(mode):
        return "chardevice"
    if stat.S_ISBLK(mode):
        return "blockdevice"
    if stat.S_ISFIFO(mode):
        return "fifo"
    if stat.S_ISSOCK(mode):
        return "socket"
    return "unknown"


def _user_name(uid: int) -> Optional[str]:
    try:
        return pwd.getpwuid(uid).pw_name
    except Exception:
        return None


def _group_name(gid: int) -> Optional[str]:
    try:
        return grp.getgrgid(gid).gr_name
    except Exception:
        return None


def _format_file_attributes(st: os.stat_result, path: str, is_lstat: bool = False) -> Dict[str, Any]:
    attrs: Dict[str, Any] = {
        "type": _get_file_type(st.st_mode),
        "nlinks": st.st_nlink,
        "uid": st.st_uid,
        "gid": st.st_gid,
        "atime": int(st.st_atime),
        "mtime": int(st.st_mtime),
        "ctime": int(st.st_ctime),
        "size": st.st_size,
        "mode": st.st_mode,
        "inode": st.st_ino,
        "dev": st.st_dev,
    }
    uname = _user_name(st.st_uid)
    if uname is not None:
        attrs["uname"] = uname
    gname = _group_name(st.st_gid)
    if gname is not None:
        attrs["gname"] = gname
    if attrs["type"] == "symlink":
        try:
            attrs["link_target"] = os.readlink(path)
        except OSError:
            pass
    return attrs


async def _run_in_thread(func, *args, **kwargs):
    loop = asyncio.get_running_loop()
    return await loop.run_in_executor(None, lambda: func(*args, **kwargs))


# ============================================================================
# Watch support (polling-based, debounced)
# ============================================================================


def _build_snapshot(path: str, recursive: bool) -> Dict[str, Tuple[int, int]]:
    snapshot: Dict[str, Tuple[int, int]] = {}
    try:
        st = os.lstat(path)
        snapshot[path] = (st.st_mtime_ns, st.st_size)
    except OSError:
        return snapshot

    def add_entry(p: str) -> None:
        try:
            s = os.lstat(p)
            snapshot[p] = (s.st_mtime_ns, s.st_size)
        except OSError:
            return

    if recursive:
        for root, dirs, files in os.walk(path, topdown=True, followlinks=False):
            for name in dirs:
                add_entry(os.path.join(root, name))
            for name in files:
                add_entry(os.path.join(root, name))
    else:
        try:
            with os.scandir(path) as it:
                for e in it:
                    add_entry(e.path)
        except OSError:
            pass
    return snapshot


def _watch_loop() -> None:
    global watch_last_change_time
    while not watch_stop_event.is_set():
        changed: set[str] = set()
        with watch_lock:
            items = list(watched_paths.items())
        for watched, recursive in items:
            old = watch_snapshots.get(watched, {})
            new = _build_snapshot(watched, recursive)
            all_keys = set(old.keys()) | set(new.keys())
            for key in all_keys:
                if old.get(key) != new.get(key):
                    changed.add(key)
            watch_snapshots[watched] = new

        now = time.time()
        if changed:
            with watch_lock:
                watch_pending_paths.update(changed)
                watch_last_change_time = now

        should_emit = False
        emit_paths: List[str] = []
        with watch_lock:
            if watch_pending_paths and (now - watch_last_change_time) >= WATCH_DEBOUNCE_SEC:
                emit_paths = sorted(watch_pending_paths)
                watch_pending_paths.clear()
                should_emit = True

        if should_emit:
            try:
                send_message(notification("fs.changed", {"paths": emit_paths}))
            except Exception:
                break

        watch_stop_event.wait(WATCH_POLL_SEC)


def _ensure_watch_thread() -> None:
    global watch_thread
    if watch_thread and watch_thread.is_alive():
        return
    watch_stop_event.clear()
    watch_thread = threading.Thread(target=_watch_loop, name="tramp-rpc-watch", daemon=True)
    watch_thread.start()


def _stop_watch_thread() -> None:
    watch_stop_event.set()
    if watch_thread and watch_thread.is_alive():
        watch_thread.join(timeout=0.5)


# ============================================================================
# File metadata methods
# ============================================================================


async def file_stat(params: Dict[str, Any]) -> Any:
    path = _to_path(_dget(params, "path"), "path")
    lstat = bool(_dget(params, "lstat", False))

    def _do_stat():
        try:
            st = os.lstat(path) if lstat else os.stat(path)
        except FileNotFoundError:
            return None
        return _format_file_attributes(st, path, is_lstat=lstat)

    return await _run_in_thread(_do_stat)


async def file_stat_batch(params: Dict[str, Any]) -> List[Any]:
    raw_paths = _dget(params, "paths", [])
    lstat = bool(_dget(params, "lstat", False))

    async def _one(raw_path: Any) -> Any:
        path = _to_path(raw_path, "path")
        try:
            st = await _run_in_thread(os.lstat if lstat else os.stat, path)
            return _format_file_attributes(st, path, is_lstat=lstat)
        except FileNotFoundError:
            return None
        except OSError as exc:
            return {"error": str(exc)}

    return await asyncio.gather(*[_one(p) for p in raw_paths])


async def file_exists(params: Dict[str, Any]) -> bool:
    path = _to_path(_dget(params, "path"), "path")
    return await _run_in_thread(os.path.exists, path)


async def file_readable(params: Dict[str, Any]) -> bool:
    path = _to_path(_dget(params, "path"), "path")
    return await _run_in_thread(os.access, path, os.R_OK)


async def file_writable(params: Dict[str, Any]) -> bool:
    path = _to_path(_dget(params, "path"), "path")
    return await _run_in_thread(os.access, path, os.W_OK)


async def file_executable(params: Dict[str, Any]) -> bool:
    path = _to_path(_dget(params, "path"), "path")
    return await _run_in_thread(os.access, path, os.X_OK)


async def file_truename(params: Dict[str, Any]) -> bytes:
    path = _to_path(_dget(params, "path"), "path")

    def _do_truename() -> bytes:
        real = os.path.realpath(path)
        os.stat(real)
        return os.fsencode(real)

    return await _run_in_thread(_do_truename)


async def file_newer_than(params: Dict[str, Any]) -> bool:
    a = _to_path(_dget(params, "a"), "a")
    b = _to_path(_dget(params, "b"), "b")
    return await _run_in_thread(os.path.getmtime, a) > await _run_in_thread(os.path.getmtime, b)


# ============================================================================
# File I/O methods
# ============================================================================


async def file_read(params: Dict[str, Any]) -> Dict[str, Any]:
    path = _to_path(_dget(params, "path"), "path")
    offset = _dget(params, "offset")
    length = _dget(params, "length")

    def _do_read() -> bytes:
        with open(path, "rb") as fh:
            if offset is not None:
                fh.seek(int(offset))
            return fh.read() if length is None else fh.read(int(length))

    content = await _run_in_thread(_do_read)
    return {"content": content, "size": len(content)}


async def file_write(params: Dict[str, Any]) -> Dict[str, Any]:
    path = _to_path(_dget(params, "path"), "path")
    content = _to_bytes(_dget(params, "content"), "content")
    mode = _dget(params, "mode")
    append = bool(_dget(params, "append", False))
    offset = _dget(params, "offset")

    def _do_write() -> int:
        if append:
            open_mode = "ab"
        elif offset is not None:
            open_mode = "r+b"
        else:
            open_mode = "wb"

        if open_mode == "r+b" and not os.path.exists(path):
            open(path, "wb").close()

        with open(path, open_mode) as fh:
            if offset is not None:
                fh.seek(int(offset))
            fh.write(content)

        if mode is not None:
            os.chmod(path, int(mode))
        return len(content)

    written = await _run_in_thread(_do_write)
    return {"written": written}


async def file_copy(params: Dict[str, Any]) -> Dict[str, Any]:
    src = _to_path(_dget(params, "src"), "src")
    dest = _to_path(_dget(params, "dest"), "dest")
    preserve = bool(_dget(params, "preserve", False))

    def _size_of_tree(root: str) -> int:
        total = 0
        for current_root, _, files in os.walk(root):
            for name in files:
                try:
                    total += os.path.getsize(os.path.join(current_root, name))
                except OSError:
                    pass
        return total

    def _do_copy() -> int:
        final_dest = dest
        if os.path.isdir(final_dest):
            final_dest = os.path.join(final_dest, os.path.basename(src))

        if os.path.isdir(src) and not os.path.islink(src):
            copy_fn = shutil.copy2 if preserve else shutil.copy
            shutil.copytree(src, final_dest, symlinks=True, dirs_exist_ok=True, copy_function=copy_fn)
            return _size_of_tree(final_dest)

        if preserve:
            shutil.copy2(src, final_dest)
        else:
            shutil.copy(src, final_dest)
        return os.path.getsize(final_dest)

    copied = await _run_in_thread(_do_copy)
    return {"copied": copied}


async def file_rename(params: Dict[str, Any]) -> bool:
    src = _to_path(_dget(params, "src"), "src")
    dest = _to_path(_dget(params, "dest"), "dest")
    overwrite = bool(_dget(params, "overwrite", False))

    def _do_rename() -> bool:
        if (not overwrite) and os.path.exists(dest):
            raise OSError(errno.EEXIST, f"Destination already exists: {dest}")
        if overwrite:
            os.replace(src, dest)
        else:
            os.rename(src, dest)
        return True

    return await _run_in_thread(_do_rename)


async def file_delete(params: Dict[str, Any]) -> bool:
    path = _to_path(_dget(params, "path"), "path")
    force = bool(_dget(params, "force", False))

    def _do_delete() -> bool:
        try:
            os.remove(path)
            return True
        except FileNotFoundError:
            if force:
                return False
            raise

    return await _run_in_thread(_do_delete)


async def file_set_modes(params: Dict[str, Any]) -> bool:
    path = _to_path(_dget(params, "path"), "path")
    mode = _dget(params, "mode")
    if mode is None:
        raise ValueError("mode is required")
    await _run_in_thread(os.chmod, path, int(mode))
    return True


async def file_set_times(params: Dict[str, Any]) -> bool:
    path = _to_path(_dget(params, "path"), "path")
    mtime = _dget(params, "mtime")
    if mtime is None:
        raise ValueError("mtime is required")
    atime = _dget(params, "atime", mtime)
    await _run_in_thread(os.utime, path, (float(atime), float(mtime)))
    return True


async def file_make_symlink(params: Dict[str, Any]) -> bool:
    target = _to_path(_dget(params, "target"), "target")
    link_path = _to_path(_dget(params, "link_path"), "link_path")
    await _run_in_thread(os.symlink, target, link_path)
    return True


async def file_make_hardlink(params: Dict[str, Any]) -> bool:
    src = _to_path(_dget(params, "src"), "src")
    dest = _to_path(_dget(params, "dest"), "dest")
    await _run_in_thread(os.link, src, dest)
    return True


async def file_chown(params: Dict[str, Any]) -> bool:
    path = _to_path(_dget(params, "path"), "path")
    uid = int(_dget(params, "uid", -1))
    gid = int(_dget(params, "gid", -1))
    await _run_in_thread(os.chown, path, uid, gid)
    return True


# ============================================================================
# Directory methods
# ============================================================================


async def dir_list(params: Dict[str, Any]) -> List[Dict[str, Any]]:
    path = _to_path(_dget(params, "path"), "path")
    include_attrs = bool(_dget(params, "include_attrs", False))
    include_hidden = bool(_dget(params, "include_hidden", True))

    def _entry_type(entry: os.DirEntry) -> str:
        try:
            if entry.is_symlink():
                return "symlink"
            if entry.is_dir(follow_symlinks=False):
                return "directory"
            if entry.is_file(follow_symlinks=False):
                return "file"
            st = entry.stat(follow_symlinks=False)
            return _get_file_type(st.st_mode)
        except OSError:
            return "unknown"

    def _do_list() -> List[Dict[str, Any]]:
        entries: List[Dict[str, Any]] = []

        if include_hidden:
            try:
                st_cur = os.stat(path)
                cur = {"name": b".", "type": "directory"}
                if include_attrs:
                    cur["attrs"] = _format_file_attributes(st_cur, path)
                entries.append(cur)
            except OSError:
                pass

            try:
                parent = os.path.dirname(path) or path
                st_par = os.stat(parent)
                par = {"name": b"..", "type": "directory"}
                if include_attrs:
                    par["attrs"] = _format_file_attributes(st_par, parent)
                entries.append(par)
            except OSError:
                pass

        with os.scandir(path) as it:
            for entry in it:
                name = entry.name
                if not include_hidden and name.startswith("."):
                    continue
                result: Dict[str, Any] = {
                    "name": os.fsencode(name),
                    "type": _entry_type(entry),
                }
                if include_attrs:
                    try:
                        st = entry.stat(follow_symlinks=False)
                        result["attrs"] = _format_file_attributes(st, entry.path, is_lstat=True)
                    except OSError:
                        pass
                entries.append(result)

        entries.sort(key=lambda e: e["name"])
        return entries

    return await _run_in_thread(_do_list)


async def dir_create(params: Dict[str, Any]) -> bool:
    path = _to_path(_dget(params, "path"), "path")
    parents = bool(_dget(params, "parents", False))
    mode = int(_dget(params, "mode", 0o755))

    def _do_create() -> bool:
        if parents:
            os.makedirs(path, mode=mode, exist_ok=True)
        else:
            os.mkdir(path, mode=mode)
        return True

    return await _run_in_thread(_do_create)


async def dir_remove(params: Dict[str, Any]) -> bool:
    path = _to_path(_dget(params, "path"), "path")
    recursive = bool(_dget(params, "recursive", False))

    def _do_remove() -> bool:
        if recursive:
            shutil.rmtree(path)
        else:
            os.rmdir(path)
        return True

    return await _run_in_thread(_do_remove)


async def dir_completions(params: Dict[str, Any]) -> List[str]:
    directory = _to_path(_dget(params, "directory"), "directory")
    prefix = _to_text(_dget(params, "prefix", ""), "prefix")

    def _do_completions() -> List[str]:
        comps: List[str] = []
        try:
            with os.scandir(directory) as it:
                for entry in it:
                    if entry.name.startswith(prefix):
                        suffix = "/" if entry.is_dir() else ""
                        comps.append(entry.name + suffix)
        except OSError:
            pass
        comps.sort()
        return comps

    return await _run_in_thread(_do_completions)


# ============================================================================
# Process methods
# ============================================================================


def _normalize_env(raw_env: Any, clear_env: bool) -> Optional[Dict[str, str]]:
    base: Optional[Dict[str, str]]
    if clear_env:
        base = {}
    elif raw_env:
        base = dict(os.environ)
    else:
        base = None

    if raw_env is None:
        return base

    if isinstance(raw_env, dict):
        items = raw_env.items()
    else:
        items = raw_env

    if base is None:
        base = {}

    for key, value in items:
        base[_to_text(key, "env key")] = _to_text(value, "env value")
    return base


def _normalize_args(raw_args: Any) -> List[str]:
    if raw_args is None:
        return []
    return [_to_text(a, "arg") for a in raw_args]


async def process_run(params: Dict[str, Any]) -> Dict[str, Any]:
    cmd = _to_text(_dget(params, "cmd"), "cmd")
    args = _normalize_args(_dget(params, "args", []))
    cwd = _dget(params, "cwd")
    env = _normalize_env(_dget(params, "env"), bool(_dget(params, "clear_env", False)))
    stdin_data = _dget(params, "stdin")
    if stdin_data is not None:
        stdin_data = _to_bytes(stdin_data, "stdin")

    proc = await asyncio.create_subprocess_exec(
        cmd,
        *args,
        stdin=asyncio.subprocess.PIPE if stdin_data is not None else asyncio.subprocess.DEVNULL,
        stdout=asyncio.subprocess.PIPE,
        stderr=asyncio.subprocess.PIPE,
        cwd=_to_path(cwd, "cwd") if cwd is not None else None,
        env=env,
    )

    stdout, stderr = await proc.communicate(stdin_data)
    return {
        "exit_code": proc.returncode,
        "stdout": stdout,
        "stderr": stderr,
        "stdout_encoding": None,
        "stderr_encoding": None,
    }


async def process_start(params: Dict[str, Any]) -> Dict[str, Any]:
    global next_process_pid

    cmd = _to_text(_dget(params, "cmd"), "cmd")
    args = _normalize_args(_dget(params, "args", []))
    cwd = _dget(params, "cwd")
    env = _normalize_env(_dget(params, "env"), bool(_dget(params, "clear_env", False)))

    proc = await asyncio.create_subprocess_exec(
        cmd,
        *args,
        stdin=asyncio.subprocess.PIPE,
        stdout=asyncio.subprocess.PIPE,
        stderr=asyncio.subprocess.PIPE,
        cwd=_to_path(cwd, "cwd") if cwd is not None else None,
        env=env,
    )

    async with state_lock:
        pid = next_process_pid
        next_process_pid += 1
        processes[pid] = {"process": proc, "cmd": cmd}

    return {"pid": pid}


async def process_write(params: Dict[str, Any]) -> Dict[str, Any]:
    pid = int(_dget(params, "pid"))
    data = _to_bytes(_dget(params, "data"), "data")

    async with state_lock:
        info = processes.get(pid)
    if info is None:
        raise RpcMethodError(PROCESS_ERROR, f"Process not found: {pid}")

    proc = info["process"]
    if proc.stdin is None:
        raise RpcMethodError(PROCESS_ERROR, f"Process has no stdin: {pid}")

    proc.stdin.write(data)
    await proc.stdin.drain()
    return {"written": len(data)}


async def process_read(params: Dict[str, Any]) -> Dict[str, Any]:
    pid = int(_dget(params, "pid"))
    max_bytes = int(_dget(params, "max_bytes", 65536))
    timeout_ms = int(_dget(params, "timeout_ms", 0))
    timeout = timeout_ms / 1000.0 if timeout_ms > 0 else 0.001

    async with state_lock:
        info = processes.get(pid)
    if info is None:
        raise RpcMethodError(PROCESS_ERROR, f"Process not found: {pid}")

    proc = info["process"]

    stdout = b""
    stderr = b""

    if proc.stdout is not None:
        try:
            stdout = await asyncio.wait_for(proc.stdout.read(max_bytes), timeout=timeout)
        except asyncio.TimeoutError:
            pass

    if proc.stderr is not None:
        try:
            stderr = await asyncio.wait_for(proc.stderr.read(max_bytes), timeout=timeout)
        except asyncio.TimeoutError:
            pass

    result: Dict[str, Any] = {
        "stdout": stdout if stdout else None,
        "stderr": stderr if stderr else None,
        "stdout_encoding": None,
        "stderr_encoding": None,
        "exited": proc.returncode is not None,
        "exit_code": proc.returncode,
    }
    return result


async def process_close_stdin(params: Dict[str, Any]) -> bool:
    pid = int(_dget(params, "pid"))

    async with state_lock:
        info = processes.get(pid)
    if info is None:
        raise RpcMethodError(PROCESS_ERROR, f"Process not found: {pid}")

    proc = info["process"]
    if proc.stdin is not None:
        proc.stdin.close()
        try:
            await proc.stdin.wait_closed()
        except Exception:
            pass
    return True


async def process_kill(params: Dict[str, Any]) -> bool:
    pid = int(_dget(params, "pid"))
    sig = int(_dget(params, "signal", signal.SIGTERM))

    async with state_lock:
        info = processes.get(pid)
    if info is None:
        raise RpcMethodError(PROCESS_ERROR, f"Process not found: {pid}")

    proc = info["process"]
    if proc.returncode is None and proc.pid is not None:
        os.kill(proc.pid, sig)

    if sig == signal.SIGKILL:
        async with state_lock:
            processes.pop(pid, None)
    return True


async def process_list(params: Dict[str, Any]) -> List[Dict[str, Any]]:
    del params
    result: List[Dict[str, Any]] = []
    async with state_lock:
        for pid, info in processes.items():
            proc = info["process"]
            result.append(
                {
                    "pid": pid,
                    "os_pid": proc.pid,
                    "cmd": info["cmd"],
                    "exited": proc.returncode is not None,
                    "exit_code": proc.returncode,
                }
            )
    return result


# ============================================================================
# PTY process methods
# ============================================================================


def _set_window_size(fd: int, rows: int, cols: int) -> None:
    winsize = struct.pack("HHHH", rows, cols, 0, 0)
    fcntl.ioctl(fd, termios.TIOCSWINSZ, winsize)


def _set_nonblocking(fd: int) -> None:
    flags = fcntl.fcntl(fd, fcntl.F_GETFL)
    fcntl.fcntl(fd, fcntl.F_SETFL, flags | os.O_NONBLOCK)


async def process_start_pty(params: Dict[str, Any]) -> Dict[str, Any]:
    global next_pty_pid

    cmd = _to_text(_dget(params, "cmd"), "cmd")
    args = _normalize_args(_dget(params, "args", []))
    cwd = _dget(params, "cwd")
    env = _normalize_env(_dget(params, "env"), bool(_dget(params, "clear_env", False)))
    rows = int(_dget(params, "rows", 24))
    cols = int(_dget(params, "cols", 80))

    def _fork_pty():
        master_fd, slave_fd = pty.openpty()
        _set_window_size(master_fd, rows, cols)
        tty_name = os.ttyname(slave_fd)

        child_pid = os.fork()
        if child_pid == 0:
            os.close(master_fd)
            os.setsid()
            fcntl.ioctl(slave_fd, termios.TIOCSCTTY, 0)
            os.dup2(slave_fd, 0)
            os.dup2(slave_fd, 1)
            os.dup2(slave_fd, 2)
            if slave_fd > 2:
                os.close(slave_fd)

            if cwd is not None:
                os.chdir(_to_path(cwd, "cwd"))

            if env is not None:
                os.environ.clear()
                os.environ.update(env)

            os.execvp(cmd, [cmd, *args])
            os._exit(127)

        os.close(slave_fd)
        _set_nonblocking(master_fd)
        return master_fd, child_pid, tty_name

    master_fd, child_pid, tty_name = await _run_in_thread(_fork_pty)

    async with state_lock:
        pid = next_pty_pid
        next_pty_pid += 1
        pty_processes[pid] = {
            "master_fd": master_fd,
            "child_pid": child_pid,
            "cmd": cmd,
            "exit_status": None,
        }

    return {"pid": pid, "os_pid": child_pid, "tty_name": tty_name}


async def process_read_pty(params: Dict[str, Any]) -> Dict[str, Any]:
    pid = int(_dget(params, "pid"))
    max_bytes = int(_dget(params, "max_bytes", 65536))
    timeout_ms = int(_dget(params, "timeout_ms", 0))

    async with state_lock:
        info = pty_processes.get(pid)
    if info is None:
        return {
            "output": None,
            "output_encoding": None,
            "exited": True,
            "exit_code": None,
        }

    master_fd = info["master_fd"]
    child_pid = info["child_pid"]

    def _do_read() -> Tuple[bytes, bool]:
        timeout = timeout_ms / 1000.0 if timeout_ms > 0 else 0.001
        ready, _, _ = select.select([master_fd], [], [], timeout)
        if not ready:
            return b"", False
        try:
            return os.read(master_fd, max_bytes), False
        except OSError as exc:
            if exc.errno in (errno.EAGAIN, errno.EWOULDBLOCK):
                return b"", False
            if exc.errno == errno.EIO:
                return b"", True
            raise

    output, maybe_exited = await _run_in_thread(_do_read)

    exited = False
    exit_code = None

    async with state_lock:
        current = pty_processes.get(pid)
        if current is not None:
            if current["exit_status"] is not None:
                exited = True
                exit_code = current["exit_status"]
            else:
                try:
                    wpid, status = os.waitpid(child_pid, os.WNOHANG)
                    if wpid != 0:
                        if os.WIFEXITED(status):
                            exit_code = os.WEXITSTATUS(status)
                        elif os.WIFSIGNALED(status):
                            exit_code = 128 + os.WTERMSIG(status)
                        else:
                            exit_code = -1
                        current["exit_status"] = exit_code
                        exited = True
                except ChildProcessError:
                    exited = True

    if maybe_exited and not exited:
        async with state_lock:
            current = pty_processes.get(pid)
            if current is not None:
                try:
                    wpid, status = os.waitpid(current["child_pid"], os.WNOHANG)
                    if wpid != 0:
                        if os.WIFEXITED(status):
                            exit_code = os.WEXITSTATUS(status)
                        elif os.WIFSIGNALED(status):
                            exit_code = 128 + os.WTERMSIG(status)
                        else:
                            exit_code = -1
                        current["exit_status"] = exit_code
                        exited = True
                except ChildProcessError:
                    exited = True

    return {
        "output": output if output else None,
        "output_encoding": None,
        "exited": exited,
        "exit_code": exit_code,
    }


async def process_write_pty(params: Dict[str, Any]) -> Dict[str, Any]:
    pid = int(_dget(params, "pid"))
    data = _to_bytes(_dget(params, "data"), "data")

    async with state_lock:
        info = pty_processes.get(pid)
    if info is None:
        raise RpcMethodError(PROCESS_ERROR, f"PTY process not found: {pid}")

    written = await _run_in_thread(os.write, info["master_fd"], data)
    return {"written": written}


async def process_resize_pty(params: Dict[str, Any]) -> bool:
    pid = int(_dget(params, "pid"))
    rows = int(_dget(params, "rows"))
    cols = int(_dget(params, "cols"))

    async with state_lock:
        info = pty_processes.get(pid)
    if info is None:
        raise RpcMethodError(PROCESS_ERROR, f"PTY process not found: {pid}")

    master_fd = info["master_fd"]
    child_pid = info["child_pid"]

    def _do_resize() -> None:
        _set_window_size(master_fd, rows, cols)
        try:
            fg = os.tcgetpgrp(master_fd)
            os.killpg(fg, signal.SIGWINCH)
        except Exception:
            try:
                os.killpg(os.getpgid(child_pid), signal.SIGWINCH)
            except Exception:
                pass

    await _run_in_thread(_do_resize)
    return True


async def process_kill_pty(params: Dict[str, Any]) -> bool:
    pid = int(_dget(params, "pid"))
    sig = int(_dget(params, "signal", signal.SIGTERM))

    async with state_lock:
        info = pty_processes.get(pid)
    if info is None:
        raise RpcMethodError(PROCESS_ERROR, f"PTY process not found: {pid}")

    try:
        os.kill(info["child_pid"], sig)
    except ProcessLookupError:
        pass

    if sig == signal.SIGKILL:
        async with state_lock:
            old = pty_processes.pop(pid, None)
        if old is not None:
            try:
                os.close(old["master_fd"])
            except OSError:
                pass

    return True


async def process_close_pty(params: Dict[str, Any]) -> bool:
    pid = int(_dget(params, "pid"))

    async with state_lock:
        info = pty_processes.pop(pid, None)
    if info is None:
        raise RpcMethodError(PROCESS_ERROR, f"PTY process not found: {pid}")

    try:
        os.kill(info["child_pid"], signal.SIGKILL)
    except Exception:
        pass
    try:
        os.close(info["master_fd"])
    except Exception:
        pass
    return True


async def process_list_pty(params: Dict[str, Any]) -> List[Dict[str, Any]]:
    del params
    result: List[Dict[str, Any]] = []
    async with state_lock:
        items = list(pty_processes.items())

    for pid, info in items:
        exited = False
        exit_code = info["exit_status"]
        if exit_code is None:
            try:
                wpid, status = os.waitpid(info["child_pid"], os.WNOHANG)
                if wpid != 0:
                    if os.WIFEXITED(status):
                        exit_code = os.WEXITSTATUS(status)
                    elif os.WIFSIGNALED(status):
                        exit_code = 128 + os.WTERMSIG(status)
                    else:
                        exit_code = -1
                    info["exit_status"] = exit_code
                    exited = True
            except ChildProcessError:
                exited = True
        else:
            exited = True

        result.append(
            {
                "pid": pid,
                "os_pid": info["child_pid"],
                "cmd": info["cmd"],
                "exited": exited,
                "exit_code": exit_code,
            }
        )

    return result


# ============================================================================
# System methods
# ============================================================================


async def system_info(params: Dict[str, Any]) -> Dict[str, Any]:
    del params
    uname = os.uname()
    return {
        "version": VERSION,
        "os": sys.platform,
        "arch": uname.machine,
        "hostname": socket.gethostname(),
        "uid": os.getuid(),
        "gid": os.getgid(),
        "home": os.environ.get("HOME"),
        "user": os.environ.get("USER"),
    }


async def system_getenv(params: Dict[str, Any]) -> Optional[str]:
    name = _to_text(_dget(params, "name"), "name")
    return os.environ.get(name)


async def system_expand_path(params: Dict[str, Any]) -> str:
    path = _to_text(_dget(params, "path"), "path")
    return os.path.expanduser(path)


async def system_statvfs(params: Dict[str, Any]) -> Dict[str, Any]:
    path = _to_path(_dget(params, "path"), "path")

    def _do_statvfs() -> Dict[str, Any]:
        st = os.statvfs(path)
        block_size = st.f_frsize
        return {
            "total": st.f_blocks * block_size,
            "free": st.f_bfree * block_size,
            "available": st.f_bavail * block_size,
            "block_size": block_size,
        }

    return await _run_in_thread(_do_statvfs)


async def system_groups(params: Dict[str, Any]) -> List[Dict[str, Any]]:
    del params

    def _do_groups() -> List[Dict[str, Any]]:
        result: List[Dict[str, Any]] = []
        for gid in os.getgroups():
            result.append({"gid": gid, "name": _group_name(gid)})
        return result

    return await _run_in_thread(_do_groups)


# ============================================================================
# Magit/watch parity methods
# ============================================================================


def _run_parallel_command(entry: Dict[str, Any]) -> Tuple[str, Dict[str, Any]]:
    key = _to_text(_dget(entry, "key"), "key")
    cmd = _to_text(_dget(entry, "cmd"), "cmd")
    args = _normalize_args(_dget(entry, "args", []))
    cwd_raw = _dget(entry, "cwd")
    cwd = _to_path(cwd_raw, "cwd") if cwd_raw is not None else None

    try:
        proc = subprocess.run(
            [cmd, *args],
            cwd=cwd,
            capture_output=True,
            check=False,
        )
        return key, {
            "exit_code": proc.returncode,
            "stdout": proc.stdout,
            "stderr": proc.stderr,
        }
    except Exception as exc:
        return key, {
            "exit_code": -1,
            "stdout": b"",
            "stderr": str(exc).encode("utf-8", "replace"),
        }


async def commands_run_parallel(params: Dict[str, Any]) -> Dict[str, Any]:
    commands = _dget(params, "commands", [])
    if not isinstance(commands, list):
        raise ValueError("commands must be a list")
    if len(commands) > 256:
        raise ValueError("Too many commands (max 256)")

    def _do_run() -> Dict[str, Any]:
        if not commands:
            return {}
        max_workers = min(32, len(commands))
        output: Dict[str, Any] = {}
        with concurrent.futures.ThreadPoolExecutor(max_workers=max_workers) as pool:
            futures = [pool.submit(_run_parallel_command, c) for c in commands]
            for future in futures:
                key, result = future.result()
                output[key] = result
        return output

    return await _run_in_thread(_do_run)


async def ancestors_scan(params: Dict[str, Any]) -> Dict[str, Any]:
    directory = _to_path(_dget(params, "directory"), "directory")
    markers_raw = _dget(params, "markers", [])
    max_depth = int(_dget(params, "max_depth", 10))

    markers = [_to_text(m, "marker") for m in markers_raw]

    def _do_scan() -> Dict[str, Any]:
        if not os.path.exists(directory):
            raise FileNotFoundError(directory)

        results: Dict[str, Optional[str]] = {m: None for m in markers}
        current = os.path.abspath(directory)
        depth = 0

        while depth < max_depth:
            for marker in markers:
                if results[marker] is None:
                    marker_path = os.path.join(current, marker)
                    if os.path.exists(marker_path):
                        results[marker] = current
            if all(v is not None for v in results.values()):
                break

            parent = os.path.dirname(current)
            if parent == current:
                break
            current = parent
            depth += 1

        return results

    return await _run_in_thread(_do_scan)


async def watch_add(params: Dict[str, Any]) -> bool:
    path = os.path.realpath(_to_path(_dget(params, "path"), "path"))
    recursive = bool(_dget(params, "recursive", False))

    if not os.path.exists(path):
        raise FileNotFoundError(path)

    with watch_lock:
        watched_paths[path] = recursive
        watch_snapshots[path] = _build_snapshot(path, recursive)

    _ensure_watch_thread()
    return True


async def watch_remove(params: Dict[str, Any]) -> bool:
    path = os.path.realpath(_to_path(_dget(params, "path"), "path"))

    removed = False
    with watch_lock:
        if path in watched_paths:
            removed = True
            watched_paths.pop(path, None)
            watch_snapshots.pop(path, None)

    return removed


async def watch_list(params: Dict[str, Any]) -> List[Dict[str, Any]]:
    del params
    with watch_lock:
        return [{"path": p, "recursive": r} for p, r in sorted(watched_paths.items())]


# ============================================================================
# Batch + dispatch
# ============================================================================


async def batch_execute(params: Dict[str, Any]) -> Dict[str, Any]:
    requests = _dget(params, "requests", [])

    async def _one(request: Dict[str, Any]) -> Dict[str, Any]:
        method = _to_text(_dget(request, "method"), "method")
        req_params = _dget(request, "params", {}) or {}
        handler = METHOD_DISPATCH.get(method)
        if handler is None:
            return {"error": {"code": METHOD_NOT_FOUND, "message": f"Method not found: {method}"}}
        try:
            result = await handler(req_params)
            return {"result": result}
        except RpcMethodError as exc:
            e: Dict[str, Any] = {"code": exc.code, "message": exc.message}
            if exc.data is not None:
                e["data"] = exc.data
            return {"error": e}
        except ValueError as exc:
            return {"error": {"code": INVALID_PARAMS, "message": str(exc)}}
        except OSError as exc:
            code, msg, data = _map_os_error(exc, _to_text(_dget(req_params, "path", "")))
            e = {"code": code, "message": msg}
            if data is not None:
                e["data"] = data
            return {"error": e}
        except Exception as exc:
            return {"error": {"code": INTERNAL_ERROR, "message": str(exc)}}

    return {"results": await asyncio.gather(*[_one(r) for r in requests])}


METHOD_DISPATCH = {
    "file.stat": file_stat,
    "file.stat_batch": file_stat_batch,
    "file.exists": file_exists,
    "file.readable": file_readable,
    "file.writable": file_writable,
    "file.executable": file_executable,
    "file.truename": file_truename,
    "file.newer_than": file_newer_than,
    "file.read": file_read,
    "file.write": file_write,
    "file.copy": file_copy,
    "file.rename": file_rename,
    "file.delete": file_delete,
    "file.set_modes": file_set_modes,
    "file.set_times": file_set_times,
    "file.make_symlink": file_make_symlink,
    "file.make_hardlink": file_make_hardlink,
    "file.chown": file_chown,
    "dir.list": dir_list,
    "dir.create": dir_create,
    "dir.remove": dir_remove,
    "dir.completions": dir_completions,
    "process.run": process_run,
    "process.start": process_start,
    "process.write": process_write,
    "process.read": process_read,
    "process.close_stdin": process_close_stdin,
    "process.kill": process_kill,
    "process.list": process_list,
    "process.start_pty": process_start_pty,
    "process.read_pty": process_read_pty,
    "process.write_pty": process_write_pty,
    "process.resize_pty": process_resize_pty,
    "process.kill_pty": process_kill_pty,
    "process.close_pty": process_close_pty,
    "process.list_pty": process_list_pty,
    "system.info": system_info,
    "system.getenv": system_getenv,
    "system.expand_path": system_expand_path,
    "system.statvfs": system_statvfs,
    "system.groups": system_groups,
    "commands.run_parallel": commands_run_parallel,
    "ancestors.scan": ancestors_scan,
    "watch.add": watch_add,
    "watch.remove": watch_remove,
    "watch.list": watch_list,
    "batch": batch_execute,
}


async def process_request_dict(request: Dict[str, Any]) -> Dict[str, Any]:
    request_id = _dget(request, "id")
    version = _dget(request, "version")
    if version != "2.0":
        return error_response(request_id, INVALID_REQUEST, "Invalid RPC version")

    method_raw = _dget(request, "method")
    if method_raw is None:
        return error_response(request_id, INVALID_REQUEST, "Method is required")

    method = _to_text(method_raw, "method")
    params = _dget(request, "params", {}) or {}
    handler = METHOD_DISPATCH.get(method)
    if handler is None:
        return error_response(request_id, METHOD_NOT_FOUND, f"Method not found: {method}")

    try:
        result = await handler(params)
        return success_response(request_id, result)
    except RpcMethodError as exc:
        return error_response(request_id, exc.code, exc.message, exc.data)
    except ValueError as exc:
        return error_response(request_id, INVALID_PARAMS, str(exc))
    except OSError as exc:
        path_text = _to_text(_dget(params, "path", "")) if isinstance(params, dict) else ""
        code, message, data = _map_os_error(exc, path_text)
        return error_response(request_id, code, message, data)
    except Exception as exc:
        return error_response(request_id, INTERNAL_ERROR, str(exc))


# ============================================================================
# Main loop
# ============================================================================


def _read_exact(size: int) -> Optional[bytes]:
    data = bytearray()
    while len(data) < size:
        chunk = sys.stdin.buffer.read(size - len(data))
        if not chunk:
            return None
        data.extend(chunk)
    return bytes(data)


def main() -> None:
    loop = asyncio.new_event_loop()
    asyncio.set_event_loop(loop)

    try:
        while True:
            length_prefix = _read_exact(4)
            if length_prefix is None:
                break
            message_len = struct.unpack(">I", length_prefix)[0]
            if message_len > MAX_MESSAGE_SIZE:
                payload = _read_exact(message_len)
                if payload is None:
                    break
                continue

            payload = _read_exact(message_len)
            if payload is None:
                break

            try:
                request = msgpack.unpackb(payload, raw=False, strict_map_key=False)
            except Exception as exc:
                send_message(error_response(None, PARSE_ERROR, f"Parse error: {exc}"))
                continue

            response = loop.run_until_complete(process_request_dict(request))
            send_message(response)
    except BrokenPipeError:
        pass
    except KeyboardInterrupt:
        pass
    finally:
        _stop_watch_thread()
        pending = asyncio.all_tasks(loop)
        for task in pending:
            task.cancel()
        if pending:
            loop.run_until_complete(asyncio.gather(*pending, return_exceptions=True))
        loop.close()


if __name__ == "__main__":
    main()
