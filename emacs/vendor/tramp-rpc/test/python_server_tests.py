#!/usr/bin/env python3
"""Integration tests for the TRAMP-RPC Python MessagePack server."""

import os
import struct
import subprocess
import tempfile
import time
import unittest

import msgpack


SERVER = "/Users/tung/Projects/dotfiles/emacs/vendor/tramp-rpc/server/tramp-rpc-server.py"


class RpcClient:
    def __init__(self):
        self.proc = subprocess.Popen(
            ["python3", SERVER],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        self.request_id = 0
        self.notifications = []

    def close(self):
        if self.proc.poll() is None:
            self.proc.terminate()
            try:
                self.proc.wait(timeout=2)
            except subprocess.TimeoutExpired:
                self.proc.kill()
        if self.proc.stdin:
            self.proc.stdin.close()
        if self.proc.stdout:
            self.proc.stdout.close()
        if self.proc.stderr:
            self.proc.stderr.close()

    def _read_exact(self, n):
        data = b""
        while len(data) < n:
            chunk = self.proc.stdout.read(n - len(data))
            if not chunk:
                raise RuntimeError("server closed stdout")
            data += chunk
        return data

    def read_message(self):
        length = struct.unpack(">I", self._read_exact(4))[0]
        payload = self._read_exact(length)
        return msgpack.unpackb(payload, raw=False)

    def send_request(self, method, params=None):
        self.request_id += 1
        req = {
            "version": "2.0",
            "id": self.request_id,
            "method": method,
            "params": params or {},
        }
        payload = msgpack.packb(req, use_bin_type=True)
        self.proc.stdin.write(struct.pack(">I", len(payload)))
        self.proc.stdin.write(payload)
        self.proc.stdin.flush()
        return self.request_id

    def call(self, method, params=None, timeout=5.0):
        req_id = self.send_request(method, params)
        deadline = time.time() + timeout
        while time.time() < deadline:
            msg = self.read_message()
            if "method" in msg and "id" not in msg:
                self.notifications.append(msg)
                continue
            if msg.get("id") == req_id:
                if "error" in msg:
                    raise RuntimeError(f"rpc error: {msg['error']}")
                return msg["result"]
        raise TimeoutError(f"timeout waiting for response id={req_id}")

    def wait_for_notification(self, method, timeout=5.0):
        deadline = time.time() + timeout
        while time.time() < deadline:
            for i, msg in enumerate(list(self.notifications)):
                if msg.get("method") == method:
                    self.notifications.pop(i)
                    return msg
            msg = self.read_message()
            if "method" in msg and "id" not in msg:
                if msg.get("method") == method:
                    return msg
                self.notifications.append(msg)
                continue
            # Unexpected response message, keep it for the caller.
            self.notifications.append(msg)
        raise TimeoutError(f"timeout waiting for notification method={method}")


class PythonServerTests(unittest.TestCase):
    def setUp(self):
        self.client = RpcClient()

    def tearDown(self):
        self.client.close()

    def test_protocol_system_info(self):
        result = self.client.call("system.info")
        self.assertEqual(result["version"], "0.6.0")
        self.assertIn("arch", result)
        self.assertIn("uid", result)

    def test_commands_run_parallel(self):
        result = self.client.call(
            "commands.run_parallel",
            {
                "commands": [
                    {
                        "key": "hello",
                        "cmd": "/bin/sh",
                        "args": ["-c", "printf hi"],
                    }
                ]
            },
        )
        self.assertIn("hello", result)
        self.assertEqual(result["hello"]["exit_code"], 0)
        self.assertEqual(result["hello"]["stdout"], b"hi")

    def test_ancestors_scan(self):
        with tempfile.TemporaryDirectory() as tmp:
            project = os.path.join(tmp, "proj")
            nested = os.path.join(project, "a", "b")
            os.makedirs(nested)
            git_marker = os.path.join(project, ".git")
            os.makedirs(git_marker)

            result = self.client.call(
                "ancestors.scan",
                {
                    "directory": nested,
                    "markers": [".git", ".editorconfig"],
                    "max_depth": 10,
                },
            )
            self.assertEqual(result[".git"], project)
            self.assertIsNone(result[".editorconfig"])

    def test_watch_notification(self):
        with tempfile.TemporaryDirectory() as tmp:
            self.client.call("watch.add", {"path": tmp, "recursive": False})
            target = os.path.join(tmp, "created.txt")
            with open(target, "w", encoding="utf-8") as fh:
                fh.write("x")

            notif = self.client.wait_for_notification("fs.changed", timeout=8.0)
            paths = notif["params"]["paths"]
            self.assertTrue(any(p.endswith("created.txt") for p in paths))

            removed = self.client.call("watch.remove", {"path": tmp})
            self.assertTrue(removed)


if __name__ == "__main__":
    unittest.main()
