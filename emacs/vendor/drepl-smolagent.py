# /// script
# requires-python = ">=3.10,<3.14"
# dependencies = [
#   "smolagents[litellm]>=1.24,<2",
# ]
# ///
"""Minimal smolagents dREPL kernel."""

from __future__ import annotations

import base64
from contextlib import redirect_stderr, redirect_stdout
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import traceback
from typing import Any


PROMPT = "smolagent> "
CONTROL_OUT = sys.stdout
DIRECTIVE_RE = re.compile(
    r"^%%(?P<kind>sh|shell|bash|zsh|agent|load)"
    r"(?:\s+--run-id\s+(?P<run_id>[A-Za-z0-9_.:-]+))?\s*$",
    re.IGNORECASE,
)
SLASH_COMMANDS = (
    "/help",
    "/agent",
    "/shell",
    "/cd",
    "/pwd",
    "/reset",
    "/context",
    "/model",
    "/env",
)
DEFAULT_AGENT_INSTRUCTIONS = (
    "The final_answer tool is an internal control tool used only to finish "
    "a run. Do not describe final_answer to the user as an available "
    "capability; when asked about tools, list only user-facing tools such "
    "as read_file, write_file, edit_file, and bash."
)


def flush() -> None:
    CONTROL_OUT.flush()


def write(text: str = "") -> None:
    CONTROL_OUT.write(text)
    flush()


def writeln(text: str = "") -> None:
    CONTROL_OUT.write(text + "\n")
    flush()


def send_msg(**data: Any) -> None:
    CONTROL_OUT.write("\033]5161;")
    json.dump(data, CONTROL_OUT, separators=(",", ":"))
    CONTROL_OUT.write("\033\\")
    flush()


def send_mime(mime_type: str, data: str | bytes, **header: Any) -> None:
    if isinstance(data, str):
        payload = data.encode()
    else:
        payload = data
    header = {"type": mime_type, **{k: v for k, v in header.items() if v is not None}}
    CONTROL_OUT.write("\033]5151;")
    json.dump(header, CONTROL_OUT, separators=(",", ":"))
    CONTROL_OUT.write("\n")
    CONTROL_OUT.write(base64.b64encode(payload).decode())
    CONTROL_OUT.write("\033\\")
    flush()


def read_msg() -> dict[str, Any]:
    send_msg(op="status", status="ready")
    fragments: list[bytes] = []
    while True:
        line = sys.stdin.buffer.readline()
        if not line:
            raise EOFError
        if line.startswith(b"\033+"):
            fragments.append(line[2:].rstrip(b"\r\n"))
            continue
        if line.startswith(b"\033="):
            fragments.append(line[2:].rstrip(b"\r\n"))
            return json.loads(b"".join(fragments))
        raise RuntimeError(f"invalid dREPL input: {line!r}")


def split_directive(code: str) -> tuple[str | None, str | None, str]:
    lines = code.splitlines()
    while lines and not lines[0].strip():
        lines.pop(0)
    if not lines:
        return None, None, ""

    match = DIRECTIVE_RE.match(lines[0].strip())
    if not match:
        return None, None, code.strip()

    return (
        match.group("kind").lower(),
        match.group("run_id"),
        "\n".join(lines[1:]).strip(),
    )


def shell_command_from_code(code: str) -> str | None:
    stripped = code.strip()
    if stripped.startswith("/shell "):
        return stripped[len("/shell ") :].strip()
    if stripped.startswith("!"):
        return stripped[1:].strip()

    nonempty = [line for line in code.splitlines() if line.strip()]
    if nonempty and all(line.lstrip().startswith("$ ") for line in nonempty):
        return "\n".join(line.lstrip()[2:] for line in nonempty)

    return None


def agent_prompt_from_code(code: str) -> str:
    stripped = code.strip()
    if stripped.startswith("/agent "):
        return stripped[len("/agent ") :].strip()
    return stripped


def env_int(name: str, default: int) -> int:
    value = os.getenv(name)
    if not value:
        return default
    try:
        return int(value)
    except ValueError:
        return default


def env_float(name: str, default: float) -> float:
    value = os.getenv(name)
    if not value:
        return default
    try:
        return float(value)
    except ValueError:
        return default


def env_bool(name: str, default: bool) -> bool:
    value = os.getenv(name)
    if value is None:
        return default
    return value.strip().lower() not in {"", "0", "false", "no", "off"}


def tool_max_chars() -> int:
    return env_int("SMOLAGENT_TOOL_MAX_CHARS", 60000)


def bash_timeout() -> int:
    return env_int("SMOLAGENT_BASH_TIMEOUT", 60)


def shell_echo() -> bool:
    return env_bool("SMOLAGENT_SHELL_ECHO", False)


def inline_steps() -> bool:
    return env_bool("SMOLAGENT_INLINE_STEPS", True)


def agent_kind() -> str:
    return os.getenv("SMOLAGENT_AGENT_KIND", "toolcalling").strip().lower()


def agent_instructions() -> str:
    extra = os.getenv("SMOLAGENT_AGENT_INSTRUCTIONS", "").strip()
    if extra:
        return DEFAULT_AGENT_INSTRUCTIONS + "\n\n" + extra
    return DEFAULT_AGENT_INSTRUCTIONS


def secret_fingerprint(value: str | None) -> str:
    if not value:
        return "missing"
    digest = hashlib.sha256(value.encode()).hexdigest()[:10]
    suffix = value[-4:] if len(value) >= 4 else value
    return f"set len={len(value)} sha256:{digest} suffix:{suffix}"


def openai_default_headers() -> dict[str, str]:
    headers: dict[str, str] = {}
    user_agent = os.getenv("SMOLAGENT_USER_AGENT")
    if user_agent:
        headers["User-Agent"] = user_agent

    raw_headers = os.getenv("SMOLAGENT_DEFAULT_HEADERS")
    if raw_headers:
        try:
            parsed = json.loads(raw_headers)
        except json.JSONDecodeError as error:
            raise RuntimeError("SMOLAGENT_DEFAULT_HEADERS must be a JSON object") from error
        if not isinstance(parsed, dict) or not all(
            isinstance(key, str) and isinstance(value, str)
            for key, value in parsed.items()
        ):
            raise RuntimeError("SMOLAGENT_DEFAULT_HEADERS must be a JSON object of string values")
        headers.update(parsed)

    return headers


def headers_for_display(headers: dict[str, str]) -> dict[str, str]:
    safe: dict[str, str] = {}
    for key, value in headers.items():
        if re.search(r"authorization|api[-_]?key|token|secret|cookie", key, re.IGNORECASE):
            safe[key] = secret_fingerprint(value)
        else:
            safe[key] = value
    return safe


def workspace_root() -> Path:
    return Path(os.getenv("SMOLAGENT_WORKSPACE", str(Path.cwd()))).expanduser().resolve()


def allow_outside_workspace() -> bool:
    return env_bool("SMOLAGENT_ALLOW_OUTSIDE_WORKSPACE", False)


def truncate_text(text: str, limit: int | None = None) -> str:
    limit = tool_max_chars() if limit is None else limit
    if limit <= 0 or len(text) <= limit:
        return text
    omitted = len(text) - limit
    return f"{text[:limit]}\n\n[truncated {omitted} characters]"


def one_line(text: Any, limit: int = 220) -> str:
    rendered = str(text).replace("\n", "\\n")
    if len(rendered) <= limit:
        return rendered
    return rendered[: limit - 15] + "...[truncated]"


def resolve_tool_path(path: str) -> Path:
    root = workspace_root()
    candidate = Path(path).expanduser()
    if not candidate.is_absolute():
        candidate = root / candidate
    resolved = candidate.resolve()
    if not allow_outside_workspace() and not resolved.is_relative_to(root):
        raise PermissionError(
            f"Path {resolved} is outside workspace {root}. "
            "Set SMOLAGENT_ALLOW_OUTSIDE_WORKSPACE=1 to allow it."
        )
    return resolved


def make_basic_tools(Tool):
    class ReadFileTool(Tool):
        name = "read_file"
        description = (
            "Read a UTF-8 text file from the current workspace. "
            "Use this before editing files so changes are based on current contents."
        )
        inputs = {
            "path": {
                "type": "string",
                "description": "Path to read, relative to the workspace unless absolute paths are allowed.",
            }
        }
        output_type = "string"

        def forward(self, path: str) -> str:
            resolved = resolve_tool_path(path)
            if not resolved.exists():
                return f"File not found: {resolved}"
            if not resolved.is_file():
                return f"Not a regular file: {resolved}"
            data = resolved.read_bytes()
            text = data.decode("utf-8", errors="replace")
            return truncate_text(text)

    class WriteFileTool(Tool):
        name = "write_file"
        description = (
            "Write UTF-8 text to a file in the current workspace, replacing existing contents. "
            "Parent directories are created if needed."
        )
        inputs = {
            "path": {
                "type": "string",
                "description": "Path to write, relative to the workspace unless absolute paths are allowed.",
            },
            "content": {
                "type": "string",
                "description": "Complete new file contents.",
            },
        }
        output_type = "string"

        def forward(self, path: str, content: str) -> str:
            resolved = resolve_tool_path(path)
            resolved.parent.mkdir(parents=True, exist_ok=True)
            resolved.write_text(content, encoding="utf-8")
            return f"Wrote {len(content)} characters to {resolved}"

    class EditFileTool(Tool):
        name = "edit_file"
        description = (
            "Edit a UTF-8 text file by replacing exactly one occurrence of old_text with new_text. "
            "If old_text is missing or appears multiple times, no change is made."
        )
        inputs = {
            "path": {
                "type": "string",
                "description": "Path to edit, relative to the workspace unless absolute paths are allowed.",
            },
            "old_text": {
                "type": "string",
                "description": "Exact text to replace. Include enough surrounding context to make it unique.",
            },
            "new_text": {
                "type": "string",
                "description": "Replacement text.",
            },
        }
        output_type = "string"

        def forward(self, path: str, old_text: str, new_text: str) -> str:
            resolved = resolve_tool_path(path)
            if not resolved.exists():
                return f"File not found: {resolved}"
            if not resolved.is_file():
                return f"Not a regular file: {resolved}"
            content = resolved.read_text(encoding="utf-8", errors="replace")
            count = content.count(old_text)
            if count == 0:
                return f"No match for old_text in {resolved}; file was not changed."
            if count > 1:
                return f"old_text matched {count} times in {resolved}; file was not changed."
            updated = content.replace(old_text, new_text, 1)
            resolved.write_text(updated, encoding="utf-8")
            return (
                f"Edited {resolved}: replaced {len(old_text)} characters "
                f"with {len(new_text)} characters."
            )

    class BashTool(Tool):
        name = "bash"
        description = (
            "Run a non-interactive shell command in the current kernel working directory. "
            "Stdout and stderr are combined and returned."
        )
        inputs = {
            "command": {
                "type": "string",
                "description": "Shell command to run.",
            }
        }
        output_type = "string"

        def forward(self, command: str) -> str:
            if not command.strip():
                return "No command provided."
            shell = os.getenv("SMOLAGENT_SHELL") or os.getenv("SHELL") or "/bin/zsh"
            try:
                proc = subprocess.run(
                    [shell, "-lc", "stty -g >/dev/null 2>&1 || true; " + command],
                    cwd=Path.cwd(),
                    stdout=subprocess.PIPE,
                    stderr=subprocess.STDOUT,
                    stdin=subprocess.DEVNULL,
                    text=True,
                    timeout=bash_timeout(),
                )
            except subprocess.TimeoutExpired as error:
                output = error.stdout or ""
                return truncate_text(
                    f"$ {command}\n{output}\n[timed out after {bash_timeout()} seconds]"
                )
            output = proc.stdout.rstrip("\n")
            if proc.returncode == 0:
                return truncate_text(output)
            return truncate_text(f"$ {command}\n{output}\n[exit {proc.returncode}]")

    return [ReadFileTool(), WriteFileTool(), EditFileTool(), BashTool()]


class Kernel:
    def __init__(self) -> None:
        self.agent = None
        self.agent_has_memory = False
        self.model_id = os.getenv("SMOLAGENT_MODEL", "glm-5")
        self.model_class = os.getenv("SMOLAGENT_MODEL_CLASS", "openai")
        self.max_steps = env_int("SMOLAGENT_MAX_STEPS", 4)
        self.inline_trace = env_bool("SMOLAGENT_INLINE_TRACE", False)
        self.stream_outputs = env_bool("SMOLAGENT_STREAM_OUTPUTS", self.inline_trace)
        self.loaded_context = ""

    def run(self) -> None:
        writeln("SmolAgent dREPL POC")
        writeln("Type /help for commands.")
        while True:
            write(PROMPT)
            try:
                msg = read_msg()
            except EOFError:
                return
            except Exception:
                traceback.print_exc()
                continue

            op = msg.get("op")
            try:
                if op == "eval":
                    self.eval(msg)
                elif op == "complete":
                    self.complete(msg)
                elif op == "checkinput":
                    self.checkinput(msg)
                elif op == "describe":
                    self.describe(msg)
                elif op == "setoptions":
                    send_msg(id=msg.get("id"))
                else:
                    writeln(f"Unsupported op: {op}")
                    if msg.get("id") is not None:
                        send_msg(id=msg.get("id"))
            except KeyboardInterrupt:
                writeln("Interrupted")
                if msg.get("id") is not None:
                    send_msg(id=msg.get("id"))
            except Exception:
                traceback.print_exc()
                if msg.get("id") is not None:
                    send_msg(id=msg.get("id"))

    def eval(self, msg: dict[str, Any]) -> None:
        send_msg(op="status", status="rawio")
        code = msg.get("code") or ""
        if self.handle_command(code):
            send_msg(id=msg.get("id"))
            return

        kind, run_id, body = split_directive(code)
        if run_id:
            send_msg(op="run-start", run_id=run_id, kind=kind or "agent")

        try:
            if kind in {"sh", "shell", "bash", "zsh"}:
                exit_code = self.run_shell(body, run_id=run_id)
                if run_id:
                    send_msg(
                        op="run-end",
                        run_id=run_id,
                        status="ok" if exit_code == 0 else f"exit {exit_code}",
                    )
            elif kind == "agent":
                if body:
                    self.run_agent(body, run_id=run_id)
                if run_id:
                    send_msg(op="run-end", run_id=run_id, status="ok")
            elif kind == "load":
                self.load_context(body, run_id=run_id)
                if run_id:
                    send_msg(op="run-end", run_id=run_id, status="ok")
            else:
                shell_command = shell_command_from_code(code)
                if shell_command is not None:
                    self.run_shell(shell_command)
                else:
                    prompt = agent_prompt_from_code(code)
                    if prompt:
                        self.run_agent(prompt)
        except Exception:
            text = traceback.format_exc()
            if run_id:
                send_mime("text/plain", text, run_id=run_id, stream="stderr")
                send_msg(op="run-end", run_id=run_id, status="error")
            else:
                write(text)
        send_msg(id=msg.get("id"))

    def handle_command(self, code: str) -> bool:
        stripped = code.strip()
        if not stripped.startswith("/"):
            return False

        command, _, arg = stripped.partition(" ")
        if command == "/help":
            self.print_help()
        elif command == "/cd":
            self.change_directory(arg)
        elif command == "/pwd":
            writeln(str(Path.cwd()))
        elif command == "/reset":
            self.agent = None
            self.agent_has_memory = False
            self.loaded_context = ""
            writeln("Agent memory reset.")
        elif command == "/context":
            self.handle_context_command(arg)
        elif command == "/model":
            if arg.strip():
                self.model_id = arg.strip()
                self.agent = None
                self.agent_has_memory = False
            self.print_model_config()
        elif command == "/env":
            self.print_model_config()
        elif command in {"/agent", "/shell"}:
            if not arg.strip():
                writeln(f"Usage: {command} TEXT")
                return True
            return False
        else:
            writeln(f"Unknown command: {command}")
            writeln("Type /help for commands.")
        return True

    def print_help(self) -> None:
        writeln("Commands:")
        writeln("  /agent TEXT       run TEXT with smolagents CodeAgent")
        writeln("  /shell CMD        run CMD with the login shell")
        writeln("  !CMD or $ CMD     shell shorthand")
        writeln("  /cd DIR           change the kernel working directory")
        writeln("  /pwd              print the kernel working directory")
        writeln("  /model [MODEL]    show or set SMOLAGENT_MODEL")
        writeln("  /env              show model environment without printing secrets")
        writeln("  /context [clear]  show or clear loaded buffer context")
        writeln("  /reset            reset the cached CodeAgent")
        writeln("")
        writeln("Cells in drepl-smolagent-cells-mode:")
        writeln("  # %%              plain text is sent to CodeAgent")
        writeln("  # %%              !CMD runs CMD with the shell")
        writeln("  # %%              /load stores the buffer transcript as context")
        writeln("")
        writeln("Agent tools:")
        writeln("  read_file, write_file, edit_file, bash")
        writeln("")
        writeln("Environment:")
        writeln("  SMOLAGENT_MODEL_CLASS=litellm|openai|inference")
        writeln("  SMOLAGENT_MODEL=glm-5")
        writeln("  SMOLAGENT_API_BASE=https://...")
        writeln("  SMOLAGENT_API_KEY=...")
        writeln("  SMOLAGENT_USER_AGENT=OpenAI/Go 3.22.0")
        writeln("  SMOLAGENT_DEFAULT_HEADERS='{\"Header\":\"value\"}'")
        writeln("  SMOLAGENT_MAX_STEPS=4")
        writeln("  SMOLAGENT_AGENT_KIND=toolcalling")
        writeln("  SMOLAGENT_INLINE_TRACE=0")
        writeln("  SMOLAGENT_INLINE_STEPS=1")
        writeln("  SMOLAGENT_STREAM_OUTPUTS=0")
        writeln("  SMOLAGENT_SHELL_ECHO=0")
        writeln("  SMOLAGENT_WORKSPACE=<kernel cwd>")
        writeln("  SMOLAGENT_ALLOW_OUTSIDE_WORKSPACE=0")
        writeln("  SMOLAGENT_TOOL_MAX_CHARS=60000")
        writeln("  SMOLAGENT_BASH_TIMEOUT=60")

    def print_model_config(self) -> None:
        api_base = os.getenv("SMOLAGENT_API_BASE") or os.getenv("OPENAI_API_BASE")
        api_key = os.getenv("SMOLAGENT_API_KEY") or os.getenv("OPENAI_API_KEY")
        headers = openai_default_headers()
        hf_token = os.getenv("HF_TOKEN")
        writeln(f"SMOLAGENT_MODEL_CLASS={self.model_class}")
        writeln(f"SMOLAGENT_MODEL={self.model_id}")
        writeln(f"SMOLAGENT_API_BASE={api_base or ''}")
        writeln(f"SMOLAGENT_API_KEY={secret_fingerprint(os.getenv('SMOLAGENT_API_KEY'))}")
        writeln(f"OPENAI_API_KEY={secret_fingerprint(os.getenv('OPENAI_API_KEY'))}")
        writeln(f"EFFECTIVE_API_KEY={secret_fingerprint(api_key)}")
        display_headers = headers_for_display(headers)
        writeln(f"OPENAI_DEFAULT_HEADERS={json.dumps(display_headers, sort_keys=True) if headers else '{}'}")
        writeln(f"HF_TOKEN={'set' if hf_token else 'missing'}")
        writeln(f"SMOLAGENT_AGENT_KIND={agent_kind()}")
        writeln(f"SMOLAGENT_INLINE_TRACE={int(self.inline_trace)}")
        writeln(f"SMOLAGENT_INLINE_STEPS={int(inline_steps())}")
        writeln(f"SMOLAGENT_STREAM_OUTPUTS={int(self.stream_outputs)}")
        writeln(f"SMOLAGENT_SHELL_ECHO={int(shell_echo())}")
        writeln(f"SMOLAGENT_WORKSPACE={workspace_root()}")
        writeln(f"SMOLAGENT_ALLOW_OUTSIDE_WORKSPACE={int(allow_outside_workspace())}")
        writeln(f"SMOLAGENT_TOOL_MAX_CHARS={tool_max_chars()}")
        writeln(f"SMOLAGENT_BASH_TIMEOUT={bash_timeout()}")
        writeln(f"LOADED_CONTEXT_CHARS={len(self.loaded_context)}")

    def change_directory(self, arg: str) -> None:
        if not arg.strip():
            writeln(str(Path.cwd()))
            return
        target = Path(os.path.expandvars(os.path.expanduser(arg.strip()))).resolve()
        os.chdir(target)
        writeln(str(Path.cwd()))

    def handle_context_command(self, arg: str) -> None:
        if arg.strip() == "clear":
            self.loaded_context = ""
            writeln("Loaded context cleared.")
        elif self.loaded_context:
            writeln(f"Loaded context: {len(self.loaded_context)} chars.")
        else:
            writeln("No loaded context.")

    def load_context(self, context: str, run_id: str | None = None) -> None:
        self.loaded_context = context
        message = f"Loaded buffer context ({len(context)} chars).\n"
        if run_id:
            send_mime("text/plain", message, run_id=run_id, stream="stdout")
        else:
            write(message)

    def prompt_with_context(self, prompt: str) -> str:
        if not self.loaded_context:
            return prompt
        return (
            "You are working in an Emacs code-cells session. "
            "Use the loaded transcript as background context, then answer "
            "or act on the current request.\n\n"
            "<loaded_transcript>\n"
            f"{self.loaded_context}\n"
            "</loaded_transcript>\n\n"
            "<current_request>\n"
            f"{prompt}\n"
            "</current_request>"
        )

    def emit_output(self, text: str, run_id: str | None = None, stream: str = "stdout") -> None:
        if not text:
            return
        if run_id:
            send_mime("text/plain", text, run_id=run_id, stream=stream)
        else:
            write(text)

    def run_shell(self, command: str, run_id: str | None = None) -> int:
        if not command:
            return 0
        shell = os.getenv("SMOLAGENT_SHELL") or os.getenv("SHELL") or "/bin/zsh"
        if shell_echo():
            self.emit_output(f"$ {command}\n", run_id=run_id)
        proc = subprocess.Popen(
            [shell, "-lc", "stty -g >/dev/null 2>&1 || true; " + command],
            cwd=Path.cwd(),
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            stdin=sys.stdin,
            text=True,
            bufsize=1,
        )
        assert proc.stdout is not None
        for line in proc.stdout:
            self.emit_output(line, run_id=run_id)
        code = proc.wait()
        if code != 0 or shell_echo():
            self.emit_output(f"[exit {code}]\n", run_id=run_id)
        return code

    def run_agent(self, prompt: str, run_id: str | None = None) -> None:
        agent = self.get_agent()
        prompt = self.prompt_with_context(prompt)
        if run_id:
            stream = MimeStream(self, run_id) if self.inline_trace else NullStream()
            with redirect_stdout(stream), redirect_stderr(stream):
                if inline_steps():
                    result = self.run_agent_stream(agent, prompt, run_id)
                else:
                    result = agent.run(
                        prompt,
                        reset=not self.agent_has_memory,
                        max_steps=self.max_steps,
                    )
        else:
            result = agent.run(
                prompt,
                reset=not self.agent_has_memory,
                max_steps=self.max_steps,
            )
        self.agent_has_memory = True
        final = f"{str(result).strip()}\n" if result is not None else ""
        if run_id:
            if final:
                send_mime("text/markdown", final, run_id=run_id, stream="result")
        else:
            write(final)

    def run_agent_stream(self, agent, prompt: str, run_id: str) -> Any:
        final = None
        for event in agent.run(
            prompt,
            stream=True,
            reset=not self.agent_has_memory,
            max_steps=self.max_steps,
        ):
            summary = self.agent_event_summary(event)
            if summary:
                send_mime("text/plain", summary, run_id=run_id, stream="step")
            if type(event).__name__ == "FinalAnswerStep":
                final = getattr(event, "output", None)
        return final

    def agent_event_summary(self, event: Any) -> str:
        event_type = type(event).__name__
        if event_type == "ToolCall":
            name = getattr(event, "name", "tool")
            if name == "final_answer":
                return ""
            arguments = getattr(event, "arguments", "")
            return f"{name}({one_line(arguments)})\n"
        if event_type == "ToolOutput":
            return ""
        if event_type == "ActionStep":
            error = getattr(event, "error", None)
            if error:
                step = getattr(event, "step_number", "?")
                return f"step {step} error: {one_line(error)}\n"
        return ""

    def get_agent(self):
        if self.agent is not None:
            return self.agent

        try:
            from smolagents import CodeAgent, Tool, ToolCallingAgent
        except Exception as error:
            raise RuntimeError(
                "Could not import smolagents. Run this kernel via "
                "`uv run --script emacs/vendor/drepl-smolagent.py`."
            ) from error

        model = self.make_model()
        tools = make_basic_tools(Tool)
        authorized_imports = [
            item.strip()
            for item in os.getenv("SMOLAGENT_AUTHORIZED_IMPORTS", "").split(",")
            if item.strip()
        ]
        if agent_kind() in {"code", "codeagent", "code-agent"}:
            self.agent = CodeAgent(
                tools=tools,
                model=model,
                additional_authorized_imports=authorized_imports or None,
                instructions=agent_instructions(),
                max_steps=self.max_steps,
                stream_outputs=self.stream_outputs,
            )
        else:
            self.agent = ToolCallingAgent(
                tools=tools,
                model=model,
                instructions=agent_instructions(),
                max_steps=self.max_steps,
                stream_outputs=self.stream_outputs,
            )
        return self.agent

    def make_model(self):
        model_class = self.model_class.lower()
        temperature = env_float("SMOLAGENT_TEMPERATURE", 0.2)
        max_tokens = env_int("SMOLAGENT_MAX_TOKENS", 2000)
        api_base = os.getenv("SMOLAGENT_API_BASE") or os.getenv("OPENAI_API_BASE")
        api_key = os.getenv("SMOLAGENT_API_KEY") or os.getenv("OPENAI_API_KEY")
        headers = openai_default_headers()

        if model_class in {"openai", "openai-compatible"}:
            from smolagents import OpenAIModel

            if not api_key:
                raise RuntimeError(
                    "Missing API key for SMOLAGENT_MODEL_CLASS=openai. "
                    "Set SMOLAGENT_API_KEY, OPENAI_API_KEY, or make sure "
                    "Emacs auth-source has an entry with host \"alibaba\", "
                    "then restart the SmolAgent dREPL buffer."
                )

            return OpenAIModel(
                model_id=self.model_id,
                api_base=api_base,
                api_key=api_key,
                client_kwargs={"default_headers": headers} if headers else None,
                temperature=temperature,
                max_tokens=max_tokens,
            )

        if model_class in {"hf", "huggingface", "inference"}:
            from smolagents import InferenceClientModel

            return InferenceClientModel(
                model_id=self.model_id,
                token=os.getenv("HF_TOKEN"),
                temperature=temperature,
                max_tokens=max_tokens,
            )

        from smolagents import LiteLLMModel

        if self.model_id.startswith("openai/") and not api_key:
            raise RuntimeError(
                "Missing API key for the default LiteLLM OpenAI model. "
                "Set SMOLAGENT_API_KEY or OPENAI_API_KEY, or configure "
                "SMOLAGENT_MODEL_CLASS/SMOLAGENT_MODEL for another provider."
            )

        return LiteLLMModel(
            model_id=self.model_id,
            api_base=api_base,
            api_key=api_key,
            temperature=temperature,
            max_tokens=max_tokens,
        )

    def complete(self, msg: dict[str, Any]) -> None:
        code = msg.get("code") or ""
        pos = int(msg.get("pos") or len(code))
        before = code[:pos]
        match = re.search(r"/[A-Za-z-]*$", before)
        if not match:
            send_msg(id=msg.get("id"), candidates=[])
            return
        prefix = match.group(0)
        candidates = [
            {"text": command, "annot": " SmolAgent command"}
            for command in SLASH_COMMANDS
            if command.startswith(prefix)
        ]
        send_msg(id=msg.get("id"), prefix=prefix, candidates=candidates)

    def checkinput(self, msg: dict[str, Any]) -> None:
        send_msg(id=msg.get("id"), status="complete", prompt=PROMPT)

    def describe(self, msg: dict[str, Any]) -> None:
        code = msg.get("code") or ""
        pos = int(msg.get("pos") or len(code))
        before = code[:pos]
        match = re.search(r"/[A-Za-z-]*$", before)
        if match and match.group(0) in SLASH_COMMANDS:
            send_msg(
                id=msg.get("id"),
                name=match.group(0),
                type="SmolAgent command",
                text="Run /help for the command list.",
            )
        else:
            send_msg(id=msg.get("id"))


class MimeStream:
    def __init__(self, kernel: Kernel, run_id: str) -> None:
        self.kernel = kernel
        self.run_id = run_id

    def write(self, text: str) -> int:
        self.kernel.emit_output(text, run_id=self.run_id)
        return len(text)

    def flush(self) -> None:
        flush()


class NullStream:
    def write(self, text: str) -> int:
        return len(text)

    def flush(self) -> None:
        pass


if __name__ == "__main__":
    Kernel().run()
