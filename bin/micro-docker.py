import subprocess
import os
import sys
import time
import json
import shutil

# --- CONFIGURATION ---
HOST_VM_NAME = "crun-host-podman"
PODMAN_SOCKET_PATH = f"/tmp/podman-{HOST_VM_NAME}.sock"

def check_requirements():
    """Ensure necessary native binaries are present."""
    for binary in ["container", "podman"]:
        if not shutil.which(binary):
            print(f"❌ Error: {binary} is missing from your PATH.")
            sys.exit(1)

    # Check if Apple container service is up
    result = subprocess.run(["container", "system", "status"], capture_output=True, text=True)
    if "running" not in result.stdout.lower():
        print("⚠️ Apple container system daemon is not running. Starting it...")
        subprocess.run(["sudo", "container", "system", "start"], check=True)

def create_host_configuration():
    """Generates the automated entry script inside the Apple Micro-VM to inject crun and podman."""
    # We boot an alpine image because it has a 0MB base footprint and boots instantly
    init_script = """#!/bin/sh
apk update && apk add --no-cache crun podman shadow
mkdir -p /etc/containers
cat <<EOF > /etc/containers/containers.conf
[engine]
runtime = "crun"
EOF
# Launch the podman system service daemon listening to the internal vsock/stream
podman system service --time=0 unix:///tmp/podman.sock
"""
    with open("/tmp/vm_provision.sh", "w") as f:
        f.write(init_script)
    os.chmod("/tmp/vm_provision.sh", 0o755)

def launch_micro_vm():
    """Launches the singular Apple Container Micro-VM acting as the process host."""
    print(f"🚀 Initializing Apple Micro-VM Host [{HOST_VM_NAME}]...")

    # Check if already running
    check_run = subprocess.run(["container", "ls", "--format", "json"], capture_output=True, text=True)
    if HOST_VM_NAME in check_run.stdout:
        print("ℹ️ Host is already alive. Re-mapping network sockets.")
        return

    # Trigger raw Apple Containerization. Framework via the native tool
    # Mounts your local script and runs it inside the clean Linux Kernel space
    cmd = [
        "container", "run", "-d",
        "--name", HOST_VM_NAME,
        "--volume", "/tmp/vm_provision.sh:/etc/local_init.sh",
        "--volume", f"{os.path.expanduser('~')}:{os.path.expanduser('~')}", # Pure VirtioFS mount
        "alpine:latest",
        "/etc/local_init.sh"
    ]

    subprocess.run(cmd, check=True)
    print("⏳ Waiting for the kernel to boot and crun to initialize...")
    time.sleep(3) # Give vminitd and alpine a moment to settle

def expose_socket_to_mac():
    """Bridges the Podman socket inside the Apple Micro-VM out to macOS."""
    if os.path.exists(PODMAN_SOCKET_PATH):
        os.remove(PODMAN_SOCKET_PATH)

    print(f"🔌 Exposing Podman/crun engine socket to Mac at: {PODMAN_SOCKET_PATH}")

    # Use container exec to map the internal socket stream natively out to the mac file system
    # This keeps your Mac's terminal tooling completely native without proxy daemons
    forward_cmd = f"container exec {HOST_VM_NAME} socat - UNIX-CONNECT:/tmp/podman.sock"

    # We spawn a background process on the Mac to keep this stream piped open
    log_file = open("/tmp/apple_container_socket.log", "w")
    subprocess.Popen(
        ["nc", "-lk", PODMAN_SOCKET_PATH, "-e", forward_cmd],
        stdout=log_file, stderr=log_file
    )

def print_instructions():
    """Prints user instructions for terminal activation."""
    print("\n" + "="*50)
    print(" 🎉 SUCCESS: YOUR LIGHTWEIGHT HOSTER IS ONLINE! 🎉")
    print("="*50)
    print(f"Your architecture: Mac OS -> Apple Micro-VM -> crun (Process Isolated)")
    print("\nTo connect your Mac's podman/docker CLI, run this in your terminal:")
    print(f"\n   export DOCKER_HOST=\"unix://{PODMAN_SOCKET_PATH}\"")
    print("\nTest it out with:")
    print("   podman run --rm alpine uname -a")
    print("="*50 + "\n")

if __name__ == "__main__":
    check_requirements()
    create_host_configuration()
    launch_micro_vm()
    expose_socket_to_mac()
    print_instructions()
