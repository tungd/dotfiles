import CEmacsHost
import Foundation

enum PTYProcessError: Error, CustomStringConvertible {
    case spawnFailed(errno: Int32)
    case nonBlockingFailed(errno: Int32)
    case closed

    var description: String {
        switch self {
        case .spawnFailed(let errno):
            "failed to spawn PTY child: errno \(errno)"
        case .nonBlockingFailed(let errno):
            "failed to set PTY non-blocking: errno \(errno)"
        case .closed:
            "PTY is closed"
        }
    }
}

final class PTYProcess: @unchecked Sendable {
    private let lock = NSLock()
    private var masterFD: Int32
    private let childPID: pid_t
    private var closed = false

    init(argv: [String], rows: Int, cols: Int) throws {
        let duplicated = argv.map { strdup($0) }
        defer {
            for pointer in duplicated {
                free(pointer)
            }
        }

        var cArgv: [UnsafeMutablePointer<CChar>?] = duplicated
        cArgv.append(nil)

        var master: Int32 = -1
        let pid = cArgv.withUnsafeMutableBufferPointer { buffer in
            eh_spawn_pty(&master, buffer.baseAddress, Int32(rows), Int32(cols))
        }

        guard pid >= 0 else {
            throw PTYProcessError.spawnFailed(errno: errno)
        }

        guard eh_set_nonblocking(master) == 0 else {
            let savedErrno = errno
            close(master)
            kill(pid, SIGTERM)
            throw PTYProcessError.nonBlockingFailed(errno: savedErrno)
        }

        masterFD = master
        childPID = pid
    }

    func resize(rows: Int, cols: Int) {
        let fd = lockedFD()
        guard fd >= 0 else { return }
        _ = eh_resize_pty(fd, Int32(rows), Int32(cols))
    }

    func write(_ bytes: [UInt8]) throws {
        let fd = lockedFD()
        guard fd >= 0 else { throw PTYProcessError.closed }

        try bytes.withUnsafeBytes { rawBuffer in
            guard let baseAddress = rawBuffer.baseAddress else { return }
            var written = 0
            while written < bytes.count {
                let result = Darwin.write(fd, baseAddress.advanced(by: written), bytes.count - written)
                if result > 0 {
                    written += result
                } else if result == -1 && errno == EINTR {
                    continue
                } else if result == -1 && (errno == EAGAIN || errno == EWOULDBLOCK) {
                    usleep(1_000)
                } else {
                    throw PTYProcessError.closed
                }
            }
        }
    }

    func readLoop(_ onBytes: @escaping @Sendable ([UInt8]) async -> Void) async {
        var buffer = [UInt8](repeating: 0, count: 16_384)
        while !Task.isCancelled {
            let fd = lockedFD()
            guard fd >= 0 else { return }

            let count = buffer.withUnsafeMutableBytes { rawBuffer in
                Darwin.read(fd, rawBuffer.baseAddress, rawBuffer.count)
            }

            if count > 0 {
                await onBytes(Array(buffer.prefix(count)))
            } else if count == 0 {
                return
            } else if errno == EINTR {
                continue
            } else if errno == EAGAIN || errno == EWOULDBLOCK {
                try? await Task.sleep(nanoseconds: 8_000_000)
            } else {
                return
            }
        }
    }

    func terminate() {
        lock.lock()
        if !closed {
            closed = true
            if masterFD >= 0 {
                close(masterFD)
                masterFD = -1
            }
            kill(childPID, SIGHUP)
            kill(childPID, SIGTERM)
        }
        lock.unlock()
    }

    deinit {
        terminate()
    }

    private func lockedFD() -> Int32 {
        lock.lock()
        let fd = closed ? -1 : masterFD
        lock.unlock()
        return fd
    }
}
