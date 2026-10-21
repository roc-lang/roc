//! Live duplex transport for compiler-backed LSP specs. Input stays open until
//! closeInput or deinit; output is consumed only as complete JSON-RPC frames.
const std = @import("std");
const lsp = @import("lsp");
const env = @import("integration_env.zig");
const helpers = @import("helpers.zig");

pub const WaitError = std.mem.Allocator.Error || error{ Timeout, Closed, InvalidFrame };

/// Returns an awake-clock deadline after the given number of milliseconds.
pub fn deadline(milliseconds: i64) std.Io.Timeout {
    return (std.Io.Timeout{ .duration = .{ .raw = .fromMilliseconds(milliseconds), .clock = .awake } }).toDeadline(env.io);
}

/// A synchronized byte queue used as the harness transport's input and output.
pub const Queue = struct {
    mutex: std.Io.Mutex = .init,
    changed: std.Io.Condition = .init,
    bytes: std.ArrayList(u8) = .empty,
    offset: usize = 0,
    closed: bool = false,
    fail_read: bool = false,
    reads: usize = 0,
    waiting: std.Io.Event = .unset,

    pub fn deinit(q: *Queue) void {
        q.bytes.deinit(env.allocator);
    }

    pub fn close(q: *Queue) void {
        q.mutex.lockUncancelable(env.io);
        defer q.mutex.unlock(env.io);
        q.closed = true;
        q.changed.broadcast(env.io);
    }

    pub fn write(q: *Queue, bytes: []const u8) error{ OutOfMemory, Closed }!usize {
        q.mutex.lockUncancelable(env.io);
        defer q.mutex.unlock(env.io);
        if (q.closed) return error.Closed;
        try q.bytes.appendSlice(env.allocator, bytes);
        q.changed.broadcast(env.io);
        return bytes.len;
    }

    pub fn failRead(q: *Queue) void {
        q.mutex.lockUncancelable(env.io);
        defer q.mutex.unlock(env.io);
        q.fail_read = true;
        q.changed.broadcast(env.io);
    }

    pub fn read(q: *Queue, out: []u8) error{InjectedReadFailure}!usize {
        q.mutex.lockUncancelable(env.io);
        defer q.mutex.unlock(env.io);
        while (q.offset == q.bytes.items.len and !q.closed and !q.fail_read) {
            q.waiting.set(env.io);
            q.changed.waitUncancelable(env.io, &q.mutex);
        }
        if (q.fail_read) return error.InjectedReadFailure;
        const count = @min(out.len, q.bytes.items.len - q.offset);
        @memcpy(out[0..count], q.bytes.items[q.offset..][0..count]);
        q.offset += count;
        q.reads += 1;
        return count;
    }

    /// Cancellation is deliberately idempotent and wakes an idle transport read.
    pub fn cancel(q: *Queue) void {
        q.close();
    }

    pub fn nextFrame(q: *Queue, timeout: std.Io.Timeout) WaitError![]u8 {
        const until = timeout.toDeadline(env.io);
        q.mutex.lockUncancelable(env.io);
        defer q.mutex.unlock(env.io);
        while (true) {
            const bytes = q.bytes.items[q.offset..];
            if (std.mem.find(u8, bytes, "\r\n\r\n")) |header_end| {
                const prefix = "Content-Length: ";
                if (!std.mem.startsWith(u8, bytes, prefix)) return error.InvalidFrame;
                const length = std.fmt.parseInt(usize, bytes[prefix.len..header_end], 10) catch return error.InvalidFrame;
                if (length > 16 * 1024 * 1024) return error.InvalidFrame;
                const end = header_end + 4 + length;
                if (bytes.len >= end) {
                    const body = try env.allocator.dupe(u8, bytes[header_end + 4 .. end]);
                    q.offset += end;
                    return body;
                }
            } else if (bytes.len > 8192) return error.InvalidFrame;
            if (q.closed) return error.Closed;
            q.changed.waitTimeout(env.io, &q.mutex, until) catch |err| switch (err) {
                error.Timeout => return error.Timeout,
                error.Canceled => return error.Closed,
            };
        }
    }
};

/// Adapts a queue to the server's blocking input and cancellation interface.
pub const Reader = struct {
    queue: *Queue,
    pub fn read(r: *Reader, bytes: []u8) error{InjectedReadFailure}!usize {
        return r.queue.read(bytes);
    }
    pub fn cancel(r: *Reader) void {
        r.queue.close();
    }
};
/// Adapts server output writes to the harness's synchronized output queue.
pub const Writer = struct {
    queue: *Queue,
    pub fn writeAll(w: *Writer, bytes: []const u8) error{ OutOfMemory, Closed }!void {
        _ = try w.queue.write(bytes);
    }
};

/// LSP server type backed by the test harness's queue adapters.
pub const Server = lsp.server.Server(Reader, Writer);

/// Builds a threaded LSP harness for the supplied server type.
pub fn Harness(comptime ServerType: type) type {
    return struct {
        const Self = @This();
        input: Queue = .{},
        output: Queue = .{},
        server: ServerType = undefined,
        thread: ?std.Thread = null,
        finished: std.Io.Event = .unset,
        run_error: ?ServerType.RunError = null,

        /// The harness must stay at this address until deinit returns.
        pub fn init(h: *Self, cache_root: []const u8) std.mem.Allocator.Error!void {
            h.* = .{};
            h.server = try ServerType.init(env.allocator, env.io, .{ .queue = &h.input }, .{ .queue = &h.output }, null, .{});
            env.configureChecker(&h.server.syntax_checker, cache_root);
        }
        pub fn start(h: *Self) std.Thread.SpawnError!void {
            h.thread = try std.Thread.spawn(.{}, worker, .{h});
        }
        fn worker(h: *Self) void {
            h.server.run() catch |err| {
                h.run_error = err;
            };
            h.output.close();
            h.finished.set(env.io);
        }
        pub fn closeInput(h: *Self) void {
            h.input.close();
        }
        pub fn waitFinished(h: *Self, timeout: std.Io.Timeout) (std.Io.Event.WaitTimeoutError || ServerType.RunError)!void {
            try h.finished.waitTimeout(env.io, timeout);
            if (h.run_error) |err| return err;
        }
        pub fn deinit(h: *Self) void {
            h.closeInput();
            if (h.thread) |thread| thread.join();
            h.server.deinit();
            h.input.deinit();
            h.output.deinit();
        }
        pub fn send(h: *Self, value: anytype) error{ OutOfMemory, Closed }!void {
            const body = try std.json.Stringify.valueAlloc(env.allocator, value, .{});
            defer env.allocator.free(body);
            try h.sendBody(body);
        }
        pub fn sendBody(h: *Self, body: []const u8) error{ OutOfMemory, Closed }!void {
            const frame = try helpers.frame(env.allocator, body);
            defer env.allocator.free(frame);
            _ = try h.input.write(frame);
        }
        pub fn next(h: *Self, timeout: std.Io.Timeout) WaitError!std.json.Parsed(std.json.Value) {
            const body = try h.output.nextFrame(timeout);
            defer env.allocator.free(body);
            return std.json.parseFromSlice(std.json.Value, env.allocator, body, .{ .allocate = .alloc_always }) catch return error.InvalidFrame;
        }
    };
}

/// Threaded test harness specialized for the LSP server and queue adapters.
pub const Live = Harness(Server);

/// Counted server boundaries. A selected boundary can be held until release;
/// tests must release it before tearing down the server worker.
pub const Control = struct {
    const Boundary = lsp.server.TestHooks.Boundary;
    mutex: std.Io.Mutex = .init,
    changed: std.Io.Condition = .init,
    hooks: lsp.server.TestHooks = .{},
    counts: [4]usize = @splat(0),
    held: ?Boundary = null,
    released: bool = false,

    pub fn init(c: *Control, h: *Live) void {
        c.* = .{};
        c.hooks.context = c;
        c.hooks.boundary = arrive;
        h.server.test_hooks = &c.hooks;
    }
    fn arrive(context: ?*anyopaque, point: Boundary) void {
        const c: *Control = @ptrCast(@alignCast(context.?));
        c.mutex.lockUncancelable(env.io);
        defer c.mutex.unlock(env.io);
        c.counts[@backingInt(point)] += 1;
        c.changed.broadcast(env.io);
        while (c.held == point and !c.released) c.changed.waitUncancelable(env.io, &c.mutex);
    }
    pub fn hold(c: *Control, point: Boundary) void {
        c.mutex.lockUncancelable(env.io);
        defer c.mutex.unlock(env.io);
        c.held = point;
        c.released = false;
    }
    pub fn release(c: *Control) void {
        c.mutex.lockUncancelable(env.io);
        defer c.mutex.unlock(env.io);
        c.released = true;
        c.held = null;
        c.changed.broadcast(env.io);
    }
    /// Wait for a quiescent comparison that started after this explicit request.
    pub fn reconcile(c: *Control, h: *Live, timeout: std.Io.Timeout) std.Io.Condition.WaitTimeoutError!void {
        const until = timeout.toDeadline(env.io);
        c.mutex.lockUncancelable(env.io);
        defer c.mutex.unlock(env.io);
        const generation = c.hooks.reconcile_requested.fetchAdd(1, .seq_cst) + 1;
        h.server.wakeForTesting();
        while (c.hooks.reconcile_acknowledged.load(.seq_cst) < generation) try c.changed.waitTimeout(env.io, &c.mutex, until);
    }
    pub fn count(c: *Control, point: Boundary) usize {
        c.mutex.lockUncancelable(env.io);
        defer c.mutex.unlock(env.io);
        return c.counts[@backingInt(point)];
    }
    pub fn wait(c: *Control, point: Boundary, count_at_least: usize, timeout: std.Io.Timeout) std.Io.Condition.WaitTimeoutError!void {
        const until = timeout.toDeadline(env.io);
        c.mutex.lockUncancelable(env.io);
        defer c.mutex.unlock(env.io);
        while (c.counts[@backingInt(point)] < count_at_least) try c.changed.waitTimeout(env.io, &c.mutex, until);
    }
};
