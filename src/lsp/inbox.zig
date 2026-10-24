//! Messages the LSP reader task has received and the server has not handled
//! yet, shared between the two.
//!
//! The reader task appends every message it reads and records whether it is
//! about to wait for more client input. The server handles messages in order
//! and runs a scheduled document check only when no message is pending and the
//! reader is waiting, so a burst of edits is checked once, against its final
//! text. A message that makes the check in progress stale (an edit to the
//! checked document, or the end of the session) sets `cancel_request`, which
//! the compiler reads between units of work.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// A message as the reader task received it.
pub const Message = struct {
    payload: []u8,
    /// Null when the payload is not JSON, which the server answers with a
    /// parse error. Strings in it may point into `payload`.
    parsed: ?std.json.Parsed(std.json.Value),

    /// Read the JSON in `payload`, taking ownership of it.
    pub fn init(allocator: Allocator, payload: []u8) Allocator.Error!Message {
        errdefer allocator.free(payload);
        const parsed = std.json.parseFromSlice(std.json.Value, allocator, payload, .{}) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => return .{ .payload = payload, .parsed = null },
        };
        return .{ .payload = payload, .parsed = parsed };
    }

    pub fn deinit(self: *Message, allocator: Allocator) void {
        if (self.parsed) |*parsed| parsed.deinit();
        allocator.free(self.payload);
    }

    /// Whether handling this message makes a check of `uri` that is still
    /// running stale: it edits, reopens, or closes that document, or it ends
    /// the session.
    fn staleChecks(self: *const Message, uri: []const u8) bool {
        const parsed = self.parsed orelse return false;
        if (parsed.value != .object) return false;
        const method = parsed.value.object.get("method") orelse return false;
        if (method != .string) return false;
        if (std.mem.eql(u8, method.string, "shutdown") or std.mem.eql(u8, method.string, "exit")) return true;
        const edits_document = std.mem.eql(u8, method.string, "textDocument/didChange") or
            std.mem.eql(u8, method.string, "textDocument/didOpen") or
            std.mem.eql(u8, method.string, "textDocument/didClose");
        if (!edits_document) return false;
        const params = parsed.value.object.get("params") orelse return false;
        if (params != .object) return false;
        const text_document = params.object.get("textDocument") orelse return false;
        if (text_document != .object) return false;
        const document_uri = text_document.object.get("uri") orelse return false;
        if (document_uri != .string) return false;
        return std.mem.eql(u8, document_uri.string, uri);
    }
};

/// The inbox for a reader task whose reads fail with `ReadError`.
pub fn Inbox(comptime ReadError: type) type {
    return struct {
        const Self = @This();

        /// What the server does next.
        pub const Next = union(enum) {
            message: Message,
            /// No message is pending and the reader is waiting for client input.
            idle,
            /// The client input ended, cleanly when the error is null.
            ended: ?ReadError,
        };

        const Input = enum {
            /// The reader has buffered input or is partway through a message.
            reading,
            /// The reader has consumed all buffered input and is waiting for more.
            waiting,
            ended,
        };

        allocator: Allocator,
        std_io: std.Io,
        mutex: std.Io.Mutex = .init,
        changed: std.Io.Condition = .init,
        messages: std.Deque(Message) = .empty,
        input: Input = .reading,
        end_error: ?ReadError = null,
        /// The document of the cancelable check in progress, borrowed from the
        /// server for the duration of that check.
        checking_uri: ?[]const u8 = null,
        /// Set once the check in progress is stale.
        cancel_request: std.atomic.Value(bool) = .init(false),

        pub fn init(allocator: Allocator, std_io: std.Io) Self {
            return .{ .allocator = allocator, .std_io = std_io };
        }

        /// Free the messages the server never handled.
        pub fn deinit(self: *Self) void {
            while (self.messages.popFront()) |message| {
                var owned = message;
                owned.deinit(self.allocator);
            }
            self.messages.deinit(self.allocator);
        }

        /// Reader task: all buffered input is consumed and the next read waits
        /// for the client.
        pub fn noteWaiting(self: *Self) void {
            self.mutex.lockUncancelable(self.std_io);
            defer self.mutex.unlock(self.std_io);
            self.input = .waiting;
            self.changed.signal(self.std_io);
        }

        /// Reader task: hand over a received message.
        pub fn push(self: *Self, message: Message) Allocator.Error!void {
            self.mutex.lockUncancelable(self.std_io);
            defer self.mutex.unlock(self.std_io);
            try self.messages.pushBack(self.allocator, message);
            self.input = .reading;
            if (self.checking_uri) |uri| {
                if (message.staleChecks(uri)) self.cancel_request.store(true, .release);
            }
            self.changed.signal(self.std_io);
        }

        /// Reader task: the client input ended, cleanly when `err` is null.
        pub fn finish(self: *Self, err: ?ReadError) void {
            self.mutex.lockUncancelable(self.std_io);
            defer self.mutex.unlock(self.std_io);
            self.input = .ended;
            self.end_error = err;
            self.changed.signal(self.std_io);
        }

        /// Server: wait for the next message, or for the reader to be idle
        /// when `has_scheduled_checks`, or for the input to end.
        pub fn next(self: *Self, has_scheduled_checks: bool) Next {
            self.mutex.lockUncancelable(self.std_io);
            defer self.mutex.unlock(self.std_io);
            while (true) {
                if (self.messages.popFront()) |message| return .{ .message = message };
                switch (self.input) {
                    .reading => {},
                    .waiting => if (has_scheduled_checks) return .idle,
                    .ended => return .{ .ended = self.end_error },
                }
                self.changed.waitUncancelable(self.std_io, &self.mutex);
            }
        }

        /// Server: start a cancelable check of `uri`, unless input arrived
        /// since `next` reported the reader idle. `uri` must stay valid until
        /// `endCheck`.
        pub fn beginCheck(self: *Self, uri: []const u8) bool {
            self.mutex.lockUncancelable(self.std_io);
            defer self.mutex.unlock(self.std_io);
            if (self.messages.len != 0 or self.input == .reading) return false;
            self.checking_uri = uri;
            self.cancel_request.store(false, .release);
            return true;
        }

        /// Server: the check started by `beginCheck` returned.
        pub fn endCheck(self: *Self) void {
            self.mutex.lockUncancelable(self.std_io);
            defer self.mutex.unlock(self.std_io);
            self.checking_uri = null;
        }
    };
}
