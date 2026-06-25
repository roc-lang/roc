//! Resolve "localhost" and verify the result is a loopback address.
//!
//! Security: We must resolve "localhost" and verify it points to a loopback address.
//! This prevents attacks where:
//! 1. An attacker modifies /etc/hosts to make localhost resolve to their server
//! 2. A compromised DNS makes localhost resolve to an external IP
//! 3. Container/VM networking misconfiguration exposes localhost to external IPs
//!
//! We're being intentionally strict here:
//! - For IPv4: We only accept exactly 127.0.0.1 (not the full 127.0.0.0/8 range)
//! - For IPv6: We only accept exactly ::1 (not other loopback addresses)
//!
//! std.c.addrinfo / std.c.getaddrinfo dropped Windows support in Zig 0.16 (the
//! `ws2_32.addrinfo` field was removed). On Windows we call ws2_32 directly with
//! a locally-declared addrinfo struct (which has a different field order than
//! the POSIX one).

const std = @import("std");
const builtin = @import("builtin");

/// IPv4 loopback address 127.0.0.1 in network byte order (big-endian).
pub const IPV4_LOOPBACK_BE: u32 = 0x7F000001;
/// IPv4 loopback address 127.0.0.1 as stored on little-endian hosts.
pub const IPV4_LOOPBACK_LE: u32 = 0x0100007F;

/// Address family of a successfully verified loopback resolution.
pub const ResolvedFamily = enum { ip4, ip6 };

/// Errors returned by `resolveLoopback` / `requireLoopback`.
pub const Error = error{
    LocalhostWasNotLoopback,
    NetworkError,
};

/// Resolve "localhost" and verify the result is a loopback address.
/// Returns which address family matched, or an error.
pub fn resolveLoopback() Error!ResolvedFamily {
    if (builtin.os.tag == .windows) {
        const ws2 = struct {
            // Windows addrinfoa layout: ai_canonname precedes ai_addr. Different
            // from POSIX (which swaps them).
            const ADDRINFOA = extern struct {
                ai_flags: i32,
                ai_family: i32,
                ai_socktype: i32,
                ai_protocol: i32,
                ai_addrlen: usize,
                ai_canonname: ?[*:0]u8,
                ai_addr: ?*anyopaque,
                ai_next: ?*ADDRINFOA,
            };
            extern "ws2_32" fn getaddrinfo(
                pNodeName: ?[*:0]const u8,
                pServiceName: ?[*:0]const u8,
                pHints: ?*const ADDRINFOA,
                ppResult: *?*ADDRINFOA,
            ) callconv(.winapi) i32;
            extern "ws2_32" fn freeaddrinfo(pAddrInfo: ?*ADDRINFOA) callconv(.winapi) void;
        };
        const AF_INET: i32 = 2;
        const AF_INET6: i32 = 23; // Windows

        var result: ?*ws2.ADDRINFOA = null;
        if (ws2.getaddrinfo("localhost", null, null, &result) != 0) return error.NetworkError;
        defer ws2.freeaddrinfo(result);
        if (result == null) return error.LocalhostWasNotLoopback;

        var addr_it = result;
        while (addr_it) |addr| : (addr_it = addr.ai_next) {
            if (addr.ai_family == AF_INET) {
                const sa: *const std.posix.sockaddr.in = @ptrCast(@alignCast(addr.ai_addr.?));
                if (sa.addr == IPV4_LOOPBACK_BE or sa.addr == IPV4_LOOPBACK_LE) return .ip4;
            } else if (addr.ai_family == AF_INET6) {
                const sa6: *const std.posix.sockaddr.in6 = @ptrCast(@alignCast(addr.ai_addr.?));
                if (isIp6Loopback(sa6.addr)) return .ip6;
            }
        }
        return error.LocalhostWasNotLoopback;
    } else {
        const hints: std.c.addrinfo = .{
            .flags = @bitCast(@as(u32, 0)),
            .family = std.posix.AF.UNSPEC,
            .socktype = 0,
            .protocol = 0,
            .addrlen = 0,
            .addr = null,
            .canonname = null,
            .next = null,
        };
        var result: ?*std.c.addrinfo = null;
        if (@intFromEnum(std.c.getaddrinfo("localhost", null, &hints, &result)) != 0) {
            return error.NetworkError;
        }
        defer if (result) |r| std.c.freeaddrinfo(r);
        if (result == null) return error.LocalhostWasNotLoopback;

        var addr_it = result;
        while (addr_it) |addr| : (addr_it = addr.next) {
            if (addr.family == std.posix.AF.INET) {
                const sa: *const std.posix.sockaddr.in = @ptrCast(@alignCast(addr.addr));
                if (sa.addr == IPV4_LOOPBACK_BE or sa.addr == IPV4_LOOPBACK_LE) return .ip4;
            } else if (addr.family == std.posix.AF.INET6) {
                const sa6: *const std.posix.sockaddr.in6 = @ptrCast(@alignCast(addr.addr));
                if (isIp6Loopback(sa6.addr)) return .ip6;
            }
        }
        return error.LocalhostWasNotLoopback;
    }
}

/// Resolve "localhost" and require at least one loopback result.
pub fn requireLoopback() Error!void {
    _ = try resolveLoopback();
}

fn isIp6Loopback(addr: [16]u8) bool {
    for (addr[0..15]) |byte| {
        if (byte != 0) return false;
    }
    return addr[15] == 1;
}
