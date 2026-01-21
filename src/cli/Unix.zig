const std = @import("std");

const Unix = @This();

old_termios: std.posix.termios,

pub const Error = std.posix.TermiosGetError || std.posix.TermiosSetError;

pub fn init() Error!Unix {
    const stdin_handle = std.fs.File.stdin().handle;
    const old_termios: std.posix.termios = try std.posix.tcgetattr(stdin_handle);

    var new_termios = old_termios;
    new_termios.lflag.ICANON = false;
    new_termios.lflag.ECHO = false;

    new_termios.cc[@intFromEnum(std.posix.V.INTR)] = 0;

    try std.posix.tcsetattr(stdin_handle, .NOW, new_termios);

    return Unix{ .old_termios = old_termios };
}

pub fn deinit(unix: Unix) void {
    const stdin_handle = std.fs.File.stdin().handle;

    std.posix.tcsetattr(stdin_handle, .NOW, unix.old_termios) catch unreachable;
}
