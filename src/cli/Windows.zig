const std = @import("std");

const windows = @cImport(@cInclude("windows.h"));

output_mode: windows.DWORD,
input_mode: windows.DWORD,

const Windows = @This();

pub const Error = error{ GetConsoleModeFailure, SetConsoleModeFailure };

pub fn init() Error!Windows {
    const h_out = std.fs.File.stdout().handle;
    const h_in = std.fs.File.stdin().handle;

    var output_mode: windows.DWORD = 0;
    var input_mode: windows.DWORD = 0;
    if (0 == windows.GetConsoleMode(h_out, &output_mode)) {
        return error.GetConsoleModeFailure;
    }
    if (0 == windows.GetConsoleMode(h_in, &input_mode)) {
        return error.GetConsoleModeFailure;
    }

    const ConsoleOutputMode = packed struct(windows.DWORD) {
        ENABLE_PROCCESSED_OUTPUT: bool, // 0x0001
        ENABLE_WRAP_AT_EOL_OUTPUT: bool, // 0x0002
        ENABLE_VIRUTAL_TERMINAL_PROCESSING: bool, // 0x0004
        DISABLE_NEWLINE_AUTO_RETURN: bool, // 0x0008
        ENABLE_LVB_GRID_WORLDWIDE: bool, // 0x0010
        _: u27,
    };

    var requested_out_mode: ConsoleOutputMode = @bitCast(output_mode);
    requested_out_mode.ENABLE_VIRUTAL_TERMINAL_PROCESSING = true;
    requested_out_mode.DISABLE_NEWLINE_AUTO_RETURN = true;

    if (0 == windows.SetConsoleMode(h_out, @bitCast(requested_out_mode))) {
        return error.SetConsoleModeFailure;
    }

    const ConsoleInputMode = packed struct(windows.DWORD) {
        ENABLE_PROCESSED_INPUT: bool, // 0x0001
        ENABLE_LINE_INPUT: bool, // 0x0002
        ENABLE_ECHO_INPUT: bool, // 0x0004
        ENABLE_WINDOW_INPUT: bool, // 0x0008
        ENABLE_MOUSE_INPUT: bool, // 0x0010
        ENABLE_INSERT_MODE: bool, // 0x0020
        ENABLE_QUICK_EDIT_MODE: bool, // 0x0040
        ENABLE_EXTENDED_FLAGS: bool, // 0x0080
        ENABLE_AUTO_POSITION: bool, // 0x0100
        ENABLE_VIRTUAL_TERMINAL_INPUT: bool, // 0x0200
        _: u22 = undefined,
    };

    var requested_in_mode: ConsoleInputMode = @bitCast(input_mode);
    requested_in_mode.ENABLE_PROCESSED_INPUT = false;
    requested_in_mode.ENABLE_LINE_INPUT = false;
    requested_in_mode.ENABLE_ECHO_INPUT = false;
    requested_in_mode.ENABLE_MOUSE_INPUT = false;
    requested_in_mode.ENABLE_QUICK_EDIT_MODE = false;
    requested_in_mode.ENABLE_VIRTUAL_TERMINAL_INPUT = true;

    if (0 == windows.SetConsoleMode(h_in, @bitCast(requested_in_mode))) {
        return error.SetConsoleModeFailure;
    }

    return Windows{
        .output_mode = output_mode,
        .input_mode = input_mode,
    };
}

pub fn deinit(state: Windows) void {
    const h_out = std.fs.File.stdout().handle;
    const h_in = std.fs.File.stdin().handle;

    _ = windows.SetConsoleMode(h_out, state.output_mode);
    _ = windows.SetConsoleMode(h_in, state.input_mode);
}
