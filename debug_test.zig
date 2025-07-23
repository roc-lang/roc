const std = @import("std");
const compile = @import("src/compile/mod.zig");

pub fn main() !void {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();
    
    std.debug.print("Creating ModuleEnv...\n", .{});
    var env = try compile.ModuleEnv.init(gpa, "0x1");
    defer env.deinit();
    std.debug.print("ModuleEnv created successfully!\n", .{});
    
    std.debug.print("Initializing CIR fields...\n", .{});
    try env.initCIRFields(gpa, "Test");
    std.debug.print("CIR fields initialized successfully!\n", .{});
}