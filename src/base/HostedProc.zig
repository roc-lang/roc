const Ident = @import("Ident.zig");

pub const HostedProc = struct {
    symbol_name: Ident.Idx,
    index: u32,
};
