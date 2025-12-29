//! Test aggregator that imports all LSP test modules.

comptime {
    _ = @import("test/transport_test.zig");
    _ = @import("test/server_test.zig");
    _ = @import("test/protocol_test.zig");
    _ = @import("test/document_store_test.zig");
}
