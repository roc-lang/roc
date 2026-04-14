//! Test aggregator that imports all LSP test modules.

comptime {
@import("test/transport_test.zig");
@import("test/server_test.zig");
@import("test/protocol_test.zig");
@import("test/document_store_test.zig");
}
