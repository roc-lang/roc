//! Root of the one test binary that holds every Zig test suite linking the
//! whole compiler. Each import is the root of one suite that build.zig
//! registers with its namespace (the import's path, with `.` for `/` and
//! without `.zig`, or the suite's directory), and each suite's runs pass
//! `--roc-test-suite=<namespace>` so they execute only that suite's tests.

test {
    _ = @import("snapshot_tool/main.zig");
    _ = @import("eval/test/builtin_doc_tests.zig");
    _ = @import("eval/test/checked_root_graph_test.zig");
    _ = @import("eval/test/lir_inline_test.zig");
    _ = @import("eval/test/rc_conformance_tests.zig");
    _ = @import("eval/test/trmc_lir_test.zig");
    _ = @import("cli/main.zig");
}
