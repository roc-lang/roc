//! The dylib platform's hosted functions, built as their own archive member.
//!
//! Nothing else in the host references these symbols: they are reachable only
//! through the app's own (weak) references to `roc_host_double`, which makes
//! this archive the shape that a multi-object host such as a Go c-archive has.
//! LLD's COFF symbol table drops a lazy archive entry when a weak reference
//! reaches it before a strong one, so without `roc build` rooting the hosted
//! symbols it references, this member would never be extracted and the app
//! would call through null. Keep the hosted functions here, and keep this
//! member free of anything the host's other member needs.

const std = @import("std");

comptime {
    @export(&hostedHostDouble, .{ .name = "roc_host_double", .visibility = .hidden });
}

/// Host.double!: double a number in the host. I64 -> I64 involves no
/// refcounted values, so under the hosted C ABI it takes no parameters
/// beyond its arguments.
fn hostedHostDouble(n: i64) callconv(.c) i64 {
    return @call(.never_inline, sharedPrivateHelper, .{n}) * 2;
}

// --- Dead-code-elimination canaries
// The dead hosted function owns one private constant, calls one dead-only
// private helper with its own constant, and also calls a shared private helper
// used by live Host.double!. Final-link section GC must drop the dead-only
// data while keeping the shared helper/data alive.
fn canaryBlob(comptime marker: []const u8) [4096]u8 {
    @setEvalBranchQuota(20000);
    var blob: [4096]u8 = undefined;
    var i: usize = 0;
    while (i < blob.len) : (i += 1) {
        blob[i] = marker[i % marker.len];
    }
    return blob;
}

const dead_hosted_canary_blob = canaryBlob("ROC_DCE_CANARY_BLOB_7f3a9c");
const dead_helper_canary_blob = canaryBlob("ROC_DCE_DEAD_HELPER_BLOB_28d0aa");
const shared_canary_blob = canaryBlob("ROC_DCE_SHARED_BLOB_93e2c1");

fn sharedPrivateHelper(n: i64) i64 {
    std.mem.doNotOptimizeAway(&shared_canary_blob);
    return n;
}

fn deadOnlyPrivateHelper(n: i64) i64 {
    std.mem.doNotOptimizeAway(&dead_helper_canary_blob);
    return n + 1;
}

fn hostUnusedNicheFeature(n: i64) callconv(.c) i64 {
    std.mem.doNotOptimizeAway(&dead_hosted_canary_blob);
    const dead_value = @call(.never_inline, deadOnlyPrivateHelper, .{n});
    return @call(.never_inline, sharedPrivateHelper, .{dead_value});
}

comptime {
    @export(&hostUnusedNicheFeature, .{ .name = "roc_host_unused_niche_feature", .visibility = .hidden });
}
