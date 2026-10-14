//! Root metadata owned by the LIR boundary.
//!
//! Checking outputs root requests. Post-check lowering converts those checked
//! requests into this small LIR-facing record when it emits root procedures.

const std = @import("std");
const check = @import("check");

const checked = check.CheckedArtifact;

/// Source role for a root procedure emitted into LIR.
pub const RootKind = checked.RootRequestKind;

/// Calling convention category for a root procedure.
pub const RootAbi = checked.RootAbi;

/// Visibility category for a root procedure.
pub const RootExposure = checked.RootExposure;

/// The checked root request metadata a root procedure carries into LIR.
pub const RootMetadata = struct {
    pub const TestPlanMetadata = struct {
        result_index: u32,
        module_index: u32,
        root_index: u32,
    };

    order: u32,
    kind: RootKind,
    abi: RootAbi,
    exposure: RootExposure,
    test_plan: ?TestPlanMetadata = null,

    pub fn fromCheckedRoot(root: checked.RootRequest) RootMetadata {
        return .{ .order = root.order, .kind = root.kind, .abi = root.abi, .exposure = root.exposure };
    }
};

test "root metadata declarations are referenced" {
    std.testing.refAllDecls(@This());
}
