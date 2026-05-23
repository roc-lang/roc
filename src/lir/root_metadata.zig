//! Root metadata owned by the LIR boundary.
//!
//! Checking outputs root requests. Post-check lowering converts those checked
//! requests into this small LIR-facing record when it emits root procedures.

const std = @import("std");
const check = @import("check");

const checked = check.CheckedArtifact;

/// Source role for a root procedure emitted into LIR.
pub const RootKind = enum {
    runtime_entrypoint,
    provided_export,
    platform_required_binding,
    hosted_export,
    test_expect,
    repl_expr,
    dev_expr,
    compile_time_constant,
    compile_time_callable,
};

/// Calling convention category for a root procedure.
pub const RootAbi = enum {
    roc,
    platform,
    hosted,
    test_expect,
    compile_time,
};

/// Visibility category for a root procedure.
pub const RootExposure = enum {
    private,
    exported,
    platform_required,
    hosted,
};

/// LIR-owned copy of checked root request metadata.
pub const RootMetadata = struct {
    order: u32,
    kind: RootKind,
    abi: RootAbi,
    exposure: RootExposure,

    pub fn fromCheckedRoot(root: checked.RootRequest) RootMetadata {
        return .{
            .order = root.order,
            .kind = switch (root.kind) {
                .runtime_entrypoint => .runtime_entrypoint,
                .provided_export => .provided_export,
                .platform_required_binding => .platform_required_binding,
                .hosted_export => .hosted_export,
                .test_expect => .test_expect,
                .repl_expr => .repl_expr,
                .dev_expr => .dev_expr,
                .compile_time_constant => .compile_time_constant,
                .compile_time_callable => .compile_time_callable,
            },
            .abi = switch (root.abi) {
                .roc => .roc,
                .platform => .platform,
                .hosted => .hosted,
                .test_expect => .test_expect,
                .compile_time => .compile_time,
            },
            .exposure = switch (root.exposure) {
                .private => .private,
                .exported => .exported,
                .platform_required => .platform_required,
                .hosted => .hosted,
            },
        };
    }
};

test "root metadata declarations are referenced" {
    std.testing.refAllDecls(@This());
}
