//! Shared MIR-family identifiers.

const std = @import("std");
const check = @import("check");

const canonical = check.CanonicalNames;
const checked_artifact = check.CheckedArtifact;

/// Public `RootKind` declaration.
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

/// Public `RootAbi` declaration.
pub const RootAbi = enum {
    roc,
    platform,
    hosted,
    test_expect,
    compile_time,
};

/// Public `RootExposure` declaration.
pub const RootExposure = enum {
    private,
    exported,
    platform_required,
    hosted,
};

/// Public `RootMetadata` declaration.
pub const RootMetadata = struct {
    order: u32,
    kind: RootKind,
    abi: RootAbi,
    exposure: RootExposure,
};

/// Public `ExecutableSyntheticProcBody` declaration.
pub const ExecutableSyntheticProcBody = union(enum) {
    erased_promoted_wrapper: checked_artifact.ErasedPromotedWrapperBodyPlan,
};

/// Public `ExecutableSyntheticProcSignaturePlan` declaration.
pub const ExecutableSyntheticProcSignaturePlan = struct {
    source_fn_ty: canonical.CanonicalTypeKey,
    params: []const checked_artifact.PromotedWrapperParam = &.{},
    ret_source_ty: canonical.CanonicalTypeKey,
};

/// Public `ExecutableSyntheticProc` declaration.
pub const ExecutableSyntheticProc = struct {
    artifact: checked_artifact.CheckedModuleArtifactKey,
    source_proc: canonical.MirProcedureRef,
    template: canonical.ProcedureTemplateRef,
    signature: ExecutableSyntheticProcSignaturePlan,
    executable_type_payloads: *const checked_artifact.ExecutableTypePayloadStore,
    executable_value_transforms: *const checked_artifact.ExecutableValueTransformPlanStore,
    comptime_plans: *const checked_artifact.CompileTimePlanStore,
    comptime_values: *const checked_artifact.CompileTimeValueStore,
    body: ExecutableSyntheticProcBody,
};

/// Explicit executable target required by a generated procedure value.
pub const ProcValueExecutableTarget = struct {
    key: canonical.ExecutableSpecializationKey,
    artifact: checked_artifact.CheckedModuleArtifactKey,
    payloads: *const checked_artifact.ExecutableTypePayloadStore,
    promoted_wrapper: ?canonical.MirProcedureRef = null,
};

/// Public `RecordShapeId` declaration.
pub const RecordShapeId = enum(u32) { _ };
/// Public `RecordFieldId` declaration.
pub const RecordFieldId = enum(u32) { _ };
/// Public `TagUnionShapeId` declaration.
pub const TagUnionShapeId = enum(u32) { _ };
/// Public `TagId` declaration.
pub const TagId = enum(u32) { _ };
/// Public `TagPayloadId` declaration.
pub const TagPayloadId = enum(u32) { _ };
/// Public `ProgramLiteralId` declaration.
pub const ProgramLiteralId = enum(u32) { _ };

/// Public `ProgramLiteral` declaration.
pub const ProgramLiteral = struct {
    bytes: []const u8,
};

/// Public `ProgramLiteralPool` declaration.
pub const ProgramLiteralPool = struct {
    allocator: std.mem.Allocator,
    literals: std.ArrayList(ProgramLiteral),
    by_bytes: std.StringHashMapUnmanaged(ProgramLiteralId),

    pub fn init(allocator: std.mem.Allocator) ProgramLiteralPool {
        return .{
            .allocator = allocator,
            .literals = .empty,
            .by_bytes = .{},
        };
    }

    pub fn deinit(self: *ProgramLiteralPool) void {
        self.by_bytes.deinit(self.allocator);
        for (self.literals.items) |literal| {
            self.allocator.free(literal.bytes);
        }
        self.literals.deinit(self.allocator);
        self.* = ProgramLiteralPool.init(self.allocator);
    }

    pub fn intern(self: *ProgramLiteralPool, bytes: []const u8) std.mem.Allocator.Error!ProgramLiteralId {
        if (self.by_bytes.get(bytes)) |existing| return existing;

        const owned = try self.allocator.dupe(u8, bytes);
        errdefer self.allocator.free(owned);

        const id: ProgramLiteralId = @enumFromInt(@as(u32, @intCast(self.literals.items.len)));
        try self.literals.append(self.allocator, .{ .bytes = owned });
        errdefer _ = self.literals.pop();

        try self.by_bytes.put(self.allocator, owned, id);
        return id;
    }

    pub fn get(self: *const ProgramLiteralPool, id: ProgramLiteralId) []const u8 {
        return self.literals.items[@intFromEnum(id)].bytes;
    }

    pub fn count(self: *const ProgramLiteralPool) usize {
        return self.literals.items.len;
    }
};

/// Public `Span` function.
pub fn Span(comptime T: type) type {
    return extern struct {
        start: u32,
        len: u32,

        pub fn empty() @This() {
            return .{ .start = 0, .len = 0 };
        }

        pub fn get(self: @This(), items: []const T) []const T {
            if (self.len == 0) return &.{};
            return items[self.start..][0..self.len];
        }
    };
}

test "mir ids tests" {
    std.testing.refAllDecls(@This());
}
