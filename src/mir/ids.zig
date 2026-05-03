//! Shared MIR-family identifiers.

const std = @import("std");
const check = @import("check");

const canonical = check.CanonicalNames;
const checked_artifact = check.CheckedArtifact;

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

pub const RootAbi = enum {
    roc,
    platform,
    hosted,
    test_expect,
    compile_time,
};

pub const RootExposure = enum {
    private,
    exported,
    platform_required,
    hosted,
};

pub const RootMetadata = struct {
    order: u32,
    kind: RootKind,
    abi: RootAbi,
    exposure: RootExposure,
};

pub const ExecutableSyntheticProcBody = union(enum) {
    erased_promoted_wrapper: checked_artifact.ErasedPromotedWrapperBodyPlan,
};

pub const ExecutableSyntheticProc = struct {
    source_proc: canonical.MirProcedureRef,
    template: canonical.ProcedureTemplateRef,
    executable_type_payloads: *const checked_artifact.ExecutableTypePayloadStore,
    executable_payload_transforms: *const checked_artifact.ExecutablePayloadTransformPlanStore,
    comptime_plans: *const checked_artifact.CompileTimePlanStore,
    body: ExecutableSyntheticProcBody,
};

pub const RecordShapeId = enum(u32) { _ };
pub const RecordFieldId = enum(u32) { _ };
pub const TagUnionShapeId = enum(u32) { _ };
pub const TagId = enum(u32) { _ };
pub const TagPayloadId = enum(u32) { _ };
pub const ProgramLiteralId = enum(u32) { _ };

pub const ProgramLiteral = struct {
    bytes: []const u8,
};

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
