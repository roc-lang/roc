//! Shared data boundary for producer-certified literal completion.
//!
//! Root evaluation identity is distinct from owning specialization identity.
//! Portable success certificates bind both to the compiled artifact that owns
//! the already-materialized value. They contain no host pointers or local
//! layout/root ids, and confer no authority to replay old source coordinates.

const std = @import("std");
const check = @import("check");
const LIR = @import("LIR.zig");

const checked = check.CheckedArtifact;

pub const SourceExprId = struct {
    module: checked.ModuleId,
    expr: checked.CheckedExprId,

    pub fn eql(self: SourceExprId, other: SourceExprId) bool {
        return checked.ModuleId.eql(self.module, other.module) and self.expr == other.expr;
    }
};

pub const RootIdentity = struct {
    source: SourceExprId,
    procedure: [32]u8,

    pub fn eql(self: RootIdentity, other: RootIdentity) bool {
        return self.source.eql(other.source) and std.mem.eql(u8, &self.procedure, &other.procedure);
    }
};

/// Activated-root ownership after lifting resolved the explicit owning FnId.
/// Null is explicit ineligibility, not permission to reconstruct a key later.
pub const RootOwner = struct {
    root: LIR.LiteralRootId,
    owner_spec_key: ?[32]u8,
    /// Exact lifted function index in this live producer, never persisted.
    owner_fn: ?OwnerFunctionId = null,
    /// Filled by the consumer of the finalized immutable lifted function table.
    owner_scope: ?*const anyopaque = null,
};

pub const OwnerFunctionId = enum(u32) { _ };

/// Additional native code demand for one proven early literal owner.
/// This is not a user/evaluation root or a reservation-time cache offer.
pub const PublicationRequest = struct {
    owner_fn: OwnerFunctionId,
    owner_scope: *const anyopaque,
    specialization_key: [32]u8,
};

pub const OwnerId = enum(u32) { _ };

/// Transient provenance published at an actual finalized runtime LIR read.
/// Publication resolves the local root through its live ProgramSession before
/// writing a stable certificate. Inlining/cloning determines the emitter.
pub const RuntimeUse = struct {
    root: LIR.LiteralRootId,
    emitter: LIR.ProcIdentity,
    owner: ?OwnerId = null,
};

pub const ReportAuthority = enum {
    /// No checked root retained this failure in the producing program.
    specialization,
    /// An embedding checked root owns reporting in the producing program.
    /// A consumer must establish its own authority; this is not a replay veto.
    checked_root,
};

pub const Outcome = union(enum) {
    success,
    rejected: struct {
        source: SourceExprId,
        kind: LIR.LiteralRejectionKind,
        message: []const u8,
        producer_report_authority: ReportAuthority,
    },
    checked_failure,
    literal_failure: RootIdentity,
    unsupported_failure,
};

/// Session-owned messages and observation flags are produced by finalization.
pub const Record = struct {
    root: RootIdentity,
    specialization_key: ?[32]u8 = null,
    /// Transient association with the shared prepared program's function table.
    owner_fn: ?OwnerFunctionId = null,
    owner_scope: ?*const anyopaque = null,
    outcome: Outcome,
    has_debug_observation: bool = false,
    has_expect_observation: bool = false,
    /// Null is an incomplete producer proof, never an assumption of no callable.
    has_callable_result: ?bool = null,

    /// This projection does not establish artifact portability or completeness.
    /// Those are separate producer proofs required before pack publication.
    pub fn portableSuccess(self: Record) ?PortableSuccess {
        const owner = self.specialization_key orelse return null;
        if (self.outcome != .success or self.has_debug_observation or self.has_expect_observation or self.has_callable_result != false) return null;
        return .{ .root = self.root, .owner_specialization_key = owner };
    }
};

pub const DebugEvent = struct {
    root: RootIdentity,
    message: []const u8,
};

/// Initial persisted class: producer-proven noncallable, observation-free success.
/// The version-6 certificate marker asserts this class; a session Record with
/// an absent representation proof cannot project into it.
pub const PortableSuccess = struct {
    root: RootIdentity,
    owner_specialization_key: [32]u8,

    /// Checked identity bytes remain authoritative after decoding; derivation
    /// fields on an in-session ModuleId are not part of persisted identity.
    pub fn eql(self: PortableSuccess, other: PortableSuccess) bool {
        return self.root.eql(other.root) and std.mem.eql(u8, &self.owner_specialization_key, &other.owner_specialization_key);
    }
};

/// Every contributing literal in an offered artifact closure must be certified.
/// The artifact carries the typed value's code/data/relocations; this certificate
/// proves its finalization, not a replacement representation of that value.
pub const Certificate = struct {
    specialization_key: [32]u8,
    artifact_identity: [32]u8,
    roots: []const PortableSuccess,
};

pub const ExternalCertificate = struct {
    emitter: LIR.ProcIdentity,
    certificate: *const Certificate,
};

test "finalized literal portability needs exact ownership and observation-free success" {
    const source: SourceExprId = .{ .module = .{ .bytes = [_]u8{7} ** 32 }, .expr = @enumFromInt(2) };
    var record: Record = .{
        .root = .{ .source = source, .procedure = [_]u8{8} ** 32 },
        .outcome = .success,
    };
    try std.testing.expect(record.portableSuccess() == null);
    const owner = [_]u8{9} ** 32;
    record.specialization_key = owner;
    try std.testing.expect(record.portableSuccess() == null);
    record.has_callable_result = true;
    try std.testing.expect(record.portableSuccess() == null);
    record.has_callable_result = false;
    try std.testing.expectEqualDeep(owner, record.portableSuccess().?.owner_specialization_key);
    record.has_debug_observation = true;
    try std.testing.expect(record.portableSuccess() == null);
    record.has_debug_observation = false;
    record.has_expect_observation = true;
    try std.testing.expect(record.portableSuccess() == null);
    record.has_expect_observation = false;
    record.outcome = .{ .rejected = .{
        .source = source,
        .kind = .quote,
        .message = "rejected",
        .producer_report_authority = .specialization,
    } };
    try std.testing.expect(record.portableSuccess() == null);
    record.outcome = .checked_failure;
    try std.testing.expect(record.portableSuccess() == null);
    record.outcome = .{ .literal_failure = record.root };
    try std.testing.expect(record.portableSuccess() == null);
    record.outcome = .unsupported_failure;
    try std.testing.expect(record.portableSuccess() == null);
}
