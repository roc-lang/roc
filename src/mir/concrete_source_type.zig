//! Run-local concrete source type payload registry.
//!
//! `CanonicalTypeKey` is only an identity key. Any stage that constructs a
//! concrete mono/lambda/executable request must retain an explicit payload ref so
//! later lowering can clone or inspect the checked type graph without
//! reconstructing it from the key.

const std = @import("std");
const check = @import("check");

const Allocator = std.mem.Allocator;
const checked_artifact = check.CheckedArtifact;
const canonical = check.CanonicalNames;

pub const ConcreteSourceTypeRef = enum(u32) { _ };

pub const ConcreteSourceTypeRoot = struct {
    key: canonical.CanonicalTypeKey,
    source: checked_artifact.ArtifactCheckedTypeRef,
};

pub const Store = struct {
    allocator: Allocator,
    roots: std.ArrayList(ConcreteSourceTypeRoot),
    by_key: std.StringHashMap(ConcreteSourceTypeRef),

    pub fn init(allocator: Allocator) Store {
        return .{
            .allocator = allocator,
            .roots = .empty,
            .by_key = std.StringHashMap(ConcreteSourceTypeRef).init(allocator),
        };
    }

    pub fn deinit(self: *Store) void {
        var keys = self.by_key.keyIterator();
        while (keys.next()) |key| self.allocator.free(key.*);
        self.by_key.deinit();
        self.roots.deinit(self.allocator);
        self.* = Store.init(self.allocator);
    }

    pub fn registerArtifactRoot(
        self: *Store,
        artifact: checked_artifact.CheckedModuleArtifactKey,
        checked_types: checked_artifact.CheckedTypeStoreView,
        root: checked_artifact.CheckedTypeId,
    ) Allocator.Error!ConcreteSourceTypeRef {
        const raw = @intFromEnum(root);
        if (raw >= checked_types.roots.len) {
            invariantViolation("concrete source type store received a checked type id outside the artifact root table");
        }

        const key = checked_types.roots[raw].key;
        if (self.by_key.get(key.bytes[0..])) |existing| {
            const existing_root = self.root(existing);
            if (!std.mem.eql(u8, existing_root.key.bytes[0..], key.bytes[0..])) {
                invariantViolation("concrete source type store key map returned a non-equivalent payload");
            }
            return existing;
        }

        const id: ConcreteSourceTypeRef = @enumFromInt(@as(u32, @intCast(self.roots.items.len)));
        const owned_key = try self.allocator.dupe(u8, key.bytes[0..]);
        errdefer self.allocator.free(owned_key);

        try self.roots.append(self.allocator, .{
            .key = key,
            .source = .{
                .artifact = artifact,
                .ty = root,
            },
        });
        try self.by_key.put(owned_key, id);
        return id;
    }

    pub fn root(self: *const Store, ref: ConcreteSourceTypeRef) ConcreteSourceTypeRoot {
        return self.roots.items[@intFromEnum(ref)];
    }

    pub fn key(self: *const Store, ref: ConcreteSourceTypeRef) canonical.CanonicalTypeKey {
        return self.root(ref).key;
    }
};

fn invariantViolation(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) {
        std.debug.panic(message, .{});
    }
    unreachable;
}

test "concrete source type store declarations are referenced" {
    std.testing.refAllDecls(@This());
}
