//! Shared compile-time and load-time checks for relocatable serialized data.

/// Maps a field name in an owning type to its serialized field name.
pub const FieldRename = struct {
    owner: []const u8,
    serialized: []const u8,
};

fn comptimeStrEq(comptime a: []const u8, comptime b: []const u8) bool {
    if (a.len != b.len) return false;
    for (a, b) |x, y| {
        if (x != y) return false;
    }
    return true;
}

fn containsName(comptime names: []const []const u8, comptime name: []const u8) bool {
    for (names) |candidate| {
        if (comptimeStrEq(candidate, name)) return true;
    }
    return false;
}

fn renamedOwnerField(comptime renames: []const FieldRename, comptime serialized: []const u8) ?[]const u8 {
    for (renames) |rename| {
        if (comptimeStrEq(rename.serialized, serialized)) return rename.owner;
    }
    return null;
}

fn renamedSerializedField(comptime renames: []const FieldRename, comptime owner: []const u8) ?[]const u8 {
    for (renames) |rename| {
        if (comptimeStrEq(rename.owner, owner)) return rename.serialized;
    }
    return null;
}

/// Verifies that serialized fields and owner fields match, accounting for
/// explicitly excluded fields and explicit renames.
pub fn assertBidirectionalFieldSet(
    comptime Owner: type,
    comptime Serialized: type,
    comptime owner_only_fields: []const []const u8,
    comptime serialized_only_fields: []const []const u8,
    comptime renames: []const FieldRename,
) void {
    comptime {
        if (@typeInfo(Owner) != .@"struct") {
            @compileError("field-set audit owner must be a struct: " ++ @typeName(Owner));
        }
        if (@typeInfo(Serialized) != .@"struct") {
            @compileError("field-set audit serialized type must be a struct: " ++ @typeName(Serialized));
        }

        for (renames) |rename| {
            if (!@hasField(Owner, rename.owner)) {
                @compileError("field-set audit rename owner field '" ++ rename.owner ++
                    "' does not exist in " ++ @typeName(Owner));
            }
            if (!@hasField(Serialized, rename.serialized)) {
                @compileError("field-set audit rename serialized field '" ++ rename.serialized ++
                    "' does not exist in " ++ @typeName(Serialized));
            }
        }

        for (@typeInfo(Serialized).@"struct".fields) |field| {
            if (@hasField(Owner, field.name)) continue;
            if (containsName(serialized_only_fields, field.name)) continue;
            if (renamedOwnerField(renames, field.name) != null) continue;
            @compileError("field-set audit: serialized field '" ++ field.name ++
                "' has no owner field in " ++ @typeName(Owner));
        }

        for (@typeInfo(Owner).@"struct".fields) |field| {
            if (@hasField(Serialized, field.name)) continue;
            if (containsName(owner_only_fields, field.name)) continue;
            if (renamedSerializedField(renames, field.name) != null) continue;
            @compileError("field-set audit: owner field '" ++ field.name ++
                "' is neither serialized nor explicitly owner-only in " ++ @typeName(Owner));
        }
    }
}

fn comptimeHasRelocationMarker(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        .@"struct" => |s| blk: {
            if (@hasDecl(T, "serialized_relocatable_pointers")) break :blk true;
            inline for (s.fields) |field| {
                if (comptimeHasRelocationMarker(field.type)) break :blk true;
            }
            break :blk false;
        },
        .@"union" => |u| blk: {
            inline for (u.fields) |field| {
                if (comptimeHasRelocationMarker(field.type)) break :blk true;
            }
            break :blk false;
        },
        .array => |a| comptimeHasRelocationMarker(a.child),
        .optional => |o| comptimeHasRelocationMarker(o.child),
        else => false,
    };
}

/// Compile-time guard that every field of a serialized type is either a
/// recognized relocatable marker or a relocation-invariant POD leaf/aggregate.
/// A raw pointer/slice embedded directly in a serialized struct would dangle
/// after relocation because there is no marker-specific fixup to validate or
/// apply, so reject those shapes at comptime.
pub fn assertSerializedRelocatable(comptime T: type) void {
    comptime {
        @setEvalBranchQuota(20_000_000);
        switch (@typeInfo(T)) {
            .@"struct" => |s| {
                if (@hasDecl(T, "serialized_relocatable_pointers")) return;
                for (s.fields) |field| assertSerializedRelocatable(field.type);
            },
            .@"union" => |u| {
                for (u.fields) |field| assertSerializedRelocatable(field.type);
            },
            .array => |a| assertSerializedRelocatable(a.child),
            .optional => |o| assertSerializedRelocatable(o.child),
            .int, .float, .bool, .void, .@"enum", .error_set, .vector => {},
            .pointer => @compileError("Serialized type '" ++ @typeName(T) ++
                "' embeds a pointer/slice outside a relocatable marker; it would dangle after relocation. Wrap it in a serialized relocation marker."),
            else => @compileError("Serialized type '" ++ @typeName(T) ++
                "' has a field with an unsupported serialized representation: " ++ @tagName(@typeInfo(T))),
        }
    }
}

/// Validates every relocatable marker reachable from a serialized value.
pub fn validateSerializedRelocations(comptime T: type, self: *const T, backing_len: u64) error{CorruptArtifact}!void {
    switch (@typeInfo(T)) {
        .@"struct" => |s| {
            if (@hasDecl(T, "serialized_relocatable_pointers")) {
                try self.validateRelocations(backing_len);
                return;
            }
            if (s.layout == .@"packed") {
                if (comptime comptimeHasRelocationMarker(T)) {
                    @compileError("serialized packed struct '" ++ @typeName(T) ++
                        "' contains a relocatable marker; validation cannot address packed fields safely");
                }
                return;
            }
            inline for (s.fields) |field| {
                try validateSerializedRelocations(field.type, &@field(self, field.name), backing_len);
            }
        },
        .array => |a| {
            for (self) |*elem| try validateSerializedRelocations(a.child, elem, backing_len);
        },
        .optional => |o| {
            if (self.*) |*payload| try validateSerializedRelocations(o.child, payload, backing_len);
        },
        .@"union" => {
            if (comptime comptimeHasRelocationMarker(T)) {
                @compileError("serialized union '" ++ @typeName(T) ++
                    "' contains a relocatable marker; validation cannot choose an active variant without a tag");
            }
        },
        else => {},
    }
}
