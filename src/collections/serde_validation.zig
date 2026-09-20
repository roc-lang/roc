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
        .type,
        .void,
        .bool,
        .noreturn,
        .int,
        .float,
        .pointer,
        .comptime_float,
        .comptime_int,
        .undefined,
        .null,
        .error_union,
        .error_set,
        .@"enum",
        .@"fn",
        .@"opaque",
        .frame,
        .@"anyframe",
        .vector,
        .enum_literal,
        => false,
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
            .type,
            .noreturn,
            .comptime_float,
            .comptime_int,
            .undefined,
            .null,
            .error_union,
            .@"fn",
            .@"opaque",
            .frame,
            .@"anyframe",
            .enum_literal,
            => @compileError("Serialized type '" ++ @typeName(T) ++
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
        .type,
        .void,
        .bool,
        .noreturn,
        .int,
        .float,
        .pointer,
        .comptime_float,
        .comptime_int,
        .undefined,
        .null,
        .error_union,
        .error_set,
        .@"enum",
        .@"fn",
        .@"opaque",
        .frame,
        .@"anyframe",
        .vector,
        .enum_literal,
        => {},
    }
}

/// How a type's in-memory bytes can be turned into deterministic serialized bytes.
///
/// Raw-byte serialization copies `@sizeOf(T)` bytes per element, so every one of
/// those bytes must have a defined value that is a function of the logical value
/// alone. Zig only guarantees that for bytes covered by a declared field; anything
/// else (an implicit inter-field gap, the tail of a union variant smaller than the
/// union, a null optional's payload area) holds whatever was in that memory before,
/// which on a real compile run is allocator- and ASLR-dependent garbage.
pub const ByteDetermination = enum {
    /// Every byte of `@sizeOf(T)` belongs to a declared field for every possible
    /// value. The live bytes are already deterministic, so serialization can point
    /// an iovec straight at them: no scratch copy, no scan, no scrub.
    fully_defined,
    /// Some bytes or bits are undefined, but every one of them is identified by the
    /// value itself—an in-band discriminant (a union tag, an optional's null bit) says
    /// which bytes are live, and a scalar's declared width says which of its storage
    /// bits are value bits. `CompactWriter.zeroValuePadding` canonicalizes all of them,
    /// so a scrubbed copy is deterministic.
    scrubbable,
};

/// Classify `T` for raw-byte serialization, or `@compileError` if neither
/// classification holds.
///
/// The third outcome—bytes that are neither defined nor mechanically scrubbable—is
/// rejected at compile time rather than silently serialized. The shapes that land
/// there are exactly the ones that produced non-reproducible compiler output: an
/// `extern struct` with implicit trailing/inter-field padding, and an `extern union`
/// whose variants are smaller than the union (its tail has no discriminant in the
/// value, so nothing can scrub it). Both are fixed the same way: declare the
/// remaining bytes as an explicitly zero-defaulted reserved field.
pub fn byteDetermination(comptime T: type) ByteDetermination {
    return comptime blk: {
        @setEvalBranchQuota(20_000_000);
        if (isFullyDefined(T)) break :blk .fully_defined;
        assertScrubbable(T, @typeName(T));
        break :blk .scrubbable;
    };
}

/// Whether every byte of `@sizeOf(T)` is covered by a declared field, recursively,
/// for every possible value of `T`.
pub fn isFullyDefined(comptime T: type) bool {
    return comptime blk: {
        @setEvalBranchQuota(20_000_000);
        if (@sizeOf(T) == 0) break :blk true;
        break :blk switch (@typeInfo(T)) {
            // A bool occupies a whole byte holding 0 or 1; pointers are all address
            // bytes. Integers, floats, and vectors are defined only when their value
            // bits fill their storage (`u24`, `f80`, and `@Vector(3, u32)` do not).
            .bool, .pointer => true,
            .int, .float, .vector => @bitSizeOf(T) == 8 * @sizeOf(T),
            .@"enum" => |e| isFullyDefined(e.tag_type),
            .array => |a| isFullyDefined(a.child),
            .@"struct" => |s| switch (s.layout) {
                // A packed struct's bits are exactly its fields' bits; it is defined
                // when those bits fill whole bytes.
                .@"packed" => @bitSizeOf(T) == 8 * @sizeOf(T),
                .@"extern", .auto => fieldsCoverEveryByte(T) and allFieldsFullyDefined(T),
            },
            .optional => false,
            .@"union" => |u| u: {
                // A tagged union's discriminant and inactive-variant bytes are not
                // covered by the active field, so it is never "fully defined"—it is
                // scrubbable instead. An untagged union is defined only when every
                // variant covers the whole union.
                if (u.tag_type != null) break :u false;
                for (u.fields) |f| {
                    if (@sizeOf(f.type) != @sizeOf(T)) break :u false;
                    if (!isFullyDefined(f.type)) break :u false;
                }
                break :u true;
            },
            .type,
            .void,
            .noreturn,
            .comptime_float,
            .comptime_int,
            .undefined,
            .null,
            .error_union,
            .error_set,
            .@"fn",
            .@"opaque",
            .frame,
            .@"anyframe",
            .enum_literal,
            => false,
        };
    };
}

fn fieldsCoverEveryByte(comptime T: type) bool {
    return comptime blk: {
        for (byteCoverageMask(T)) |c| {
            if (!c) break :blk false;
        }
        break :blk true;
    };
}

fn byteCoverageMask(comptime T: type) [@sizeOf(T)]bool {
    return comptime blk: {
        var covered = [_]bool{false} ** @sizeOf(T);
        for (@typeInfo(T).@"struct".fields) |field| {
            if (@sizeOf(field.type) == 0) continue;
            const start = @offsetOf(T, field.name);
            for (start..start + @sizeOf(field.type)) |i| covered[i] = true;
        }
        break :blk covered;
    };
}

fn allFieldsFullyDefined(comptime T: type) bool {
    return comptime blk: {
        for (@typeInfo(T).@"struct".fields) |field| {
            if (!isFullyDefined(field.type)) break :blk false;
        }
        break :blk true;
    };
}

/// Report the first uncovered byte range or non-defined member of `T`, so the
/// author sees which declaration to extend rather than just "not portable".
pub fn assertFullyDefined(comptime T: type, comptime path: []const u8) void {
    comptime {
        @setEvalBranchQuota(20_000_000);
        if (@sizeOf(T) == 0) return;
        switch (@typeInfo(T)) {
            .bool, .pointer => return,
            .int, .float, .vector => if (@bitSizeOf(T) != 8 * @sizeOf(T)) @compileError(
                "serialized type '" ++ path ++ "' (" ++ @typeName(T) ++ ") stores " ++
                    digits(@bitSizeOf(T)) ++ " value bits in " ++ digits(8 * @sizeOf(T)) ++
                    " bits of storage; the leftover bits are undefined. Use a width whose value bits fill its storage.",
            ),
            .@"enum" => |e| assertFullyDefined(e.tag_type, path ++ ".<tag>"),
            .array => |a| assertFullyDefined(a.child, path ++ "[]"),
            .@"struct" => |s| {
                if (s.layout == .@"packed") {
                    if (@bitSizeOf(T) != 8 * @sizeOf(T)) @compileError(
                        "serialized packed struct '" ++ path ++ "' (" ++ @typeName(T) ++ ") declares " ++
                            digits(@bitSizeOf(T)) ++ " bits but occupies " ++ digits(8 * @sizeOf(T)) ++
                            "; declare the remaining bits as an explicitly zero-defaulted field.",
                    );
                    return;
                }
                if (!fieldsCoverEveryByte(T)) @compileError(
                    "serialized type '" ++ path ++ "' (" ++ @typeName(T) ++ ") leaves byte " ++
                        digits(firstUncoveredByte(T)) ++ " of " ++ digits(@sizeOf(T)) ++
                        " uncovered by any field, so it is implicit padding that serialization would write undefined. " ++
                        "Declare it as an explicitly zero-defaulted reserved field (e.g. `_reserved: u32 = 0`).",
                );
                for (s.fields) |field| assertFullyDefined(field.type, path ++ "." ++ field.name);
            },
            .@"union" => |u| {
                if (u.tag_type != null) @compileError(
                    "serialized type '" ++ path ++ "' (" ++ @typeName(T) ++
                        ") is a tagged union, whose discriminant and inactive-variant bytes are not fully defined.",
                );
                for (u.fields) |f| {
                    if (@sizeOf(f.type) != @sizeOf(T)) @compileError(
                        "serialized union '" ++ path ++ "' (" ++ @typeName(T) ++ ") is " ++ digits(@sizeOf(T)) ++
                            " bytes but variant '" ++ f.name ++ "' is only " ++ digits(@sizeOf(f.type)) ++
                            "; the remaining bytes have no discriminant that could tell serialization to scrub them. " ++
                            "Give every variant an explicitly zero-defaulted reserved field so it fills the union exactly.",
                    );
                    assertFullyDefined(f.type, path ++ "." ++ f.name);
                }
            },
            .optional => @compileError("serialized type '" ++ path ++ "' (" ++ @typeName(T) ++
                ") is an optional, whose payload bytes are undefined when it holds null."),
            .type,
            .void,
            .noreturn,
            .comptime_float,
            .comptime_int,
            .undefined,
            .null,
            .error_union,
            .error_set,
            .@"fn",
            .@"opaque",
            .frame,
            .@"anyframe",
            .enum_literal,
            => @compileError("serialized type '" ++ path ++ "' (" ++ @typeName(T) ++
                ") has no defined byte representation: " ++ @tagName(@typeInfo(T))),
        }
    }
}

/// `@compileError` unless every undefined byte of `T` is one that
/// `CompactWriter.zeroValuePadding` canonicalizes. The two must stay in lockstep:
/// each shape accepted here is a shape that function walks.
pub fn assertScrubbable(comptime T: type, comptime path: []const u8) void {
    comptime {
        @setEvalBranchQuota(20_000_000);
        if (isFullyDefined(T)) return;
        switch (@typeInfo(T)) {
            // A scalar narrower than its storage: the declared width says exactly which
            // bits are value bits, so the scrubber masks the rest to zero.
            .int, .@"enum" => {},
            .optional => |o| assertScrubbable(o.child, path ++ ".?"),
            .array => |a| assertScrubbable(a.child, path ++ "[]"),
            .@"struct" => |s| {
                if (s.layout == .@"packed") {
                    // Same as a narrow scalar: the backing integer's declared bits are
                    // the value, and the scrubber masks the spare high bits.
                    return;
                }
                // An extern struct's layout is fixed by declaration, so an inter-field
                // gap is the author's to close; an auto struct's gaps are memset instead.
                if (s.layout == .@"extern" and !fieldsCoverEveryByte(T)) assertFullyDefined(T, path);
                for (s.fields) |field| assertScrubbable(field.type, path ++ "." ++ field.name);
            },
            .@"union" => |u| {
                if (u.tag_type == null) {
                    // No in-band discriminant: nothing can decide which bytes are live.
                    assertFullyDefined(T, path);
                    return;
                }
                // The discriminant needs no width requirement: the scrubber rewrites it
                // zero-extended from its masked value, so even a sub-byte tag's storage
                // byte is deterministic.
                for (u.fields) |f| assertScrubbable(f.type, path ++ "." ++ f.name);
            },
            .type,
            .void,
            .noreturn,
            .comptime_float,
            .comptime_int,
            .undefined,
            .null,
            .error_union,
            .error_set,
            .@"fn",
            .@"opaque",
            .frame,
            .@"anyframe",
            .enum_literal,
            => assertFullyDefined(T, path),
            .float, .vector, .bool, .pointer => assertFullyDefined(T, path),
        }
    }
}

fn firstUncoveredByte(comptime T: type) usize {
    return comptime blk: {
        for (byteCoverageMask(T), 0..) |c, i| {
            if (!c) break :blk i;
        }
        unreachable;
    };
}

/// Decimal rendering of a comptime integer for the messages above. `std.fmt` is
/// unavailable here because this file is imported by modules whose lints ban it.
fn digits(comptime value: usize) []const u8 {
    return comptime blk: {
        if (value == 0) break :blk "0";
        var buf: [20]u8 = undefined;
        var end: usize = buf.len;
        var rest = value;
        while (rest > 0) {
            end -= 1;
            buf[end] = '0' + @as(u8, @intCast(rest % 10));
            rest /= 10;
        }
        const frozen = buf;
        break :blk frozen[end..];
    };
}
