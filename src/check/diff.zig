//! Type comparison utilities for generating helpful error hints.
//!
//! This module compares two snapshotted types and produces hints about
//! potential issues like typos, missing fields, arity mismatches, and
//! effect mismatches.

const std = @import("std");
const base = @import("base");
const snapshot = @import("snapshot.zig");

const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const SnapshotContentIdx = snapshot.SnapshotContentIdx;
const SnapshotContent = snapshot.SnapshotContent;
const SnapshotFlatType = snapshot.SnapshotFlatType;

/// Hint about a type mismatch
pub const Hint = union(enum) {
    arity_mismatch: ArityMismatch,
    fields_missing: FieldsMissing,
    field_typo: FieldTypo,
    tag_typo: TagTypo,
    effect_mismatch: EffectMismatch,
};

pub const ArityMismatch = struct {
    expected: u32,
    actual: u32,
};

pub const FieldsMissing = struct {
    /// Field names that are missing. Allocated from scratch buffer.
    fields: []const Ident.Idx,
};

pub const FieldTypo = struct {
    typo: Ident.Idx,
    suggestion: Ident.Idx,
};

pub const TagTypo = struct {
    typo: Ident.Idx,
    suggestion: Ident.Idx,
};

pub const EffectMismatch = struct {
    /// true if expected pure but got effectful, false if expected effectful but got pure
    expected_pure: bool,
};

/// A bounded list of hints (max 5)
pub const HintList = struct {
    hints: [5]Hint = undefined,
    len: usize = 0,

    pub fn append(self: *HintList, hint: Hint) void {
        if (self.len < 5) {
            self.hints[self.len] = hint;
            self.len += 1;
        }
    }

    pub fn slice(self: *const HintList) []const Hint {
        return self.hints[0..self.len];
    }
};

/// Scratch buffer for hint generation to avoid repeated allocations.
pub const Scratch = struct {
    gpa: Allocator,
    /// Backing storage for field names referenced by FieldsMissing hints
    field_names: std.ArrayList(Ident.Idx),

    pub fn init(gpa: Allocator) !Scratch {
        return .{
            .gpa = gpa,
            .field_names = try std.ArrayList(Ident.Idx).initCapacity(gpa, 8),
        };
    }

    pub fn deinit(self: *Scratch) void {
        self.field_names.deinit(self.gpa);
    }

    pub fn reset(self: *Scratch) void {
        self.field_names.clearRetainingCapacity();
    }
};

/// Calculate Damerau-Levenshtein edit distance between two strings.
/// Returns the minimum number of edits (insertions, deletions, substitutions,
/// or transpositions of adjacent characters) needed to transform a into b.
pub fn editDistance(a: []const u8, b: []const u8) u32 {
    // Short-circuit for empty strings
    if (a.len == 0) return @intCast(b.len);
    if (b.len == 0) return @intCast(a.len);

    // Use a simple matrix approach for small strings
    // For strings up to ~50 chars, this is efficient enough
    const max_len = 64;
    if (a.len > max_len or b.len > max_len) {
        // For very long strings, fall back to max length as distance
        return @intCast(@max(a.len, b.len));
    }

    // Damerau-Levenshtein with two-row optimization
    var prev_prev: [max_len + 1]u32 = undefined;
    var prev: [max_len + 1]u32 = undefined;
    var curr: [max_len + 1]u32 = undefined;

    // Initialize first row
    for (0..b.len + 1) |j| {
        prev[j] = @intCast(j);
    }

    for (a, 0..) |ca, i| {
        curr[0] = @intCast(i + 1);

        for (b, 0..) |cb, j| {
            const cost: u32 = if (ca == cb) 0 else 1;

            curr[j + 1] = @min(
                @min(
                    prev[j + 1] + 1, // deletion
                    curr[j] + 1, // insertion
                ),
                prev[j] + cost, // substitution
            );

            // Transposition
            if (i > 0 and j > 0 and ca == b[j - 1] and a[i - 1] == cb) {
                curr[j + 1] = @min(curr[j + 1], prev_prev[j - 1] + cost);
            }
        }

        // Rotate rows
        const tmp = prev_prev;
        prev_prev = prev;
        prev = curr;
        curr = tmp;
    }

    return prev[b.len];
}

/// Check if a string is a likely typo of another.
/// Uses edit distance with length-dependent thresholds.
pub fn isLikelyTypo(typo: []const u8, correct: []const u8) bool {
    const dist = editDistance(typo, correct);
    // dist > 0 ensures we don't suggest exact matches
    if (dist == 0) return false;

    const min_len = @min(typo.len, correct.len);
    // Short strings (1-2 chars): only allow edit distance of 1
    if (min_len <= 2) return dist == 1;
    // Longer strings: allow edit distance of 1-2
    return dist <= 2;
}

/// Find the best typo suggestion from a list of candidates.
/// Returns the candidate with the smallest edit distance (if within typo threshold).
pub fn findBestTypoSuggestion(
    typo: []const u8,
    candidates: anytype,
    ident_store: *const Ident.Store,
) ?Ident.Idx {
    var best: ?Ident.Idx = null;
    var best_dist: u32 = 3; // Max typo distance + 1

    for (candidates) |candidate| {
        const candidate_name = switch (@TypeOf(candidate)) {
            Ident.Idx => candidate,
            snapshot.SnapshotRecordField => candidate.name,
            snapshot.SnapshotTag => candidate.name,
            else => @compileError("Unsupported candidate type"),
        };
        const candidate_text = ident_store.getText(candidate_name);
        const dist = editDistance(typo, candidate_text);
        if (dist > 0 and dist < best_dist and isLikelyTypo(typo, candidate_text)) {
            best_dist = dist;
            best = candidate_name;
        }
    }

    return best;
}

/// Find the best typo suggestion from a slice of identifier indices.
fn findBestTypoSuggestionFromNames(
    typo: []const u8,
    candidates: []const Ident.Idx,
    ident_store: *const Ident.Store,
) ?Ident.Idx {
    var best: ?Ident.Idx = null;
    var best_dist: u32 = 3; // Max typo distance + 1

    for (candidates) |candidate| {
        const candidate_text = ident_store.getText(candidate);
        const dist = editDistance(typo, candidate_text);
        if (dist > 0 and dist < best_dist and isLikelyTypo(typo, candidate_text)) {
            best_dist = dist;
            best = candidate;
        }
    }

    return best;
}

/// Compare two snapshot types and generate hints about their differences.
pub fn compareTypes(
    snap_store: *const snapshot.Store,
    ident_store: *const Ident.Store,
    expected_idx: SnapshotContentIdx,
    actual_idx: SnapshotContentIdx,
    scratch: *Scratch,
) HintList {
    scratch.reset();
    var hints = HintList{};

    const expected = snap_store.getContent(expected_idx);
    const actual = snap_store.getContent(actual_idx);

    switch (expected) {
        .structure => |exp_flat| switch (actual) {
            .structure => |act_flat| {
                compareStructures(snap_store, ident_store, exp_flat, act_flat, &hints, scratch);
            },
            else => {},
        },
        .alias => |exp_alias| switch (actual) {
            .alias => |act_alias| {
                // Compare the backing types of aliases
                compareTypes2(snap_store, ident_store, exp_alias.backing, act_alias.backing, &hints, scratch);
            },
            .structure => |act_flat| {
                // Compare alias backing with structure
                const exp_backing = snap_store.getContent(exp_alias.backing);
                switch (exp_backing) {
                    .structure => |exp_flat| {
                        compareStructures(snap_store, ident_store, exp_flat, act_flat, &hints, scratch);
                    },
                    else => {},
                }
            },
            else => {},
        },
        else => {},
    }

    return hints;
}

/// Internal comparison that doesn't reset scratch
fn compareTypes2(
    snap_store: *const snapshot.Store,
    ident_store: *const Ident.Store,
    expected_idx: SnapshotContentIdx,
    actual_idx: SnapshotContentIdx,
    hints: *HintList,
    scratch: *Scratch,
) void {
    const expected = snap_store.getContent(expected_idx);
    const actual = snap_store.getContent(actual_idx);

    switch (expected) {
        .structure => |exp_flat| switch (actual) {
            .structure => |act_flat| {
                compareStructures(snap_store, ident_store, exp_flat, act_flat, hints, scratch);
            },
            else => {},
        },
        .alias => |exp_alias| switch (actual) {
            .alias => |act_alias| {
                compareTypes2(snap_store, ident_store, exp_alias.backing, act_alias.backing, hints, scratch);
            },
            .structure => |act_flat| {
                const exp_backing = snap_store.getContent(exp_alias.backing);
                switch (exp_backing) {
                    .structure => |exp_flat| {
                        compareStructures(snap_store, ident_store, exp_flat, act_flat, hints, scratch);
                    },
                    else => {},
                }
            },
            else => {},
        },
        else => {},
    }
}

fn compareStructures(
    snap_store: *const snapshot.Store,
    ident_store: *const Ident.Store,
    expected: SnapshotFlatType,
    actual: SnapshotFlatType,
    hints: *HintList,
    scratch: *Scratch,
) void {
    switch (expected) {
        .fn_pure => |exp_func| {
            switch (actual) {
                .fn_pure => |act_func| {
                    compareFunctions(exp_func, act_func, hints);
                },
                .fn_effectful => |act_func| {
                    // Expected pure, got effectful
                    hints.append(.{ .effect_mismatch = .{ .expected_pure = true } });
                    compareFunctions(exp_func, act_func, hints);
                },
                .fn_unbound => |act_func| {
                    compareFunctions(exp_func, act_func, hints);
                },
                else => {},
            }
        },
        .fn_effectful => |exp_func| {
            switch (actual) {
                .fn_pure => |act_func| {
                    // Expected effectful, got pure
                    hints.append(.{ .effect_mismatch = .{ .expected_pure = false } });
                    compareFunctions(exp_func, act_func, hints);
                },
                .fn_effectful => |act_func| {
                    compareFunctions(exp_func, act_func, hints);
                },
                .fn_unbound => |act_func| {
                    compareFunctions(exp_func, act_func, hints);
                },
                else => {},
            }
        },
        .fn_unbound => |exp_func| {
            switch (actual) {
                .fn_pure, .fn_effectful, .fn_unbound => |act_func| {
                    compareFunctions(exp_func, act_func, hints);
                },
                else => {},
            }
        },
        .record => |exp_record| {
            switch (actual) {
                .record => |act_record| {
                    compareRecords(snap_store, ident_store, exp_record, act_record, hints, scratch);
                },
                .record_unbound => |act_fields| {
                    const exp_fields = snap_store.sliceRecordFields(exp_record.fields);
                    const act_fields_slice = snap_store.sliceRecordFields(act_fields);
                    compareRecordFieldSlices(ident_store, exp_fields, act_fields_slice, hints, scratch);
                },
                .empty_record => {
                    // Actual is empty but expected has fields - all fields missing
                    const exp_fields = snap_store.sliceRecordFields(exp_record.fields);
                    addMissingFieldsFromSlice(exp_fields, hints, scratch);
                },
                else => {},
            }
        },
        .record_unbound => |exp_fields| {
            switch (actual) {
                .record => |act_record| {
                    const exp_fields_slice = snap_store.sliceRecordFields(exp_fields);
                    const act_fields_slice = snap_store.sliceRecordFields(act_record.fields);
                    compareRecordFieldSlices(ident_store, exp_fields_slice, act_fields_slice, hints, scratch);
                },
                .record_unbound => |act_fields| {
                    const exp_fields_slice = snap_store.sliceRecordFields(exp_fields);
                    const act_fields_slice = snap_store.sliceRecordFields(act_fields);
                    compareRecordFieldSlices(ident_store, exp_fields_slice, act_fields_slice, hints, scratch);
                },
                .empty_record => {
                    const exp_fields_slice = snap_store.sliceRecordFields(exp_fields);
                    addMissingFieldsFromSlice(exp_fields_slice, hints, scratch);
                },
                else => {},
            }
        },
        .tag_union => |exp_union| {
            switch (actual) {
                .tag_union => |act_union| {
                    compareTagUnions(snap_store, ident_store, exp_union, act_union, hints);
                },
                else => {},
            }
        },
        else => {},
    }
}

fn compareFunctions(
    exp_func: snapshot.SnapshotFunc,
    act_func: snapshot.SnapshotFunc,
    hints: *HintList,
) void {
    // Compare arity
    const exp_arity: u32 = @intCast(exp_func.args.len());
    const act_arity: u32 = @intCast(act_func.args.len());
    if (exp_arity != act_arity) {
        hints.append(.{ .arity_mismatch = .{
            .expected = exp_arity,
            .actual = act_arity,
        } });
    }
}

fn compareRecords(
    snap_store: *const snapshot.Store,
    ident_store: *const Ident.Store,
    exp_record: snapshot.SnapshotRecord,
    act_record: snapshot.SnapshotRecord,
    hints: *HintList,
    scratch: *Scratch,
) void {
    const exp_fields = snap_store.sliceRecordFields(exp_record.fields);
    const act_fields = snap_store.sliceRecordFields(act_record.fields);
    compareRecordFieldSlices(ident_store, exp_fields, act_fields, hints, scratch);
}

const RecordFieldSlice = snapshot.SnapshotRecordFieldSafeList.Slice;

fn compareRecordFieldSlices(
    ident_store: *const Ident.Store,
    exp_fields: RecordFieldSlice,
    act_fields: RecordFieldSlice,
    hints: *HintList,
    scratch: *Scratch,
) void {
    // Find fields in expected that are missing from actual
    const start_idx = scratch.field_names.items.len;

    const exp_names = exp_fields.items(.name);
    const act_names = act_fields.items(.name);

    for (exp_names) |exp_name_idx| {
        var found = false;

        for (act_names) |act_name_idx| {
            if (exp_name_idx == act_name_idx) {
                found = true;
                break;
            }
        }

        if (!found) {
            // Check if there's a typo candidate in actual fields
            const exp_name = ident_store.getText(exp_name_idx);
            if (findBestTypoSuggestionFromNames(exp_name, act_names, ident_store)) |suggestion| {
                hints.append(.{ .field_typo = .{
                    .typo = suggestion,
                    .suggestion = exp_name_idx,
                } });
            } else {
                // No typo found, add to missing fields
                scratch.field_names.append(scratch.gpa, exp_name_idx) catch {};
            }
        }
    }

    // If we collected missing fields, add a hint
    const missing_count = scratch.field_names.items.len - start_idx;
    if (missing_count > 0) {
        hints.append(.{ .fields_missing = .{
            .fields = scratch.field_names.items[start_idx..],
        } });
    }
}

fn addMissingFieldsFromSlice(
    exp_fields: RecordFieldSlice,
    hints: *HintList,
    scratch: *Scratch,
) void {
    if (exp_fields.len == 0) return;

    const start_idx = scratch.field_names.items.len;
    for (exp_fields.items(.name)) |name| {
        scratch.field_names.append(scratch.gpa, name) catch {};
    }

    hints.append(.{ .fields_missing = .{
        .fields = scratch.field_names.items[start_idx..],
    } });
}

fn compareTagUnions(
    snap_store: *const snapshot.Store,
    ident_store: *const Ident.Store,
    exp_union: snapshot.SnapshotTagUnion,
    act_union: snapshot.SnapshotTagUnion,
    hints: *HintList,
) void {
    const exp_tags = snap_store.sliceTags(exp_union.tags);
    const act_tags = snap_store.sliceTags(act_union.tags);

    const exp_tag_names = exp_tags.items(.name);
    const act_tag_names = act_tags.items(.name);

    // Look for tags in actual that might be typos of expected tags
    for (act_tag_names) |act_name_idx| {
        var found = false;

        for (exp_tag_names) |exp_name_idx| {
            if (act_name_idx == exp_name_idx) {
                found = true;
                break;
            }
        }

        if (!found) {
            // Check if this is a typo of an expected tag
            const act_name = ident_store.getText(act_name_idx);
            if (findBestTypoSuggestionFromNames(act_name, exp_tag_names, ident_store)) |suggestion| {
                hints.append(.{ .tag_typo = .{
                    .typo = act_name_idx,
                    .suggestion = suggestion,
                } });
            }
        }
    }
}

// Tests
test "editDistance - identical strings" {
    try std.testing.expectEqual(@as(u32, 0), editDistance("hello", "hello"));
}

test "editDistance - empty strings" {
    try std.testing.expectEqual(@as(u32, 5), editDistance("", "hello"));
    try std.testing.expectEqual(@as(u32, 5), editDistance("hello", ""));
    try std.testing.expectEqual(@as(u32, 0), editDistance("", ""));
}

test "editDistance - single substitution" {
    try std.testing.expectEqual(@as(u32, 1), editDistance("name", "nme"));
    try std.testing.expectEqual(@as(u32, 1), editDistance("cat", "car"));
}

test "editDistance - transposition" {
    try std.testing.expectEqual(@as(u32, 1), editDistance("teh", "the"));
    try std.testing.expectEqual(@as(u32, 1), editDistance("ab", "ba"));
}

test "editDistance - insertion" {
    try std.testing.expectEqual(@as(u32, 1), editDistance("name", "naame"));
    try std.testing.expectEqual(@as(u32, 1), editDistance("helo", "hello"));
}

test "editDistance - deletion" {
    try std.testing.expectEqual(@as(u32, 1), editDistance("hello", "helo"));
}

test "isLikelyTypo - valid typos" {
    try std.testing.expect(isLikelyTypo("nme", "name"));
    try std.testing.expect(isLikelyTypo("teh", "the"));
    try std.testing.expect(isLikelyTypo("feild", "field"));
    try std.testing.expect(isLikelyTypo("recieve", "receive"));
}

test "isLikelyTypo - not typos" {
    try std.testing.expect(!isLikelyTypo("foo", "bar"));
    try std.testing.expect(!isLikelyTypo("completely", "different"));
    try std.testing.expect(!isLikelyTypo("name", "name")); // exact match
}

test "isLikelyTypo - short strings" {
    try std.testing.expect(isLikelyTypo("a", "b")); // single char, dist=1
    try std.testing.expect(!isLikelyTypo("a", "abc")); // dist=2, but len=1, so no
}
