//! Type comparison utilities for generating helpful error hints.
//!
//! This module compares two snapshotted types and produces hints about
//! potential issues like typos, missing fields, arity mismatches, and
//! effect mismatches.

const std = @import("std");
const base = @import("base");
const snapshot = @import("../snapshot.zig");

const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const SnapshotContentIdx = snapshot.SnapshotContentIdx;
const SnapshotFlatType = snapshot.SnapshotFlatType;
const SnapshotRecordFieldSafeList = snapshot.SnapshotRecordFieldSafeList;
const SnapshotTagSafeList = snapshot.SnapshotTagSafeList;

/// Hint about a type mismatch
pub const Hint = union(enum) {
    arity_mismatch: ArityMismatch,
    fields_missing: FieldsMissing,
    field_typo: FieldTypo,
    tag_typo: TagTypo,
    effect_mismatch: EffectMismatch,
};

/// Hint about fn arity mismatch
pub const ArityMismatch = struct {
    expected: u32,
    actual: u32,
};

/// Hint about missing fields
pub const FieldsMissing = struct {
    /// Range into the fields buffer passed to compareTypes
    fields: SnapshotRecordFieldSafeList.Range,
};

/// Hint about a field typo
pub const FieldTypo = struct {
    typo: Ident.Idx,
    suggestion: Ident.Idx,
};

/// Hint about a tag typo
pub const TagTypo = struct {
    typo: Ident.Idx,
    suggestion: Ident.Idx,
};

/// Hint about a function effect/pure mismatch
pub const EffectMismatch = struct {
    expected: enum { pure, effectful },
};

/// A bounded list of hints (max 5)
pub const HintList = struct {
    hints: [5]Hint = undefined,
    len: usize = 0,

    /// Append a hint to the list
    pub fn append(self: *HintList, hint: Hint) void {
        if (self.len < 5) {
            self.hints[self.len] = hint;
            self.len += 1;
        }
    }

    /// The the slice of hints
    pub fn slice(self: *const HintList) []const Hint {
        return self.hints[0..self.len];
    }
};

// typo suggestions //

/// Calculate Damerau-Levenshtein edit distance between two strings.
/// Returns the minimum number of edits (insertions, deletions, substitutions,
/// or transpositions of adjacent characters) needed to transform a into b.
fn editDistance(a: []const u8, b: []const u8) u32 {
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
fn isLikelyTypo(typo_len: usize, correct_len: usize, dist: u32) bool {
    // dist > 0 ensures we don't suggest exact matches
    if (dist == 0) return false;

    const min_len = @min(typo_len, correct_len);

    // Short strings (1-2 chars): only allow edit distance of 1
    if (min_len <= 2) return dist == 1;

    // Longer strings: allow edit distance of 1-2
    return dist <= 2;
}

/// Find the best typo suggestion from a slice of identifier indices.
fn findBestTypoSuggestion(
    typo: Ident.Idx,
    candidates: []const Ident.Idx,
    ident_store: *const Ident.Store,
) ?Ident.Idx {
    var best: ?Ident.Idx = null;
    var best_dist: u32 = 3; // Max typo distance + 1

    const typo_text = ident_store.getText(typo);

    for (candidates) |candidate| {
        const candidate_text = ident_store.getText(candidate);
        const dist = editDistance(typo_text, candidate_text);
        if (dist > 0 and dist < best_dist and isLikelyTypo(typo_text.len, candidate_text.len, dist)) {
            best_dist = dist;
            best = candidate;
        }
    }

    return best;
}

/// A ranked ident based on the distance
pub const TypoSuggestion = struct {
    ident: Ident.Idx,
    dist: u32,

    const Self = TypoSuggestion;

    /// Returns true if field `a` should sort before field `b` by dist.
    pub fn sortByDistAsc(_: void, a: Self, b: Self) bool {
        return std.math.order(a.dist, b.dist) == .lt;
    }

    pub const ArrayList = std.array_list.Managed(Self);
};

/// Find the best typo suggestions from a slice of identifier indices.
///
/// This clobbers `suggestions` and writes suggestions sorted by rank
pub fn findBestTypoSuggestions(
    typo: Ident.Idx,
    candidates: []const Ident.Idx,
    ident_store: *const Ident.Store,
    suggestions: *TypoSuggestion.ArrayList,
) std.mem.Allocator.Error!void {
    // Clobber and allocate
    suggestions.clearRetainingCapacity();
    try suggestions.ensureUnusedCapacity(candidates.len);

    const typo_text = ident_store.getText(typo);

    // Calculate dist
    for (candidates) |candidate| {
        const candidate_text = ident_store.getText(candidate);
        const dist = editDistance(typo_text, candidate_text);
        suggestions.appendAssumeCapacity(.{
            .dist = dist,
            .ident = candidate,
        });
    }

    // Sort
    std.mem.sort(TypoSuggestion, suggestions.items, {}, TypoSuggestion.sortByDistAsc);
}

// type comparisons //

/// Compare two snapshot types and generate hints about their differences.
/// Caller provides fields and tags buffers. These are clobbered and
/// used to accumulate records/tags. FieldsMissing hints contain Ranges into fields.
pub fn compareTypes(
    snap_store: *const snapshot.Store,
    ident_store: *const Ident.Store,
    expected_idx: SnapshotContentIdx,
    actual_idx: SnapshotContentIdx,
    gpa: Allocator,
    fields: *SnapshotRecordFieldSafeList,
    tags: *SnapshotTagSafeList,
) HintList {
    fields.items.shrinkRetainingCapacity(0);
    tags.items.shrinkRetainingCapacity(0);
    var hints = HintList{};

    const expected = snap_store.getContent(expected_idx);
    const actual = snap_store.getContent(actual_idx);

    switch (expected) {
        .structure => |exp_flat| switch (actual) {
            .structure => |act_flat| {
                compareStructures(snap_store, ident_store, exp_flat, act_flat, &hints, gpa, fields, tags);
            },
            else => {},
        },
        .alias => |exp_alias| switch (actual) {
            .alias => |act_alias| {
                // Compare the backing types of aliases
                compareTypesInternal(snap_store, ident_store, exp_alias.backing, act_alias.backing, &hints, gpa, fields, tags);
            },
            .structure => |act_flat| {
                // Compare alias backing with structure
                const exp_backing = snap_store.getContent(exp_alias.backing);
                switch (exp_backing) {
                    .structure => |exp_flat| {
                        compareStructures(snap_store, ident_store, exp_flat, act_flat, &hints, gpa, fields, tags);
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

/// Internal comparison that doesn't reset buffers
fn compareTypesInternal(
    snap_store: *const snapshot.Store,
    ident_store: *const Ident.Store,
    expected_idx: SnapshotContentIdx,
    actual_idx: SnapshotContentIdx,
    hints: *HintList,
    gpa: Allocator,
    fields: *SnapshotRecordFieldSafeList,
    tags: *SnapshotTagSafeList,
) void {
    const expected = snap_store.getContent(expected_idx);
    const actual = snap_store.getContent(actual_idx);

    switch (expected) {
        .structure => |exp_flat| switch (actual) {
            .structure => |act_flat| {
                compareStructures(snap_store, ident_store, exp_flat, act_flat, hints, gpa, fields, tags);
            },
            else => {},
        },
        .alias => |exp_alias| switch (actual) {
            .alias => |act_alias| {
                compareTypesInternal(snap_store, ident_store, exp_alias.backing, act_alias.backing, hints, gpa, fields, tags);
            },
            .structure => |act_flat| {
                const exp_backing = snap_store.getContent(exp_alias.backing);
                switch (exp_backing) {
                    .structure => |exp_flat| {
                        compareStructures(snap_store, ident_store, exp_flat, act_flat, hints, gpa, fields, tags);
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
    gpa: Allocator,
    fields: *SnapshotRecordFieldSafeList,
    tags: *SnapshotTagSafeList,
) void {
    switch (expected) {
        .fn_pure => |exp_func| {
            switch (actual) {
                .fn_pure => |act_func| {
                    compareFunctions(exp_func, act_func, hints);
                },
                .fn_effectful => |act_func| {
                    // Expected pure, got effectful
                    hints.append(.{ .effect_mismatch = .{ .expected = .pure } });
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
                    hints.append(.{ .effect_mismatch = .{ .expected = .effectful } });
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
                    compareRecords(snap_store, ident_store, exp_record, act_record, hints, gpa, fields);
                },
                .record_unbound => |act_fields_range| {
                    // Gather expected fields (with extensions), actual is just immediate fields
                    const exp_range = gatherFieldsFromRecord(snap_store, exp_record, gpa, fields);
                    const exp_names = fields.sliceRange(exp_range).items(.name);
                    const act_names = snap_store.sliceRecordFields(act_fields_range).items(.name);
                    compareFieldNames(ident_store, exp_names, act_names, hints, gpa, fields);
                },
                .empty_record => {
                    // Actual is empty but expected has fields - gather all missing
                    const exp_range = gatherFieldsFromRecord(snap_store, exp_record, gpa, fields);
                    const exp_names = fields.sliceRange(exp_range).items(.name);
                    addMissingFields(exp_names, hints, gpa, fields);
                },
                else => {},
            }
        },
        .record_unbound => |exp_fields_range| {
            switch (actual) {
                .record => |act_record| {
                    // Expected is just immediate fields, gather actual (with extensions)
                    const exp_names = snap_store.sliceRecordFields(exp_fields_range).items(.name);
                    const act_range = gatherFieldsFromRecord(snap_store, act_record, gpa, fields);
                    const act_names = fields.sliceRange(act_range).items(.name);
                    compareFieldNames(ident_store, exp_names, act_names, hints, gpa, fields);
                },
                .record_unbound => |act_fields_range| {
                    // Both are just immediate fields, no extensions
                    const exp_names = snap_store.sliceRecordFields(exp_fields_range).items(.name);
                    const act_names = snap_store.sliceRecordFields(act_fields_range).items(.name);
                    compareFieldNames(ident_store, exp_names, act_names, hints, gpa, fields);
                },
                .empty_record => {
                    const exp_names = snap_store.sliceRecordFields(exp_fields_range).items(.name);
                    addMissingFields(exp_names, hints, gpa, fields);
                },
                else => {},
            }
        },
        .tag_union => |exp_union| {
            switch (actual) {
                .tag_union => |act_union| {
                    compareTagUnions(snap_store, ident_store, exp_union, act_union, hints, gpa, tags);
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
    gpa: Allocator,
    fields: *SnapshotRecordFieldSafeList,
) void {
    // Gather ALL fields from expected (including extensions)
    const exp_range = gatherFieldsFromRecord(snap_store, exp_record, gpa, fields);
    const exp_names = fields.sliceRange(exp_range).items(.name);

    // Gather ALL fields from actual (including extensions)
    const act_range = gatherFieldsFromRecord(snap_store, act_record, gpa, fields);
    const act_names = fields.sliceRange(act_range).items(.name);

    compareFieldNames(ident_store, exp_names, act_names, hints, gpa, fields);
}

/// Gather all fields from a record, following extension chain.
/// Returns a Range into fields buffer.
pub fn gatherFieldsFromRecord(
    snap_store: *const snapshot.Store,
    record: snapshot.SnapshotRecord,
    gpa: Allocator,
    fields: *SnapshotRecordFieldSafeList,
) SnapshotRecordFieldSafeList.Range {
    const start: u32 = fields.len();
    snap_store.gatherRecordFieldsHelp(record, gpa, fields) catch {};
    return fields.rangeToEnd(start);
}

/// Compare two sets of field names and generate hints for missing fields and typos.
fn compareFieldNames(
    ident_store: *const Ident.Store,
    exp_names: []const Ident.Idx,
    act_names: []const Ident.Idx,
    hints: *HintList,
    gpa: Allocator,
    fields: *SnapshotRecordFieldSafeList,
) void {
    // Track where missing fields start in buffer
    const start_idx: u32 = fields.len();

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
            if (findBestTypoSuggestion(exp_name_idx, act_names, ident_store)) |suggestion| {
                hints.append(.{ .field_typo = .{
                    .typo = suggestion,
                    .suggestion = exp_name_idx,
                } });
            } else {
                // No typo found, add to missing fields (with placeholder content)
                _ = fields.append(gpa, .{ .name = exp_name_idx, .content = .first }) catch {};
            }
        }
    }

    // If we collected missing fields, add a hint
    const missing_range = fields.rangeToEnd(start_idx);
    if (missing_range.count > 0) {
        hints.append(.{ .fields_missing = .{
            .fields = missing_range,
        } });
    }
}

/// Add a hint for all missing fields.
fn addMissingFields(
    exp_names: []const Ident.Idx,
    hints: *HintList,
    gpa: Allocator,
    fields: *SnapshotRecordFieldSafeList,
) void {
    if (exp_names.len == 0) return;

    const start_idx: u32 = fields.len();
    for (exp_names) |name| {
        _ = fields.append(gpa, .{ .name = name, .content = .first }) catch {};
    }

    hints.append(.{ .fields_missing = .{
        .fields = fields.rangeToEnd(start_idx),
    } });
}

fn compareTagUnions(
    snap_store: *const snapshot.Store,
    ident_store: *const Ident.Store,
    exp_union: snapshot.SnapshotTagUnion,
    act_union: snapshot.SnapshotTagUnion,
    hints: *HintList,
    gpa: Allocator,
    tags: *SnapshotTagSafeList,
) void {
    // Gather ALL tags from expected (including extensions)
    const exp_range = gatherTagsFromUnion(snap_store, exp_union, gpa, tags);
    const exp_tag_names = tags.sliceRange(exp_range).items(.name);

    // Gather ALL tags from actual (including extensions)
    const act_range = gatherTagsFromUnion(snap_store, act_union, gpa, tags);
    const act_tag_names = tags.sliceRange(act_range).items(.name);

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
            if (findBestTypoSuggestion(act_name_idx, exp_tag_names, ident_store)) |suggestion| {
                hints.append(.{ .tag_typo = .{
                    .typo = act_name_idx,
                    .suggestion = suggestion,
                } });
            }
        }
    }
}

/// Gather all tags from a tag union, following extension chain.
/// Returns a Range into tags buffer.
fn gatherTagsFromUnion(
    snap_store: *const snapshot.Store,
    union_: snapshot.SnapshotTagUnion,
    gpa: Allocator,
    tags: *SnapshotTagSafeList,
) SnapshotTagSafeList.Range {
    const start: u32 = tags.len();

    // Add immediate tags
    const union_tags = snap_store.sliceTags(union_.tags);
    for (union_tags.items(.name), union_tags.items(.args), union_tags.items(.formatted)) |name, args, formatted| {
        _ = tags.append(gpa, .{ .name = name, .args = args, .formatted = formatted }) catch {};
    }

    // Follow extension chain
    var ext_idx = union_.ext;
    while (true) {
        const content = snap_store.getContent(ext_idx);
        switch (content) {
            .structure => |flat| switch (flat) {
                .tag_union => |ext_union| {
                    const ext_tags = snap_store.sliceTags(ext_union.tags);
                    for (ext_tags.items(.name), ext_tags.items(.args), ext_tags.items(.formatted)) |name, args, formatted| {
                        _ = tags.append(gpa, .{ .name = name, .args = args, .formatted = formatted }) catch {};
                    }
                    ext_idx = ext_union.ext;
                },
                .empty_tag_union => break,
                else => break,
            },
            .alias => |alias| {
                ext_idx = alias.backing;
            },
            .flex, .rigid, .err, .recursive => break,
        }
    }

    return tags.rangeToEnd(start);
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
    // spellchecker:ignore-next-line
    try std.testing.expectEqual(@as(u32, 1), editDistance("teh", "the"));
    try std.testing.expectEqual(@as(u32, 1), editDistance("ab", "ba"));
}

test "editDistance - insertion" {
    // spellchecker:ignore-next-line
    try std.testing.expectEqual(@as(u32, 1), editDistance("name", "naame"));

    // spellchecker:ignore-next-line
    try std.testing.expectEqual(@as(u32, 1), editDistance("helo", "hello"));
}

test "editDistance - deletion" {
    // spellchecker:ignore-next-line
    try std.testing.expectEqual(@as(u32, 1), editDistance("hello", "helo"));
}

test "isLikelyTypo - valid typos" {
    try std.testing.expect(blk: {
        const typo = "nme";
        const actual = "name";
        const dist = editDistance(typo, actual);
        break :blk isLikelyTypo(typo.len, actual.len, dist);
    });
    try std.testing.expect(blk: {
        // spellchecker:ignore-next-line
        const typo = "teh";
        const actual = "the";
        const dist = editDistance(typo, actual);
        break :blk isLikelyTypo(typo.len, actual.len, dist);
    });
    try std.testing.expect(blk: {
        // spellchecker:ignore-next-line
        const typo = "feild";
        const actual = "field";
        const dist = editDistance(typo, actual);
        break :blk isLikelyTypo(typo.len, actual.len, dist);
    });
    try std.testing.expect(blk: {
        // spellchecker:ignore-next-line
        const typo = "recieve";
        const actual = "receive";
        const dist = editDistance(typo, actual);
        break :blk isLikelyTypo(typo.len, actual.len, dist);
    });
}

test "isLikelyTypo - not typos" {
    try std.testing.expect(blk: {
        const typo = "foo";
        const actual = "bar";
        const dist = editDistance(typo, actual);
        break :blk !isLikelyTypo(typo.len, actual.len, dist);
    });
    try std.testing.expect(blk: {
        const typo = "completely";
        const actual = "different";
        const dist = editDistance(typo, actual);
        break :blk !isLikelyTypo(typo.len, actual.len, dist);
    });
    try std.testing.expect(blk: {
        const typo = "name";
        const actual = "name";
        const dist = editDistance(typo, actual);
        break :blk !isLikelyTypo(typo.len, actual.len, dist);
    }); // exact match
}

test "isLikelyTypo - short strings" {
    try std.testing.expect(blk: {
        const typo = "a";
        const actual = "b";
        const dist = editDistance(typo, actual);
        break :blk isLikelyTypo(typo.len, actual.len, dist);
    }); // single char, dist=1
    try std.testing.expect(blk: {
        const typo = "a";
        const actual = "abc";
        const dist = editDistance(typo, actual);
        break :blk !isLikelyTypo(typo.len, actual.len, dist);
    }); // dist=2, but len=1, so no
}
