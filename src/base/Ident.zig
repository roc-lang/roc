//! Any text in a Roc source file that has significant content.
//!
//! During tokenization, all variable names, record field names, type names, etc. are interned
//! into a deduplicated collection, the [Ident.Store]. On interning, each Ident gets a unique ID
//! that represents that string, which can be used to look up the string value in the [Ident.Store]
//! in constant time. Storing IDs in each IR instead of strings also uses less memory in the IRs.

const std = @import("std");
const collections = @import("../collections.zig");
const problem = @import("../problem.zig");
const Region = @import("Region.zig");
const ModuleImport = @import("ModuleImport.zig");

const SmallStringInterner = collections.SmallStringInterner;
const exitOnOom = collections.utils.exitOnOom;

const Ident = @This();

/// The original text of the identifier.
raw_text: []const u8,

/// Attributes of the identifier such as if it is effectful, ignored, or reassignable.
attributes: Attributes,

/// Create a new identifier from a string.
pub fn for_text(text: []const u8) Ident {
    // TODO: parse idents and their attributes/problems
    return Ident{
        .raw_text = text,
        .attributes = Attributes{
            .effectful = false,
            .ignored = false,
            .reassignable = false,
        },
    };
}

/// The index from the store, with the attributes packed into unused bytes.
///
/// With 29-bits for the ID we can store up to 536,870,912 identifiers.
pub const Idx = packed struct(u32) {
    attributes: Attributes,
    idx: u29,
};

/// Identifier attributes such as if it is effectful, ignored, or reassignable.
pub const Attributes = packed struct(u3) {
    effectful: bool,
    ignored: bool,
    reassignable: bool,
};

/// An interner for identifier names.
pub const Store = struct {
    interner: SmallStringInterner = .{},
    /// The index of the local module import that this ident comes from.
    ///
    /// By default, this is set to index 0, the primary module being compiled.
    /// This needs to be set when the ident is first seen during canonicalization
    /// before doing anything else.
    exposing_modules: std.ArrayListUnmanaged(ModuleImport.Idx) = .{},
    attributes: std.ArrayListUnmanaged(Attributes) = .{},
    next_unique_name: u32 = 0,

    /// deinit the store memory
    pub fn deinit(self: *Store, gpa: std.mem.Allocator) void {
        self.interner.deinit(gpa);
        self.exposing_modules.deinit(gpa);
        self.attributes.deinit(gpa);
    }

    /// insert a new identifier into the store
    pub fn insert(self: *Store, gpa: std.mem.Allocator, ident: Ident, region: Region) Idx {
        const idx = self.interner.insert(gpa, ident.raw_text, region);
        self.exposing_modules.append(gpa, @enumFromInt(0)) catch |err| exitOnOom(err);
        self.attributes.append(gpa, ident.attributes) catch |err| exitOnOom(err);

        return Idx{
            .attributes = ident.attributes,
            .idx = @as(u29, @intCast(@intFromEnum(idx))),
        };
    }

    /// generate a unique identifier
    pub fn genUnique(self: *Store, gpa: std.mem.Allocator) Idx {
        var id = self.next_unique_name;
        self.next_unique_name += 1;

        // Manually render the text into a buffer to avoid allocating
        // a string as the string interner will copy the text anyway.

        var digit_index: u8 = 9;
        // The max u32 value is 4294967295 which is 10 digits
        var str_buffer = [_]u8{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
        while (id > 0) {
            const digit = id % 10;
            str_buffer[digit_index] = @as(u8, @intCast(digit)) + '0';

            id = (id - digit) / 10;
            digit_index -= 1;
        }

        // const name_length = if (id < 10) 1 else ;
        const name = str_buffer[digit_index..];

        const idx = self.interner.insert(gpa, name, Region.zero());
        self.exposing_modules.append(gpa, @enumFromInt(0)) catch |err| exitOnOom(err);

        const attributes = Attributes{
            .effectful = false,
            .ignored = false,
            .reassignable = false,
        };
        self.attributes.append(gpa, attributes) catch |err| exitOnOom(err);

        return Idx{
            .attributes = attributes,
            .idx = @truncate(@intFromEnum(idx)),
        };
    }

    /// asserts two identifiers have the same text,
    /// e.g. "foo" and "foo"
    pub fn identsHaveSameText(
        self: *const Store,
        first_idx: Idx,
        second_idx: Idx,
    ) bool {
        return self.interner.indicesHaveSameText(
            @enumFromInt(@as(u32, first_idx.idx)),
            @enumFromInt(@as(u32, second_idx.idx)),
        );
    }

    /// get the text for an identifier
    pub fn getText(self: *const Store, idx: Idx) []u8 {
        return self.interner.getText(@enumFromInt(@as(u32, idx.idx)));
    }

    /// get the region for an identifier
    pub fn getRegion(self: *const Store, idx: Idx) Region {
        return self.interner.getRegion(@enumFromInt(@as(u32, idx.idx)));
    }

    /// get the module for an identifier
    /// TODO -- Sam can you confirm this is correct?
    pub fn getExposingModule(self: *const Store, idx: Idx) ModuleImport.Idx {
        return self.exposing_modules.items[@as(usize, idx.idx)];
    }

    /// Set the module import that exposes this ident.
    ///
    /// NOTE: This should be called as soon as an ident is encountered during
    /// canonicalization to make sure that we don't have to worry if the exposing
    /// module is zero because it hasn't been set yet or if it's actually zero.
    pub fn setExposingModule(self: *const Store, idx: Idx, exposing_module: ModuleImport.Idx) void {
        self.exposing_modules.items[@as(usize, idx.idx)] = exposing_module;
    }
};
