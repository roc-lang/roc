//! Stores attributes for an identifier like the raw_text, is it effectful, ignored, or reassignable.
//! An example of an identifier is the name of a top-level function like `main!` or a variable like `x_`.
const std = @import("std");
const utils = @import("../collections/utils.zig");
const collections = @import("../collections.zig");
const problem = @import("../problem.zig");
const Region = @import("./Region.zig");
const Module = @import("./Module.zig");

const Problem = problem.Problem;
const SmallStringInterner = collections.SmallStringInterner;
const exitOnOom = utils.exitOnOom;

const Ident = @This();

/// The original text of the identifier.
raw_text: []u8,

/// Attributes of the identifier such as if it is effectful, ignored, or reassignable.
attributes: Attributes,

/// Problems with the identifier
/// e.g. if it has two underscores in a row
/// or if it starts with a lowercase then it shouldn't be `lowerCamelCase`, it must be `snake_case`
problems: Problems,

/// Create a new identifier from a string.
pub fn for_text(text: []u8) Ident {
    // TODO: parse idents and their attributes/problems
    return Ident{
        .raw_text = text,
        .attributes = Attributes{
            .effectful = false,
            .ignored = false,
            .reassignable = false,
        },
        .problems = Problems{ .subsequent_underscores = false },
    };
}

/// The index from the store, with the attributes packed into unused bytes.
///
/// With 29-bits for the ID we can store up to 536,870,912 identifiers.
pub const Idx = packed struct(u32) {
    attributes: Attributes,
    idx: u29,
};

/// Identifier attributes such as if it is effectful, ignored, or reassignable packed into 3-bits.
pub const Attributes = packed struct(u3) {
    effectful: bool,
    ignored: bool,
    reassignable: bool,
};

// for example we detect two underscores in a row during parsing... we can make a problem and report
// it to the user later as a warning, but still allow the program to run
pub const Problems = packed struct {
    // TODO: add more problem cases
    subsequent_underscores: bool,

    pub fn has_problems(self: Problems) bool {
        return self.subsequent_underscores;
    }
};

/// An interner for identifier names.
pub const Store = struct {
    interner: SmallStringInterner,
    /// The index of the local module import that this ident comes from.
    ///
    /// By default, this is set to index 0, the primary module being compiled.
    /// This needs to be set when the ident is first seen during canonicalization
    /// before doing anything else.
    exposing_modules: std.ArrayList(Module.Idx),
    next_unique_name: u32,

    pub fn init(allocator: std.mem.Allocator) Store {
        return Store{
            .interner = SmallStringInterner.init(allocator),
            .exposing_modules = std.ArrayList(Module.Idx).init(allocator),
            .next_unique_name = 0,
        };
    }

    pub fn deinit(self: *Store) void {
        self.interner.deinit();
        self.exposing_modules.deinit();
    }

    pub fn insert(self: *Store, ident: Ident, region: Region, problems: *std.ArrayList(Problem)) Idx {
        if (ident.problems.has_problems()) {
            problems.append(Problem.Parse.make(.{ .IdentIssue = .{
                .problems = ident.problems,
                .region = region,
            } })) catch exitOnOom();
        }

        const idx = self.interner.insert(ident.raw_text, region);
        self.exposing_modules.append(@enumFromInt(0)) catch exitOnOom();

        return Idx{
            .attributes = ident.attributes,
            .idx = @as(u29, @intCast(@intFromEnum(idx))),
        };
    }

    pub fn genUnique(self: *Store) Idx {
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

        const idx = self.interner.insert(name, Region.zero());
        self.exposing_modules.append(@enumFromInt(0)) catch exitOnOom();

        return Idx{
            .attributes = Attributes{
                .effectful = false,
                .ignored = false,
                .reassignable = false,
            },
            .idx = @truncate(@intFromEnum(idx)),
        };
    }

    pub fn identsHaveSameText(
        self: *Store,
        first_idx: Idx,
        second_idx: Idx,
    ) bool {
        return self.interner.indicesHaveSameText(
            @enumFromInt(@as(u32, first_idx.idx)),
            @enumFromInt(@as(u32, second_idx.idx)),
        );
    }

    pub fn getText(self: *Store, idx: Idx) []u8 {
        return self.interner.getText(@enumFromInt(@as(u32, idx.idx)));
    }

    pub fn getRegion(self: *Store, idx: Idx) Region {
        return self.interner.getRegion(@enumFromInt(@as(u32, idx.idx)));
    }

    pub fn getExposingModule(self: *Store, idx: Idx) Module.Idx {
        return self.exposing_modules.items[@as(usize, idx.idx)];
    }

    /// Set the module that exposes this ident.
    ///
    /// NOTE: This should be called as soon as an ident is encountered during
    /// canonicalization to make sure that we don't have to worry if the exposing
    /// module is zero because it hasn't been set yet or if it's actually zero.
    pub fn setExposingModule(self: *Store, idx: Idx, exposing_module: Module.Idx) void {
        self.exposing_modules.items[@as(usize, idx.idx)] = exposing_module;
    }

    /// Look up text in this store, returning a slice of all string interner IDs
    /// that match this ident's text.
    pub fn lookup(self: *Store, string: []u8) []SmallStringInterner.Idx {
        return self.interner.lookup(string);
    }
};
