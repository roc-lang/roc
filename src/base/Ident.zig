//! Stores attributes for an identifier like the raw_text, is it effectful, ignored, or reassignable.
//! An example of an identifier is the name of a top-level function like `main!` or a variable like `x_`.
const std = @import("std");
const base = @import("../base.zig");
const collections = @import("../collections.zig");
const problem = @import("../problem.zig");
const module = @import("module.zig");

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
        .attributes = Attributes{},
        .problems = Problems{},
    };
}

/// The index from the store, with the attributes packed into unused bytes.
///
/// With 29-bits for the ID we can store up to 536,870,911 identifiers.
pub const Idx = packed struct(u32) {
    attributes: Attributes,
    id: u29,
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

    pub fn has_problems(self: *Problems) bool {
        return self.subsequent_underscores;
    }
};

/// Stores identifiers and their regions.
pub const Store = struct {
    interner: collections.IdentInterner,
    regions: std.AutoHashMap(Ident.Idx, base.Region),

    pub fn init(allocator: std.mem.Allocator) Store {
        return Store{
            .interner = collections.SmallString.Interner.init(allocator),
            .regions = std.AutoHashMap(u32, base.Region).init(allocator),
        };
    }

    pub fn deinit(self: *Store) void {
        self.interner.deinit();
        self.regions.deinit();
    }

    pub fn insert(
        self: *Store,
        ident: Ident,
        region: base.Region,
        problems: *std.ArrayList(problem.Problem),
    ) Idx {
        if (ident.problems.has_problems()) {
            problems.push(.IdentIssue{ .problems = ident.problems, .region = region });
        }

        const id = self.interner.insert(ident.raw_text);
        self.regions.put(id.id, region);

        return Idx{ .attributes = ident.attibutes, .id = @as(u29, id.id) };
    }

    pub fn getText(self: *Store, ident_id: Idx) []u8 {
        return self.interner.get(collections.SmallString.Id{ .id = @as(u32, ident_id.id) });
    }

    pub fn getRegion(self: *Store, ident_id: Idx) base.Region {
        return self.regions.get(ident_id).?;
    }
};
