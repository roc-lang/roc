const base = @import("../../base.zig");
const collections = @import("../../collections.zig");

const Ident = base.Ident;

const Self = @This();

header: Header,
defs: Stmt.List,

pub const Header = struct {
    packages: Package.List,
    exposed: Exposed.List,

    pub const Exposed = struct {
        ident: Ident.Idx,
        kind: Kind,

        pub const Kind = enum {
            Type,
            Value,
        };

        pub const List = collections.SafeList(@This());
    };

    pub const Package = struct {
        shorthand: Ident.Idx,
        url: collections.StringLiteral.Idx,

        const List = collections.SafeList(@This());
    };
};

pub const Stmt = union(enum) {
    Import: Import,

    pub const Import = struct {
        // TODO: this will all be changed when the 1st draft of the parser MR is merged
        // Changing to get test working for now
        name: []u8,
        name_region: base.Region,
        package_shorthand: ?[]u8,
        exposing: collections.SafeList(Exposing),

        pub const Exposing = union(enum) {
            Value: base.Ident.Idx,
            Type: base.Ident.Idx,
            CustomTagUnion: struct {
                name: base.Ident.Idx,
                variants: collections.SafeList(base.Ident.Idx),
            },
        };
    };

    pub const List = collections.SafeList(@This());
};
