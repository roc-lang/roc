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
        name: base.Ident.Idx,
        name_region: base.Region,
        package_shorthand: ?base.Ident.Idx,
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
