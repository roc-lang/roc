const base = @import("../../base.zig");
const collections = @import("../../collections.zig");

const IR = @This();

// @Anthony Bullard -- save us!
header: struct {
    exposed_idents: collections.SafeList(base.Ident.Idx),
},
defs: collections.SafeList(Stmt),

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
};
