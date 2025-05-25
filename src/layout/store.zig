//! The store of solved types
//! Contains both Slot & Descriptor stores

const std = @import("std");
const types = @import("../types/types.zig");
const layout = @import("./layout.zig");

const Var = types.Var;

pub const Store = struct {
    pub fn addTypeVar(
        self: *Store,
        var_store: *const types.Store,
        var_: Var,
    ) layout.Idx {
        switch (var_store.resolveVar(var_).desc.content) {
            .structure => |flat_type| {
                switch (flat_type) {
                    .str => {
                        // TODO
                    },
                    .box => |elem_var| {
                        // TODO
                    },
                    .list => |elem_var| {
                        // TODO
                    },
                    .tuple => |tuple| {
                        // TODO
                    },
                    .num => |num| {
                        // TODO
                    },
                    .custom_type => |custom_type| {
                        // TODO
                    },
                    .func => |func| {
                        // TODO
                    },
                    .record => |record| {
                        // TODO
                    },
                    .empty_record => {
                        // TODO
                    },
                    .tag_union => |tag_union| {
                        // TODO
                    },
                    .empty_tag_union => {
                        // TODO
                    },
                }
            },
            .flex_var => |_| {
                // TODO: isn't there some scenario where we can send a flex var to the host as void*?
            },
            .rigid_var => |_| {
                // TODO: isn't there some scenario where we can send a rigid var to the host as void*?
            },
            .alias => |alias| {
                // TODO resolve the alias in a loop, without recursion
            },
            .effectful => {
                // doesn't make sense; ideally we'd move this out of Content to make impossible states impossible
            },
            .pure => {
                // doesn't make sense; ideally we'd move this out of Content to make impossible states impossible
            },
            .err => {
                // generate a crash prob
            },
        }
    }
};
