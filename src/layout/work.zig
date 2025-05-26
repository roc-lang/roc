const std = @import("std");
const types = @import("../types/types.zig");
const layout = @import("./layout.zig");
const Ident = @import("../base/Ident.zig");

const Var = types.Var;
const Idx = layout.Idx;

pub const Parent = union(enum) {
    none,
    box_elem: struct { idx: Idx },
    list_elem: struct { idx: Idx },
    record_field: struct { idx: Idx, field_name: Ident.Idx },
};

pub const FieldWorkItem = struct {
    var_: Var,
    field_name: Ident.Idx,
};

pub const WorkItem = struct {
    var_to_process: Var,
    parent: Parent,
    // For deferred layout creation (box/list elements)
    deferred_action: ?DeferredAction = null,

    pub const DeferredAction = union(enum) {
        create_box: struct { parent: Parent },
        create_list: struct { parent: Parent },
        finalize_record: struct {
            record_idx: Idx,
            parent: Parent,
            fields_start: u32,
        },
    };
};