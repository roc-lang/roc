const Monotype = @import("Monotype.zig");

pub const Projection = union(enum) {
    field: Monotype.Name,
    tuple_elem: u32,
    tag_payload: struct {
        tag_name: Monotype.Name,
        payload_index: u32,
    },
    list_elem: u32,
};

pub const ProjectionSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() ProjectionSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: ProjectionSpan) bool {
        return self.len == 0;
    }
};
