//! What `Str.inspect` renders, defined once for every consumer.
//!
//! Three consumers render values: Monotype lowering (the `.lss` strategy)
//! builds rendering expressions, Boxy lowering emits rendering LIR, and the
//! Boxy runtime renders descriptor-described values to bytes. Each reads its
//! own type representation into an `InspectShape`, and `InspectSpec` decides
//! from that shape what the rendering is: a whole literal, an ordered list of
//! parts (literal text and component renderings), or a rendering the
//! consumer realizes from the action's text (a list loop, a tag switch).
//! Every format literal lives in `text`, so a change to how Roc renders a
//! value is a change here, and every consumer follows it.

const ShapeKind = @import("shape.zig").ShapeKind;

/// The error set of a parts sink: a pointer to a type that declares the
/// `PartsError` its `literal`, `label`, and `child` return.
fn SinkError(comptime SinkPtr: type) type {
    return @typeInfo(SinkPtr).pointer.child.PartsError;
}

/// Every literal inspection renders.
pub const text = struct {
    pub const record_open = "{ ";
    pub const record_close = " }";
    pub const empty_record = "{}";
    pub const field_label_sep = ": ";
    /// Between record fields, tuple elements, tag payloads, and list items.
    pub const sep = ", ";
    pub const tuple_open = "(";
    pub const tuple_close = ")";
    pub const empty_tuple = "()";
    pub const payload_open = "(";
    pub const payload_close = ")";
    pub const list_open = "[";
    pub const list_close = "]";
    pub const empty_list = list_open ++ list_close;
    pub const box_open = "Box(";
    pub const box_close = ")";
    /// An unset optional field.
    pub const missing = "<missing>";
    pub const function = "<function>";
    /// A value whose structure its type does not expose.
    pub const opaque_value = "<opaque>";
    pub const bool_true = "True";
    pub const bool_false = "False";
    /// A Box whose erased allocation holds the empty record.
    pub const boxed_empty_record = box_open ++ empty_record ++ box_close;
    /// The crash message when rendering reaches a value of a type with no values.
    pub const uninhabited_crash = "uninhabited value reached Str.inspect";
};

/// How inspection reads one type. `Ctx` names the consumer's handles:
///
/// ```
/// Ty, Primitive, Record, Tuple, TagUnion, PresenceSlot, Override, Child, Label
/// ```
///
/// A `Label` is a record field's label or a variant's name; a part sink
/// resolves it to text when it emits it.
pub fn InspectShape(comptime Ctx: type) type {
    return union(ShapeKind) {
        primitive: Ctx.Primitive,
        bool,
        empty_record,
        record: Ctx.Record,
        tuple: Ctx.Tuple,
        tag_union: Ctx.TagUnion,
        presence_slot: Ctx.PresenceSlot,
        list: Ctx.Ty,
        box: Ctx.Ty,
        nominal: Nominal,
        transparent: Ctx.Ty,
        function,
        dynamic,

        pub const Nominal = struct {
            /// The eligible `to_inspect` override, which replaces the
            /// default rendering.
            override: ?Ctx.Override,
            /// The backing the default rendering shows, or null when the
            /// type does not expose its structure.
            backing: ?Ctx.Ty,
        };
    };
}

/// One record field as a rendering reads it.
pub fn RecordField(comptime Ctx: type) type {
    return struct {
        label: Ctx.Label,
        child: Ctx.Child,
    };
}

/// Inspection rendering over `Ctx`'s type representation.
///
/// `Ctx` supplies, besides the `InspectShape` handle types:
///
/// ```
/// inspectShape(ty) InspectShape(Ctx)
/// recordSlotCount(record) u32
/// recordField(record, slot) ?RecordField(Ctx)   // null: padding, which holds no value
/// tupleElemCount(tuple) u32
/// tupleElem(tuple, index) Child
/// variantCount(tag_union) u32
/// variantName(tag_union, variant) Label
/// payloadCount(tag_union, variant) u32
/// payload(tag_union, variant, index) Child
/// ```
///
/// A part sink supplies `literal([]const u8)`, `label(Label)`, and
/// `child(Child)`, called in rendering order.
pub fn InspectSpec(comptime Ctx: type) type {
    return struct {
        pub const Shape = InspectShape(Ctx);

        /// What a value's rendering is.
        pub const Action = union(enum) {
            /// The whole rendering is this text.
            literal: []const u8,
            primitive: Ctx.Primitive,
            /// `text.bool_true` or `text.bool_false` by discriminant.
            bool,
            /// The parts `recordParts` emits.
            record: Ctx.Record,
            /// The parts `tupleParts` emits.
            tuple: Ctx.Tuple,
            /// The active variant's parts, which `variantParts` emits.
            tag_union: Ctx.TagUnion,
            /// The type has no values, so no rendering is reachable.
            uninhabited,
            /// `text.missing` for an unset field, otherwise the payload's
            /// rendering.
            presence_slot: Ctx.PresenceSlot,
            /// `text.list_open`, the items' renderings separated by
            /// `text.sep`, then `text.list_close`.
            list: Ctx.Ty,
            /// `text.box_open`, the payload's rendering, `text.box_close`.
            box: Ctx.Ty,
            /// The `to_inspect` override's result.
            override: Ctx.Override,
            /// A nominal's backing value rendered at the backing type.
            backing: Ctx.Ty,
            /// The value rendered at the type it names.
            transparent: Ctx.Ty,
            /// The value's runtime descriptor decides.
            descriptor,
        };

        pub fn action(ctx: Ctx, ty: Ctx.Ty) Action {
            return actionForShape(ctx, ctx.inspectShape(ty));
        }

        pub fn actionForShape(ctx: Ctx, shape: Shape) Action {
            return switch (shape) {
                .primitive => |primitive| .{ .primitive = primitive },
                .bool => .bool,
                .empty_record => .{ .literal = text.empty_record },
                .record => |record| if (renderedFieldCount(ctx, record) == 0)
                    .{ .literal = text.empty_record }
                else
                    .{ .record = record },
                .tuple => |tuple| if (ctx.tupleElemCount(tuple) == 0)
                    .{ .literal = text.empty_tuple }
                else
                    .{ .tuple = tuple },
                .tag_union => |tag_union| if (ctx.variantCount(tag_union) == 0)
                    .uninhabited
                else
                    .{ .tag_union = tag_union },
                .presence_slot => |slot| .{ .presence_slot = slot },
                .list => |elem| .{ .list = elem },
                .box => |payload| .{ .box = payload },
                .nominal => |nominal| if (nominal.override) |override|
                    .{ .override = override }
                else if (nominal.backing) |backing|
                    .{ .backing = backing }
                else
                    .{ .literal = text.opaque_value },
                .transparent => |named| .{ .transparent = named },
                .function => .{ .literal = text.function },
                .dynamic => .descriptor,
            };
        }

        fn renderedFieldCount(ctx: Ctx, record: Ctx.Record) u32 {
            var count: u32 = 0;
            var slot: u32 = 0;
            while (slot < ctx.recordSlotCount(record)) : (slot += 1) {
                if (ctx.recordField(record, slot) != null) count += 1;
            }
            return count;
        }

        /// A record's rendering: `{ label: value, ... }`, padding skipped.
        pub fn recordParts(ctx: Ctx, record: Ctx.Record, sink: anytype) SinkError(@TypeOf(sink))!void {
            try sink.literal(text.record_open);
            var written: u32 = 0;
            var slot: u32 = 0;
            while (slot < ctx.recordSlotCount(record)) : (slot += 1) {
                const field = ctx.recordField(record, slot) orelse continue;
                if (written != 0) try sink.literal(text.sep);
                try sink.label(field.label);
                try sink.literal(text.field_label_sep);
                try sink.child(field.child);
                written += 1;
            }
            try sink.literal(text.record_close);
        }

        /// A tuple's rendering: `(value, ...)`.
        pub fn tupleParts(ctx: Ctx, tuple: Ctx.Tuple, sink: anytype) SinkError(@TypeOf(sink))!void {
            try sink.literal(text.tuple_open);
            var index: u32 = 0;
            while (index < ctx.tupleElemCount(tuple)) : (index += 1) {
                if (index != 0) try sink.literal(text.sep);
                try sink.child(ctx.tupleElem(tuple, index));
            }
            try sink.literal(text.tuple_close);
        }

        /// One variant's rendering: its name, then `(payload, ...)` when it
        /// carries payloads.
        pub fn variantParts(ctx: Ctx, tag_union: Ctx.TagUnion, variant: u32, sink: anytype) SinkError(@TypeOf(sink))!void {
            try sink.label(ctx.variantName(tag_union, variant));
            const count = ctx.payloadCount(tag_union, variant);
            if (count == 0) return;
            try sink.literal(text.payload_open);
            var index: u32 = 0;
            while (index < count) : (index += 1) {
                if (index != 0) try sink.literal(text.sep);
                try sink.child(ctx.payload(tag_union, variant, index));
            }
            try sink.literal(text.payload_close);
        }

        /// The separator before list item `index`.
        pub fn listSeparator(index: u64) []const u8 {
            return if (index == 0) "" else text.sep;
        }

        /// The text a Bool renders as.
        pub fn boolText(value: bool) []const u8 {
            return if (value) text.bool_true else text.bool_false;
        }
    };
}
