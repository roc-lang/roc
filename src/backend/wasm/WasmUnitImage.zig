//! A wasm unit image: one module's wasm code and data laid out once, ready
//! to be composed into a wasm module by copying and patching (plan "Wasm
//! fast dev link", design.md "Fast Dev Link").
//!
//! An image holds the unit's function signatures, its function bodies
//! exactly as a code section stores them (locals declaration, then code),
//! its data segments, the symbols it exports, the functions whose table
//! slots its code or data takes, the imports it names, and every site in
//! a body or segment that names something by index or address: another
//! function of the unit, a data segment of the unit, a signature of the
//! unit, or an import. Every code site is a padded 5-byte LEB128 field and
//! every data site a 4-byte little-endian field, so a site is patched in
//! place and no body or segment changes length when it is placed. The
//! image fixes no index or address of its own: site fields hold zero.
//!
//! Each function and each segment is an atom; its sites are its edges.
//! When the unit's code checks stack bounds, each function also records
//! the stack frame facts a composer needs to give each check its room
//! (`StackRoom`): how far below its entry stack pointer its own frame
//! reaches, the stack depth each of its calls starts its callee at, and
//! whether it runs a check before anything else.
//!
//! The same format holds the code a program is linked with beside its
//! units: the compiler's support objects and a platform's host, each
//! converted once from a relocatable object (`ObjectImage`). Those images
//! may also define globals with their initial values, name constructors
//! the program runs at start, mark definitions weak, and import functions
//! under a module and field other than their symbol name.
//!
//! The encoding is a pure function of its input: fixed-width
//! little-endian tables in input order, exports sorted by name, and a
//! BLAKE3 digest of the bytes. Nothing in an image names a path.
//!
//! This file depends on `std` alone.
const std = @import("std");
const Allocator = std.mem.Allocator;

const magic = "RBWU".*;
/// Format version; bump whenever the encoding or its contents change.
pub const format_version: u32 = 2;

/// The alignment an image's bytes need in memory for `View`.
pub const alignment: std.mem.Alignment = .@"16";

/// How a site's field encodes its target.
pub const SiteKind = enum(u8) {
    /// A function index in `call` (`R_WASM_FUNCTION_INDEX_LEB`).
    function_index_leb = 0,
    /// A type index in `call_indirect` (`R_WASM_TYPE_INDEX_LEB`).
    type_index_leb = 1,
    /// A table slot in `i32.const` (`R_WASM_TABLE_INDEX_SLEB`).
    table_index_sleb = 2,
    /// A table slot relative to `__table_base` (`R_WASM_TABLE_INDEX_REL_SLEB`).
    table_index_rel_sleb = 3,
    /// A memory address in a load or store (`R_WASM_MEMORY_ADDR_LEB`).
    memory_addr_leb = 4,
    /// A memory address in `i32.const` (`R_WASM_MEMORY_ADDR_SLEB`).
    memory_addr_sleb = 5,
    /// A memory address relative to `__memory_base` (`R_WASM_MEMORY_ADDR_REL_SLEB`).
    memory_addr_rel_sleb = 6,
    /// A global index in `global.get` or `global.set` (`R_WASM_GLOBAL_INDEX_LEB`).
    global_index_leb = 7,
    /// A table number in `call_indirect` (`R_WASM_TABLE_NUMBER_LEB`).
    table_number_leb = 8,
    /// The bound of a stack check in `i32.const`: the address of its
    /// import (`__stack_low`) plus the room the checking function needs,
    /// which only the composed program's call graph decides.
    stack_room_sleb = 9,
    /// A memory address in a data segment (`R_WASM_MEMORY_ADDR_I32`).
    memory_addr_i32 = 10,
    /// A table slot in a data segment (`R_WASM_TABLE_INDEX_I32`).
    table_index_i32 = 11,

    /// The bytes of the field the site names.
    pub fn fieldSize(self: SiteKind) u32 {
        return if (self.inData()) 4 else 5;
    }

    /// Whether the site's field lies in a data segment rather than a body.
    pub fn inData(self: SiteKind) bool {
        return switch (self) {
            .memory_addr_i32, .table_index_i32 => true,
            .function_index_leb, .type_index_leb, .table_index_sleb, .table_index_rel_sleb, .memory_addr_leb, .memory_addr_sleb, .memory_addr_rel_sleb, .global_index_leb, .table_number_leb, .stack_room_sleb => false,
        };
    }
};

/// What a site, a call, or an export names.
pub const TargetKind = enum(u8) {
    /// A function of the unit, by index.
    function = 0,
    /// A data segment of the unit, by index; the site's addend is the
    /// offset in the segment.
    data = 1,
    /// A signature of the unit, by index.
    type = 2,
    /// An import of the unit, by index.
    import = 3,
    /// A global the image defines, by index.
    global = 4,
};

/// What an import names.
pub const ImportKind = enum(u8) {
    function = 0,
    global = 1,
    table = 2,
    data = 3,
};

/// `Function.flags` bit: the function runs a stack check before anything
/// else, so the check covers every call chain it starts.
pub const function_checked: u32 = 1;
/// `FunctionExport.flags`, `DataExport.flags` and `Global.flags` bit: the
/// definition is weak, so a strong definition of the same name wins.
pub const definition_weak: u32 = 2;
/// `FunctionExport.flags` bit: the object the image was converted from
/// marks the function exported, so a finished module exports it.
pub const export_object_exported: u32 = 4;
/// `Import.flags` bit: a table site names the import, so the function it
/// resolves to needs a table slot.
pub const import_address_taken: u8 = 1;
/// `Segment.flags` bit: the segment is zero-filled memory with no bytes.
pub const segment_zero_fill: u32 = 1;
/// `Header.flags` bit: the unit's code checks stack bounds, and every
/// function carries its stack frame facts.
pub const image_stack_checks: u32 = 1;
/// `Import.type` of an import whose signature the unit does not know: a
/// function only named by a table slot in a data segment.
pub const no_type: u32 = std.math.maxInt(u32);

/// One function signature: its value types, parameters then results, in
/// the value-type blob.
pub const Type = extern struct {
    first_valtype: u32,
    param_count: u32,
    result_count: u32,
    reserved: u32 = 0,
};

/// One defined function.
pub const Function = extern struct {
    /// The body (locals declaration, then code) in the code blob.
    body_offset: u32,
    body_size: u32,
    type: u32,
    /// Offset of a name for it in the string table, for a module's name
    /// section; 0 when it has none.
    name: u32,
    first_site: u32,
    site_count: u32,
    /// How far below its entry stack pointer its own frame reaches.
    frame_depth: u32,
    first_call: u32,
    call_count: u32,
    /// `function_checked`.
    flags: u32,
};

/// One data segment.
pub const Segment = extern struct {
    /// Its bytes in the data blob; none for a zero-fill segment.
    bytes_offset: u32,
    size: u32,
    alignment_log2: u32,
    /// `segment_zero_fill`.
    flags: u32,
    first_site: u32,
    site_count: u32,
};

/// One field to fill once the unit is placed.
pub const Site = extern struct {
    /// Offset of the field in its function's body or its segment's bytes.
    offset: u32,
    target: u32,
    /// Added to the target's address or slot; for a data target, the
    /// offset in the segment is folded in.
    addend: i32,
    kind: u8,
    target_kind: u8,
    reserved: u16 = 0,
};

/// One call and the stack depth below its caller's entry stack pointer
/// that its callee starts at. A `type` target is a `call_indirect` of that
/// signature.
pub const Call = extern struct {
    target: u32,
    depth: u32,
    target_kind: u8,
    reserved: [3]u8 = .{ 0, 0, 0 },
};

/// One exported function symbol: a function can be exported under more
/// than one name.
pub const FunctionExport = extern struct {
    name: u32,
    function: u32,
    /// `definition_weak`, `export_object_exported`.
    flags: u32,
};

/// One exported data symbol.
pub const DataExport = extern struct {
    name: u32,
    segment: u32,
    offset: u32,
    size: u32,
    /// `definition_weak`.
    flags: u32,
};

/// One global the image defines, with the constant it starts as.
pub const Global = extern struct {
    /// The initial value's bits: an `i32` sign-extended, an `i64`, or a
    /// float's bits.
    init: u64,
    /// Offset of its exported name in the string table; 0 when the global
    /// is not exported.
    name: u32,
    valtype: u8,
    mutable: u8,
    /// `definition_weak`.
    flags: u8,
    reserved: u8 = 0,
};

/// One constructor the program runs at start, in priority order.
pub const InitFunc = extern struct {
    priority: u32,
    function: u32,
};

/// One name the unit references and no unit symbol defines.
pub const Import = extern struct {
    /// Offsets of the import's module name, symbol name and field name in
    /// the string table. A link resolves the import by its symbol name;
    /// an import left for the embedder keeps its module and field.
    module: u32,
    name: u32,
    field: u32,
    /// For a function: its signature, or `no_type`.
    type: u32,
    kind: u8,
    /// For a global: its value type and whether it is mutable.
    valtype: u8,
    mutable: u8,
    /// `import_address_taken`.
    flags: u8,
};

const Header = extern struct {
    magic: [4]u8,
    version: u32,
    flags: u32,
    types_offset: u32,
    type_count: u32,
    valtypes_offset: u32,
    valtypes_size: u32,
    functions_offset: u32,
    function_count: u32,
    code_offset: u32,
    code_size: u32,
    segments_offset: u32,
    segment_count: u32,
    data_offset: u32,
    data_size: u32,
    sites_offset: u32,
    site_count: u32,
    calls_offset: u32,
    call_count: u32,
    /// The exported function symbols, in name order.
    exports_offset: u32,
    export_count: u32,
    /// The exported data symbols, in name order.
    data_exports_offset: u32,
    data_export_count: u32,
    /// The functions whose table slots the unit takes, ascending.
    table_requests_offset: u32,
    table_request_count: u32,
    imports_offset: u32,
    import_count: u32,
    globals_offset: u32,
    global_count: u32,
    /// The constructors, in the order they run.
    init_funcs_offset: u32,
    init_func_count: u32,
    /// Atoms a link keeps whether or not anything reaches them: functions
    /// by index, then segments numbered after the functions. Ascending.
    roots_offset: u32,
    root_count: u32,
    strings_offset: u32,
    strings_size: u32,
    total_size: u32,
    /// BLAKE3 of the image's bytes with this field zeroed.
    digest: [32]u8,
};

comptime {
    std.debug.assert(@sizeOf(Header) % 8 == 0);
    std.debug.assert(@sizeOf(Site) == 16);
}

/// Why bytes are not an image this compiler wrote.
pub const ReadError = error{InvalidImage};

/// Read access to an encoded image, borrowing its bytes.
pub const View = struct {
    bytes: []align(alignment.toByteUnits()) const u8,
    header: Header,

    /// Read `bytes` as an image, checking every table and every index it
    /// holds, so a reader can trust each one: bytes that do not pass are
    /// not an image this compiler wrote.
    pub fn init(bytes: []align(alignment.toByteUnits()) const u8) ReadError!View {
        if (bytes.len < @sizeOf(Header)) return error.InvalidImage;
        const header = std.mem.bytesToValue(Header, bytes[0..@sizeOf(Header)]);
        if (!std.mem.eql(u8, &header.magic, &magic)) return error.InvalidImage;
        if (header.version != format_version) return error.InvalidImage;
        if (header.total_size != bytes.len) return error.InvalidImage;
        if (header.flags & ~image_stack_checks != 0) return error.InvalidImage;
        try checkTable(bytes.len, header.types_offset, header.type_count, @sizeOf(Type));
        try checkRange(bytes.len, header.valtypes_offset, header.valtypes_size, 1);
        try checkTable(bytes.len, header.functions_offset, header.function_count, @sizeOf(Function));
        try checkRange(bytes.len, header.code_offset, header.code_size, 1);
        try checkTable(bytes.len, header.segments_offset, header.segment_count, @sizeOf(Segment));
        try checkRange(bytes.len, header.data_offset, header.data_size, 1);
        try checkTable(bytes.len, header.sites_offset, header.site_count, @sizeOf(Site));
        try checkTable(bytes.len, header.calls_offset, header.call_count, @sizeOf(Call));
        try checkTable(bytes.len, header.exports_offset, header.export_count, @sizeOf(FunctionExport));
        try checkTable(bytes.len, header.data_exports_offset, header.data_export_count, @sizeOf(DataExport));
        try checkTable(bytes.len, header.table_requests_offset, header.table_request_count, @sizeOf(u32));
        try checkTable(bytes.len, header.imports_offset, header.import_count, @sizeOf(Import));
        try checkTable(bytes.len, header.globals_offset, header.global_count, @sizeOf(Global));
        try checkTable(bytes.len, header.init_funcs_offset, header.init_func_count, @sizeOf(InitFunc));
        try checkTable(bytes.len, header.roots_offset, header.root_count, @sizeOf(u32));
        try checkRange(bytes.len, header.strings_offset, header.strings_size, 1);
        if (header.strings_size == 0 or bytes[header.strings_offset] != 0 or bytes[header.strings_offset + header.strings_size - 1] != 0) return error.InvalidImage;
        const view: View = .{ .bytes = bytes, .header = header };
        try view.validate();
        return view;
    }

    fn validate(self: View) ReadError!void {
        const valtype_bytes = self.valtypes();
        for (self.types()) |signature| {
            const count = std.math.add(u32, signature.param_count, signature.result_count) catch return error.InvalidImage;
            try checkRange(valtype_bytes.len, signature.first_valtype, count, 1);
            for (valtype_bytes[signature.first_valtype..][0..count]) |valtype| if (!validValType(valtype)) return error.InvalidImage;
        }
        const code_bytes = self.code();
        const all_sites = self.sites();
        const all_calls = self.calls();
        var next_site: u32 = 0;
        var next_call: u32 = 0;
        for (self.functions()) |function| {
            try checkRange(code_bytes.len, function.body_offset, function.body_size, 1);
            if (function.type >= self.header.type_count) return error.InvalidImage;
            if (function.flags & ~function_checked != 0) return error.InvalidImage;
            try self.checkName(function.name);
            if (function.first_site != next_site) return error.InvalidImage;
            try checkRange(all_sites.len, function.first_site, function.site_count, 1);
            next_site += function.site_count;
            const function_body = code_bytes[function.body_offset..][0..function.body_size];
            for (all_sites[function.first_site..][0..function.site_count]) |site| try self.checkSite(site, function_body, false);
            if (function.first_call != next_call) return error.InvalidImage;
            try checkRange(all_calls.len, function.first_call, function.call_count, 1);
            next_call += function.call_count;
            if (self.header.flags & image_stack_checks == 0 and (function.call_count != 0 or function.frame_depth != 0 or function.flags & function_checked != 0)) return error.InvalidImage;
            for (all_calls[function.first_call..][0..function.call_count]) |call| {
                const kind = std.enums.fromInt(TargetKind, call.target_kind) orelse return error.InvalidImage;
                try self.checkTarget(kind, call.target);
                switch (kind) {
                    .function, .type => {},
                    .import => if (self.imports()[call.target].kind != @backingInt(ImportKind.function)) return error.InvalidImage,
                    .data, .global => return error.InvalidImage,
                }
            }
        }
        if (next_call != all_calls.len) return error.InvalidImage;
        const data_bytes = self.data();
        for (self.segments()) |segment| {
            if (segment.alignment_log2 >= 32) return error.InvalidImage;
            if (segment.flags & ~segment_zero_fill != 0) return error.InvalidImage;
            const zero_fill = segment.flags & segment_zero_fill != 0;
            if (zero_fill and (segment.bytes_offset != 0 or segment.site_count != 0)) return error.InvalidImage;
            if (!zero_fill) try checkRange(data_bytes.len, segment.bytes_offset, segment.size, 1);
            if (segment.first_site != next_site) return error.InvalidImage;
            try checkRange(all_sites.len, segment.first_site, segment.site_count, 1);
            next_site += segment.site_count;
            const bytes = if (zero_fill) &[_]u8{} else data_bytes[segment.bytes_offset..][0..segment.size];
            for (all_sites[segment.first_site..][0..segment.site_count]) |site| try self.checkSite(site, bytes, true);
        }
        if (next_site != all_sites.len) return error.InvalidImage;
        const functions_ = self.functions();
        var previous_name: ?[]const u8 = null;
        for (self.exports()) |function_export| {
            if (function_export.function >= functions_.len) return error.InvalidImage;
            if (function_export.flags & ~(definition_weak | export_object_exported) != 0) return error.InvalidImage;
            try self.checkName(function_export.name);
            if (function_export.name == 0) return error.InvalidImage;
            const export_name = self.name(function_export.name);
            if (previous_name) |previous| if (std.mem.order(u8, previous, export_name) != .lt) return error.InvalidImage;
            previous_name = export_name;
        }
        previous_name = null;
        for (self.dataExports()) |data_export| {
            try self.checkName(data_export.name);
            if (data_export.name == 0 or data_export.segment >= self.header.segment_count) return error.InvalidImage;
            if (data_export.flags & ~definition_weak != 0) return error.InvalidImage;
            const segment = self.segments()[data_export.segment];
            const end = std.math.add(u32, data_export.offset, data_export.size) catch return error.InvalidImage;
            if (end > segment.size) return error.InvalidImage;
            const export_name = self.name(data_export.name);
            if (previous_name) |previous| if (std.mem.order(u8, previous, export_name) != .lt) return error.InvalidImage;
            previous_name = export_name;
        }
        var previous_request: ?u32 = null;
        for (self.tableRequests()) |function_index| {
            if (function_index >= functions_.len) return error.InvalidImage;
            if (previous_request) |previous| if (previous >= function_index) return error.InvalidImage;
            previous_request = function_index;
        }
        for (self.globals()) |global| {
            try self.checkName(global.name);
            if (!validValType(global.valtype) or global.mutable > 1 or global.flags & ~definition_weak != 0) return error.InvalidImage;
        }
        var previous_root: ?u32 = null;
        for (self.roots()) |atom| {
            if (atom >= self.header.function_count + self.header.segment_count) return error.InvalidImage;
            if (previous_root) |previous| if (previous >= atom) return error.InvalidImage;
            previous_root = atom;
        }
        var previous_priority: u32 = 0;
        for (self.initFuncs()) |init_func| {
            if (init_func.function >= functions_.len or init_func.priority < previous_priority) return error.InvalidImage;
            previous_priority = init_func.priority;
        }
        for (self.imports()) |import| {
            try self.checkName(import.module);
            try self.checkName(import.name);
            try self.checkName(import.field);
            if (import.name == 0 or import.field == 0) return error.InvalidImage;
            if (import.flags & ~import_address_taken != 0) return error.InvalidImage;
            const kind = std.enums.fromInt(ImportKind, import.kind) orelse return error.InvalidImage;
            switch (kind) {
                .function => if (import.type != no_type and import.type >= self.header.type_count) return error.InvalidImage,
                .global => if (import.type != no_type or !validValType(import.valtype) or import.mutable > 1) return error.InvalidImage,
                .table, .data => if (import.type != no_type) return error.InvalidImage,
            }
        }
    }

    fn checkName(self: View, offset: u32) ReadError!void {
        if (offset >= self.header.strings_size) return error.InvalidImage;
    }

    fn checkTarget(self: View, kind: TargetKind, target: u32) ReadError!void {
        const count = switch (kind) {
            .function => self.header.function_count,
            .data => self.header.segment_count,
            .type => self.header.type_count,
            .import => self.header.import_count,
            .global => self.header.global_count,
        };
        if (target >= count) return error.InvalidImage;
    }

    fn checkSite(self: View, site: Site, bytes: []const u8, in_data: bool) ReadError!void {
        const kind = std.enums.fromInt(SiteKind, site.kind) orelse return error.InvalidImage;
        const target_kind = std.enums.fromInt(TargetKind, site.target_kind) orelse return error.InvalidImage;
        if (kind.inData() != in_data) return error.InvalidImage;
        try checkRange(bytes.len, site.offset, kind.fieldSize(), 1);
        try self.checkTarget(target_kind, site.target);
        const field = bytes[site.offset..][0..kind.fieldSize()];
        if (!in_data) {
            for (field[0..4]) |byte| if (byte & 0x80 == 0) return error.InvalidImage;
            if (field[4] & 0x80 != 0) return error.InvalidImage;
        }
        const import_kind: ?ImportKind = if (target_kind == .import) std.enums.fromInt(ImportKind, self.imports()[site.target].kind) orelse return error.InvalidImage else null;
        const allowed = switch (kind) {
            .function_index_leb, .table_index_sleb, .table_index_rel_sleb, .table_index_i32 => target_kind == .function or import_kind == .function,
            .type_index_leb => target_kind == .type,
            .memory_addr_leb, .memory_addr_sleb, .memory_addr_rel_sleb, .memory_addr_i32 => target_kind == .data or import_kind == .data,
            .global_index_leb => target_kind == .global or import_kind == .global,
            .table_number_leb => import_kind == .table,
            .stack_room_sleb => import_kind == .data,
        };
        if (!allowed) return error.InvalidImage;
    }

    /// The image's content digest.
    pub fn digest(self: View) [32]u8 {
        return self.header.digest;
    }
    /// Whether the unit's code checks stack bounds (`image_stack_checks`).
    pub fn stackChecks(self: View) bool {
        return self.header.flags & image_stack_checks != 0;
    }
    pub fn types(self: View) []const Type {
        return table(Type, self.bytes, self.header.types_offset, self.header.type_count);
    }
    pub fn valtypes(self: View) []const u8 {
        return self.bytes[self.header.valtypes_offset..][0..self.header.valtypes_size];
    }
    /// The parameter types of `signature`.
    pub fn params(self: View, signature: Type) []const u8 {
        return self.valtypes()[signature.first_valtype..][0..signature.param_count];
    }
    /// The result types of `signature`.
    pub fn results(self: View, signature: Type) []const u8 {
        return self.valtypes()[signature.first_valtype + signature.param_count ..][0..signature.result_count];
    }
    pub fn functions(self: View) []const Function {
        return table(Function, self.bytes, self.header.functions_offset, self.header.function_count);
    }
    pub fn code(self: View) []const u8 {
        return self.bytes[self.header.code_offset..][0..self.header.code_size];
    }
    /// The body of `function`: its locals declaration, then its code.
    pub fn body(self: View, function: Function) []const u8 {
        return self.code()[function.body_offset..][0..function.body_size];
    }
    pub fn segments(self: View) []const Segment {
        return table(Segment, self.bytes, self.header.segments_offset, self.header.segment_count);
    }
    pub fn data(self: View) []const u8 {
        return self.bytes[self.header.data_offset..][0..self.header.data_size];
    }
    /// The bytes of `segment`; empty for a zero-fill segment.
    pub fn segmentBytes(self: View, segment: Segment) []const u8 {
        if (segment.flags & segment_zero_fill != 0) return &.{};
        return self.data()[segment.bytes_offset..][0..segment.size];
    }
    /// Every site: each function's in function order, then each segment's.
    pub fn sites(self: View) []const Site {
        return table(Site, self.bytes, self.header.sites_offset, self.header.site_count);
    }
    pub fn functionSites(self: View, function: Function) []const Site {
        return self.sites()[function.first_site..][0..function.site_count];
    }
    pub fn segmentSites(self: View, segment: Segment) []const Site {
        return self.sites()[segment.first_site..][0..segment.site_count];
    }
    pub fn calls(self: View) []const Call {
        return table(Call, self.bytes, self.header.calls_offset, self.header.call_count);
    }
    pub fn functionCalls(self: View, function: Function) []const Call {
        return self.calls()[function.first_call..][0..function.call_count];
    }
    /// The exported function symbols, in name order.
    pub fn exports(self: View) []const FunctionExport {
        return table(FunctionExport, self.bytes, self.header.exports_offset, self.header.export_count);
    }
    /// The atoms a link keeps whether or not anything reaches them.
    pub fn roots(self: View) []const u32 {
        return table(u32, self.bytes, self.header.roots_offset, self.header.root_count);
    }
    /// The exported data symbols, in name order.
    pub fn dataExports(self: View) []const DataExport {
        return table(DataExport, self.bytes, self.header.data_exports_offset, self.header.data_export_count);
    }
    /// The functions whose table slots the unit takes, ascending.
    pub fn tableRequests(self: View) []const u32 {
        return table(u32, self.bytes, self.header.table_requests_offset, self.header.table_request_count);
    }
    pub fn imports(self: View) []const Import {
        return table(Import, self.bytes, self.header.imports_offset, self.header.import_count);
    }
    /// The globals the image defines.
    pub fn globals(self: View) []const Global {
        return table(Global, self.bytes, self.header.globals_offset, self.header.global_count);
    }
    /// The constructors, in the order they run.
    pub fn initFuncs(self: View) []const InitFunc {
        return table(InitFunc, self.bytes, self.header.init_funcs_offset, self.header.init_func_count);
    }
    /// The string table: NUL-terminated names, starting with an empty one.
    pub fn strings(self: View) []const u8 {
        return self.bytes[self.header.strings_offset..][0..self.header.strings_size];
    }
    pub fn name(self: View, offset: u32) []const u8 {
        return std.mem.sliceTo(self.strings()[offset..], 0);
    }

    /// The index of the exported function named `wanted`, if any.
    pub fn findExport(self: View, wanted: []const u8) ?u32 {
        const order = self.exports();
        var low: usize = 0;
        var high: usize = order.len;
        while (low < high) {
            const mid = low + (high - low) / 2;
            switch (std.mem.order(u8, self.name(order[mid].name), wanted)) {
                .lt => low = mid + 1,
                .gt => high = mid,
                .eq => return order[mid].function,
            }
        }
        return null;
    }

    /// The index of the global exported as `wanted`, if any.
    pub fn findGlobalExport(self: View, wanted: []const u8) ?u32 {
        for (self.globals(), 0..) |global, index| {
            if (global.name != 0 and std.mem.eql(u8, self.name(global.name), wanted)) return @intCast(index);
        }
        return null;
    }

    /// The exported data symbol named `wanted`, if any.
    pub fn findDataExport(self: View, wanted: []const u8) ?DataExport {
        const all = self.dataExports();
        var low: usize = 0;
        var high: usize = all.len;
        while (low < high) {
            const mid = low + (high - low) / 2;
            switch (std.mem.order(u8, self.name(all[mid].name), wanted)) {
                .lt => low = mid + 1,
                .gt => high = mid,
                .eq => return all[mid],
            }
        }
        return null;
    }
};

fn validValType(valtype: u8) bool {
    return switch (valtype) {
        0x7F, 0x7E, 0x7D, 0x7C, 0x7B => true,
        else => false,
    };
}

fn checkRange(len: usize, offset: u32, count: u32, size: usize) ReadError!void {
    const bytes = std.math.mul(usize, count, size) catch return error.InvalidImage;
    const end = std.math.add(usize, offset, bytes) catch return error.InvalidImage;
    if (end > len) return error.InvalidImage;
}

fn checkTable(len: usize, offset: u32, count: u32, size: usize) ReadError!void {
    if (offset % 8 != 0) return error.InvalidImage;
    try checkRange(len, offset, count, size);
}

fn table(comptime T: type, bytes: []align(alignment.toByteUnits()) const u8, offset: u32, count: u32) []const T {
    const start: [*]const u8 = bytes.ptr + offset;
    const typed: [*]const T = @ptrCast(@alignCast(start));
    return typed[0..count];
}

/// What an image is built from. Every index is into the input's own
/// tables, which become the image's in the same order.
pub const Input = struct {
    /// Whether the unit's code checks stack bounds; every function then
    /// carries its frame facts.
    stack_checks: bool,
    types: []const InputType,
    functions: []const InputFunction,
    segments: []const InputSegment,
    data_exports: []const InputDataExport,
    /// Ascending, without repeats.
    table_requests: []const u32,
    imports: []const InputImport,
    globals: []const InputGlobal = &.{},
    /// In the order they run: priorities ascending.
    init_funcs: []const InitFunc = &.{},
    /// Function exports beyond each function's `export_name`.
    aliases: []const InputFunctionExport = &.{},
    /// Ascending, without repeats: functions by index, then segments
    /// numbered after the functions.
    roots: []const u32 = &.{},
};

/// A name a function is exported under.
pub const InputFunctionExport = struct {
    name: []const u8,
    function: u32,
    weak: bool = false,
    object_exported: bool = false,
};

/// A global the image defines, its initial value's bits, and its exported
/// name if it has one.
pub const InputGlobal = struct {
    valtype: u8,
    mutable: bool,
    init: u64,
    export_name: ?[]const u8,
    weak: bool = false,
};

/// A function signature: parameter and result value types, as the type
/// section encodes them.
pub const InputType = struct {
    params: []const u8,
    results: []const u8,
};

/// One padded field in a body or a segment that the composer fills: what
/// it names and how it encodes it.
pub const InputSite = struct {
    offset: u32,
    kind: SiteKind,
    target_kind: TargetKind,
    target: u32,
    addend: i32,
};

/// A call a function makes and the stack depth it starts its callee at,
/// for the stack-room analysis of platforms with stack checks.
pub const InputCall = struct {
    target_kind: TargetKind,
    target: u32,
    depth: u32,
};

/// One function of the unit: its body as the code section stores it, its
/// signature, its export name, its sites, and its stack frame facts.
pub const InputFunction = struct {
    body: []const u8,
    type: u32,
    /// The name it is exported under, if any.
    export_name: ?[]const u8,
    sites: []const InputSite,
    /// A name for a module's name section when it is not exported.
    name: ?[]const u8 = null,
    frame_depth: u32 = 0,
    calls: []const InputCall = &.{},
    checked: bool = false,
};

/// One data segment of the unit: its bytes or its zero-fill size, its
/// alignment, and the sites in it.
pub const InputSegment = struct {
    /// Null for a zero-fill segment of `size` bytes.
    bytes: ?[]const u8,
    size: u32,
    alignment_log2: u32,
    sites: []const InputSite,
};

/// A data symbol the unit exports: a range of one of its segments.
pub const InputDataExport = struct {
    name: []const u8,
    segment: u32,
    offset: u32,
    size: u32,
    weak: bool = false,
};

/// A symbol the unit imports by module and name: a function with its
/// signature, a global with its type, a table, or memory.
pub const InputImport = struct {
    module: []const u8,
    name: []const u8,
    /// The field an embedder import keeps; null when it is the symbol name.
    field: ?[]const u8 = null,
    kind: ImportKind,
    /// For a function: its signature, or `no_type`.
    type: u32 = no_type,
    /// For a global.
    valtype: u8 = 0,
    mutable: bool = false,
    address_taken: bool = false,
};

/// Why an input could not be encoded.
pub const EncodeError = Allocator.Error || error{ImageTooLarge};

/// Encode `input` as an image. The caller owns the result.
pub fn encode(allocator: Allocator, input: Input) EncodeError![]align(alignment.toByteUnits()) u8 {
    var arena_state = std.heap.ArenaAllocator.init(allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    var strings = Strings{ .arena = arena };
    try strings.bytes.append(arena, 0);

    const types_out = try arena.alloc(Type, input.types.len);
    var valtypes_out: std.ArrayList(u8) = .empty;
    for (input.types, types_out) |signature, *out| {
        out.* = .{ .first_valtype = try count32(valtypes_out.items.len), .param_count = try count32(signature.params.len), .result_count = try count32(signature.results.len) };
        try valtypes_out.appendSlice(arena, signature.params);
        try valtypes_out.appendSlice(arena, signature.results);
    }

    var code_out: std.ArrayList(u8) = .empty;
    var sites_out: std.ArrayList(Site) = .empty;
    var calls_out: std.ArrayList(Call) = .empty;
    const functions_out = try arena.alloc(Function, input.functions.len);
    var exports_out: std.ArrayList(FunctionExport) = .empty;
    var export_names: std.ArrayList([]const u8) = .empty;
    for (input.functions, functions_out, 0..) |function, *out, index| {
        out.* = .{
            .body_offset = try count32(code_out.items.len),
            .body_size = try count32(function.body.len),
            .type = function.type,
            .name = if (function.export_name orelse function.name) |name_text| try strings.add(name_text) else 0,
            .first_site = try count32(sites_out.items.len),
            .site_count = try count32(function.sites.len),
            .frame_depth = function.frame_depth,
            .first_call = try count32(calls_out.items.len),
            .call_count = try count32(function.calls.len),
            .flags = if (function.checked) function_checked else 0,
        };
        try code_out.appendSlice(arena, function.body);
        for (function.sites) |site| try sites_out.append(arena, siteRecord(site));
        for (function.calls) |call| try calls_out.append(arena, .{ .target = call.target, .depth = call.depth, .target_kind = @backingInt(call.target_kind) });
        if (function.export_name) |export_name| {
            try exports_out.append(arena, .{ .name = try strings.add(export_name), .function = @intCast(index), .flags = 0 });
            try export_names.append(arena, export_name);
        }
    }
    for (input.aliases) |alias| {
        try exports_out.append(arena, .{
            .name = try strings.add(alias.name),
            .function = alias.function,
            .flags = (if (alias.weak) definition_weak else 0) | (if (alias.object_exported) export_object_exported else 0),
        });
        try export_names.append(arena, alias.name);
    }
    const export_order = try arena.alloc(u32, exports_out.items.len);
    for (export_order, 0..) |*slot, index| slot.* = @intCast(index);
    const ByName = struct {
        names: []const []const u8,
        fn lessThan(context: @This(), a: u32, b: u32) bool {
            return std.mem.order(u8, context.names[a], context.names[b]) == .lt;
        }
    };
    std.mem.sort(u32, export_order, ByName{ .names = export_names.items }, ByName.lessThan);
    const exports_sorted = try arena.alloc(FunctionExport, exports_out.items.len);
    for (export_order, exports_sorted) |index, *out| out.* = exports_out.items[index];

    var data_out: std.ArrayList(u8) = .empty;
    const segments_out = try arena.alloc(Segment, input.segments.len);
    for (input.segments, segments_out) |segment, *out| {
        out.* = .{
            .bytes_offset = if (segment.bytes != null) try count32(data_out.items.len) else 0,
            .size = segment.size,
            .alignment_log2 = segment.alignment_log2,
            .flags = if (segment.bytes == null) segment_zero_fill else 0,
            .first_site = try count32(sites_out.items.len),
            .site_count = try count32(segment.sites.len),
        };
        if (segment.bytes) |bytes| {
            std.debug.assert(bytes.len == segment.size);
            try data_out.appendSlice(arena, bytes);
        }
        for (segment.sites) |site| try sites_out.append(arena, siteRecord(site));
    }

    const data_exports_out = try arena.alloc(DataExport, input.data_exports.len);
    const data_export_order = try arena.alloc(u32, input.data_exports.len);
    for (data_export_order, 0..) |*slot, index| slot.* = @intCast(index);
    const DataByName = struct {
        names: []const InputDataExport,
        fn lessThan(context: @This(), a: u32, b: u32) bool {
            return std.mem.order(u8, context.names[a].name, context.names[b].name) == .lt;
        }
    };
    std.mem.sort(u32, data_export_order, DataByName{ .names = input.data_exports }, DataByName.lessThan);
    for (data_export_order, data_exports_out) |index, *out| {
        const data_export = input.data_exports[index];
        out.* = .{ .name = try strings.add(data_export.name), .segment = data_export.segment, .offset = data_export.offset, .size = data_export.size, .flags = if (data_export.weak) definition_weak else 0 };
    }

    const imports_out = try arena.alloc(Import, input.imports.len);
    for (input.imports, imports_out) |import, *out| out.* = .{
        .module = try strings.add(import.module),
        .name = try strings.add(import.name),
        .field = try strings.add(import.field orelse import.name),
        .type = import.type,
        .kind = @backingInt(import.kind),
        .valtype = import.valtype,
        .mutable = @intFromBool(import.mutable),
        .flags = if (import.address_taken) import_address_taken else 0,
    };

    const globals_out = try arena.alloc(Global, input.globals.len);
    for (input.globals, globals_out) |global, *out| out.* = .{
        .init = global.init,
        .name = if (global.export_name) |export_name| try strings.add(export_name) else 0,
        .valtype = global.valtype,
        .mutable = @intFromBool(global.mutable),
        .flags = if (global.weak) @intCast(definition_weak) else 0,
    };

    // Tables first, each 8-aligned, then the blobs.
    var size: usize = @sizeOf(Header);
    const types_offset = place(&size, types_out.len * @sizeOf(Type));
    const functions_offset = place(&size, functions_out.len * @sizeOf(Function));
    const segments_offset = place(&size, segments_out.len * @sizeOf(Segment));
    const sites_offset = place(&size, sites_out.items.len * @sizeOf(Site));
    const calls_offset = place(&size, calls_out.items.len * @sizeOf(Call));
    const exports_offset = place(&size, exports_sorted.len * @sizeOf(FunctionExport));
    const data_exports_offset = place(&size, data_exports_out.len * @sizeOf(DataExport));
    const table_requests_offset = place(&size, input.table_requests.len * @sizeOf(u32));
    const imports_offset = place(&size, imports_out.len * @sizeOf(Import));
    const globals_offset = place(&size, globals_out.len * @sizeOf(Global));
    const init_funcs_offset = place(&size, input.init_funcs.len * @sizeOf(InitFunc));
    const roots_offset = place(&size, input.roots.len * @sizeOf(u32));
    const valtypes_offset = place(&size, valtypes_out.items.len);
    const code_offset = place(&size, code_out.items.len);
    const data_offset = place(&size, data_out.items.len);
    const strings_offset = place(&size, strings.bytes.items.len);
    if (size > std.math.maxInt(u32)) return error.ImageTooLarge;

    const out = try allocator.alignedAlloc(u8, alignment, size);
    errdefer allocator.free(out);
    @memset(out, 0);
    const header: Header = .{
        .magic = magic,
        .version = format_version,
        .flags = if (input.stack_checks) image_stack_checks else 0,
        .types_offset = types_offset,
        .type_count = try count32(types_out.len),
        .valtypes_offset = valtypes_offset,
        .valtypes_size = try count32(valtypes_out.items.len),
        .functions_offset = functions_offset,
        .function_count = try count32(functions_out.len),
        .code_offset = code_offset,
        .code_size = try count32(code_out.items.len),
        .segments_offset = segments_offset,
        .segment_count = try count32(segments_out.len),
        .data_offset = data_offset,
        .data_size = try count32(data_out.items.len),
        .sites_offset = sites_offset,
        .site_count = try count32(sites_out.items.len),
        .calls_offset = calls_offset,
        .call_count = try count32(calls_out.items.len),
        .exports_offset = exports_offset,
        .export_count = try count32(exports_sorted.len),
        .data_exports_offset = data_exports_offset,
        .data_export_count = try count32(data_exports_out.len),
        .table_requests_offset = table_requests_offset,
        .table_request_count = try count32(input.table_requests.len),
        .imports_offset = imports_offset,
        .import_count = try count32(imports_out.len),
        .globals_offset = globals_offset,
        .global_count = try count32(globals_out.len),
        .init_funcs_offset = init_funcs_offset,
        .init_func_count = try count32(input.init_funcs.len),
        .roots_offset = roots_offset,
        .root_count = try count32(input.roots.len),
        .strings_offset = strings_offset,
        .strings_size = try count32(strings.bytes.items.len),
        .total_size = @intCast(size),
        .digest = @splat(0),
    };
    @memcpy(out[0..@sizeOf(Header)], std.mem.asBytes(&header));
    @memcpy(out[types_offset..][0 .. types_out.len * @sizeOf(Type)], std.mem.sliceAsBytes(types_out));
    @memcpy(out[functions_offset..][0 .. functions_out.len * @sizeOf(Function)], std.mem.sliceAsBytes(functions_out));
    @memcpy(out[segments_offset..][0 .. segments_out.len * @sizeOf(Segment)], std.mem.sliceAsBytes(segments_out));
    @memcpy(out[sites_offset..][0 .. sites_out.items.len * @sizeOf(Site)], std.mem.sliceAsBytes(sites_out.items));
    @memcpy(out[calls_offset..][0 .. calls_out.items.len * @sizeOf(Call)], std.mem.sliceAsBytes(calls_out.items));
    @memcpy(out[exports_offset..][0 .. exports_sorted.len * @sizeOf(FunctionExport)], std.mem.sliceAsBytes(exports_sorted));
    @memcpy(out[data_exports_offset..][0 .. data_exports_out.len * @sizeOf(DataExport)], std.mem.sliceAsBytes(data_exports_out));
    @memcpy(out[table_requests_offset..][0 .. input.table_requests.len * @sizeOf(u32)], std.mem.sliceAsBytes(input.table_requests));
    @memcpy(out[imports_offset..][0 .. imports_out.len * @sizeOf(Import)], std.mem.sliceAsBytes(imports_out));
    @memcpy(out[globals_offset..][0 .. globals_out.len * @sizeOf(Global)], std.mem.sliceAsBytes(globals_out));
    @memcpy(out[init_funcs_offset..][0 .. input.init_funcs.len * @sizeOf(InitFunc)], std.mem.sliceAsBytes(input.init_funcs));
    @memcpy(out[roots_offset..][0 .. input.roots.len * @sizeOf(u32)], std.mem.sliceAsBytes(input.roots));
    @memcpy(out[valtypes_offset..][0..valtypes_out.items.len], valtypes_out.items);
    @memcpy(out[code_offset..][0..code_out.items.len], code_out.items);
    @memcpy(out[data_offset..][0..data_out.items.len], data_out.items);
    @memcpy(out[strings_offset..][0..strings.bytes.items.len], strings.bytes.items);
    var content_digest: [32]u8 = undefined;
    std.crypto.hash.Blake3.hash(out, &content_digest, .{});
    @memcpy(out[@offsetOf(Header, "digest")..][0..32], &content_digest);
    return out;
}

fn siteRecord(site: InputSite) Site {
    return .{ .offset = site.offset, .target = site.target, .addend = site.addend, .kind = @backingInt(site.kind), .target_kind = @backingInt(site.target_kind) };
}

fn place(size: *usize, bytes: usize) u32 {
    size.* = std.mem.alignForward(usize, size.*, 8);
    const offset = size.*;
    size.* += bytes;
    // `encode` rejects an image past `u32` before writing any offset.
    return @truncate(offset);
}

fn count32(value: usize) error{ImageTooLarge}!u32 {
    return std.math.cast(u32, value) orelse error.ImageTooLarge;
}

/// The string table, each distinct name stored once.
const Strings = struct {
    arena: Allocator,
    bytes: std.ArrayList(u8) = .empty,
    offsets: std.StringHashMapUnmanaged(u32) = .empty,

    fn add(self: *Strings, text: []const u8) EncodeError!u32 {
        if (text.len == 0) return 0;
        const gop = try self.offsets.getOrPut(self.arena, text);
        if (gop.found_existing) return gop.value_ptr.*;
        const offset = try count32(self.bytes.items.len);
        gop.value_ptr.* = offset;
        try self.bytes.appendSlice(self.arena, text);
        try self.bytes.append(self.arena, 0);
        return offset;
    }
};

/// A padded LEB128 zero, as every code site's field holds.
const zero_leb = [5]u8{ 0x80, 0x80, 0x80, 0x80, 0x00 };

/// The test image's tables; `testInput` borrows them.
const TestTables = struct {
    functions: [2]InputFunction,
    segments: [2]InputSegment,
};

fn testInput(tables: *TestTables, body_a: []const u8, body_b: []const u8, segment: []const u8) Input {
    const S = struct {
        const types = [_]InputType{ .{ .params = &.{0x7F}, .results = &.{0x7F} }, .{ .params = &.{}, .results = &.{} } };
        const a_sites = [_]InputSite{
            .{ .offset = 1, .kind = .function_index_leb, .target_kind = .import, .target = 0, .addend = 0 },
            .{ .offset = 7, .kind = .memory_addr_sleb, .target_kind = .data, .target = 0, .addend = 4 },
        };
        const b_sites = [_]InputSite{.{ .offset = 1, .kind = .function_index_leb, .target_kind = .function, .target = 0, .addend = 0 }};
        const segment_sites = [_]InputSite{.{ .offset = 0, .kind = .table_index_i32, .target_kind = .function, .target = 1, .addend = 0 }};
        const imports = [_]InputImport{ .{ .module = "env", .name = "roc_alloc", .kind = .function, .type = 0 }, .{ .module = "env", .name = "__stack_pointer", .kind = .global, .valtype = 0x7F, .mutable = true } };
        const data_exports = [_]InputDataExport{.{ .name = "roc_answer", .segment = 1, .offset = 0, .size = 8 }};
        const requests = [_]u32{1};
    };
    tables.* = .{
        .functions = .{
            .{ .body = body_a, .type = 0, .export_name = "roc__main", .sites = &S.a_sites },
            .{ .body = body_b, .type = 1, .export_name = null, .sites = &S.b_sites },
        },
        .segments = .{
            .{ .bytes = segment, .size = @intCast(segment.len), .alignment_log2 = 2, .sites = &S.segment_sites },
            .{ .bytes = null, .size = 8, .alignment_log2 = 3, .sites = &.{} },
        },
    };
    return .{
        .stack_checks = false,
        .types = &S.types,
        .functions = &tables.functions,
        .segments = &tables.segments,
        .data_exports = &S.data_exports,
        .table_requests = &S.requests,
        .imports = &S.imports,
    };
}

fn testBodies() struct { a: [13]u8, b: [7]u8 } {
    var a: [13]u8 = undefined;
    a[0] = 0x00; // no locals
    @memcpy(a[1..6], &zero_leb);
    a[6] = 0x41; // i32.const
    @memcpy(a[7..12], &zero_leb);
    a[12] = 0x0B;
    var b: [7]u8 = undefined;
    b[0] = 0x00;
    @memcpy(b[1..6], &zero_leb);
    b[6] = 0x0B;
    return .{ .a = a, .b = b };
}

test "a wasm unit image reads back what it was built from" {
    const allocator = std.testing.allocator;
    const bodies = testBodies();
    var tables: TestTables = undefined;
    const segment = [_]u8{ 0, 0, 0, 0, 1, 2, 3, 4 };
    const bytes = try encode(allocator, testInput(&tables, &bodies.a, &bodies.b, &segment));
    defer allocator.free(bytes);
    const view = try View.init(bytes);
    try std.testing.expectEqual(@as(usize, 2), view.types().len);
    try std.testing.expectEqualSlices(u8, &.{0x7F}, view.params(view.types()[0]));
    try std.testing.expectEqualSlices(u8, &.{0x7F}, view.results(view.types()[0]));
    try std.testing.expectEqual(@as(usize, 2), view.functions().len);
    try std.testing.expectEqualSlices(u8, &bodies.a, view.body(view.functions()[0]));
    try std.testing.expectEqualSlices(u8, &bodies.b, view.body(view.functions()[1]));
    try std.testing.expectEqual(@as(?u32, 0), view.findExport("roc__main"));
    try std.testing.expectEqual(@as(?u32, null), view.findExport("roc__other"));
    const sites = view.functionSites(view.functions()[0]);
    try std.testing.expectEqual(@as(usize, 2), sites.len);
    try std.testing.expectEqual(@as(i32, 4), sites[1].addend);
    try std.testing.expectEqual(@backingInt(TargetKind.data), sites[1].target_kind);
    try std.testing.expectEqualSlices(u8, &segment, view.segmentBytes(view.segments()[0]));
    try std.testing.expectEqual(@as(usize, 0), view.segmentBytes(view.segments()[1]).len);
    try std.testing.expectEqual(@as(u32, 8), view.segments()[1].size);
    try std.testing.expectEqual(@as(usize, 1), view.segmentSites(view.segments()[0]).len);
    try std.testing.expectEqualSlices(u32, &.{1}, view.tableRequests());
    try std.testing.expectEqualStrings("roc_alloc", view.name(view.imports()[0].name));
    try std.testing.expectEqualStrings("env", view.name(view.imports()[0].module));
    try std.testing.expectEqual(@as(u32, 1), view.findDataExport("roc_answer").?.segment);

    // The encoding is a function of the input.
    const again = try encode(allocator, testInput(&tables, &bodies.a, &bodies.b, &segment));
    defer allocator.free(again);
    try std.testing.expectEqualSlices(u8, bytes, again);
    var zeroed: [32]u8 = @splat(0);
    try std.testing.expect(!std.mem.eql(u8, &zeroed, &view.digest()));
    zeroed = view.digest();
    const other_segment = [_]u8{ 0, 0, 0, 0, 1, 2, 3, 5 };
    const other = try encode(allocator, testInput(&tables, &bodies.a, &bodies.b, &other_segment));
    defer allocator.free(other);
    try std.testing.expect(!std.mem.eql(u8, &zeroed, &(try View.init(other)).digest()));
}

test "bytes that are not a wasm unit image are rejected" {
    const allocator = std.testing.allocator;
    const bodies = testBodies();
    var tables: TestTables = undefined;
    const segment = [_]u8{ 0, 0, 0, 0, 1, 2, 3, 4 };
    const bytes = try encode(allocator, testInput(&tables, &bodies.a, &bodies.b, &segment));
    defer allocator.free(bytes);
    const copy = try allocator.alignedAlloc(u8, alignment, bytes.len);
    defer allocator.free(copy);

    // Truncated.
    try std.testing.expectError(error.InvalidImage, View.init(bytes[0 .. bytes.len - 8]));
    // Another magic.
    @memcpy(copy, bytes);
    copy[0] = 'X';
    try std.testing.expectError(error.InvalidImage, View.init(copy));
    // A site whose target is past its table.
    @memcpy(copy, bytes);
    const header = std.mem.bytesToValue(Header, bytes[0..@sizeOf(Header)]);
    std.mem.writeInt(u32, copy[header.sites_offset + @offsetOf(Site, "target") ..][0..4], 9, .little);
    try std.testing.expectError(error.InvalidImage, View.init(copy));
    // A site whose field runs past its body.
    @memcpy(copy, bytes);
    std.mem.writeInt(u32, copy[header.sites_offset..][0..4], 10, .little);
    try std.testing.expectError(error.InvalidImage, View.init(copy));
    // A site field that is not a padded LEB128.
    @memcpy(copy, bytes);
    copy[header.code_offset + 1] = 0x00;
    try std.testing.expectError(error.InvalidImage, View.init(copy));
    // A function naming a signature the image does not have.
    @memcpy(copy, bytes);
    std.mem.writeInt(u32, copy[header.functions_offset + @offsetOf(Function, "type") ..][0..4], 7, .little);
    try std.testing.expectError(error.InvalidImage, View.init(copy));
    // A site kind that names the wrong kind of target.
    @memcpy(copy, bytes);
    copy[header.sites_offset + @offsetOf(Site, "kind")] = @backingInt(SiteKind.global_index_leb);
    try std.testing.expectError(error.InvalidImage, View.init(copy));
    // Frame facts in an image without stack checks.
    @memcpy(copy, bytes);
    std.mem.writeInt(u32, copy[header.functions_offset + @offsetOf(Function, "frame_depth") ..][0..4], 16, .little);
    try std.testing.expectError(error.InvalidImage, View.init(copy));
}
