//! Emit foreign-language locks from the actual canonical Zig declarations.
//! Native compilers lay these declarations out for each target in the ABI matrix.
const std = @import("std");
const builtins = @import("builtins");
const Ops = builtins.host_abi.RocOps;
const Language = enum { c, rust };

fn scalar(comptime T: type, comptime lang: Language) []const u8 {
    if (T == void) return if (lang == .c) "void" else "()";
    if (T == anyopaque) return if (lang == .c) "void" else "core::ffi::c_void";
    if (T == usize) return if (lang == .c) "size_t" else "usize";
    if (T == u8) return if (lang == .c) "uint8_t" else "u8";
    if (T == Ops) return if (lang == .c) "struct RocOps" else "RocHost";
    return switch (@typeInfo(T)) {
        .optional => |o| scalar(o.child, lang),
        .pointer => |p| if (lang == .c)
            (if (p.attrs.@"const") "const " else "") ++ scalar(p.child, lang) ++ "*"
        else
            (if (p.attrs.@"const") "*const " else "*mut ") ++ scalar(p.child, lang),
        .type, .void, .bool, .noreturn, .int, .float, .array, .@"struct", .comptime_float, .comptime_int, .undefined, .null, .error_union, .error_set, .@"enum", .@"union", .@"fn", .@"opaque", .frame, .@"anyframe", .vector, .enum_literal, .spirv => @compileError("add an explicit foreign ABI spelling for " ++ @typeName(T)),
    };
}

/// A callback's return type. Rust spells a non-optional pointer result as
/// `NonNull`, so a host that could return null fails to type-check.
fn returnType(comptime T: type, comptime lang: Language) []const u8 {
    if (lang == .rust and @typeInfo(T) == .pointer and !@typeInfo(T).pointer.attrs.@"const") {
        return "core::ptr::NonNull<" ++ scalar(@typeInfo(T).pointer.child, lang) ++ ">";
    }
    return scalar(T, lang);
}

fn declaration(comptime T: type, comptime name: []const u8, comptime lang: Language) []const u8 {
    if (@typeInfo(T) == .optional) return declaration(@typeInfo(T).optional.child, name, lang);
    if (@typeInfo(T) == .pointer and @typeInfo(@typeInfo(T).pointer.child) == .@"fn") {
        const f = @typeInfo(@typeInfo(T).pointer.child).@"fn";
        if (!std.meta.eql(f.attrs.@"callconv", std.builtin.CallingConvention.c)) @compileError("host callback must use C ABI");
        if (f.attrs.varargs) @compileError("host callbacks must have fixed arity");
        var args: []const u8 = "";
        for (f.param_types, 0..) |param_type, i| {
            if (i != 0) args = args ++ ", ";
            args = args ++ scalar(param_type.?, lang);
        }
        if (lang == .c and args.len == 0) args = "void";
        return if (lang == .c)
            returnType(f.return_type.?, lang) ++ " (*" ++ name ++ ")(" ++ args ++ ")"
        else
            name ++ ": extern \"C\" fn(" ++ args ++ ") -> " ++ returnType(f.return_type.?, lang);
    }
    return if (lang == .c) scalar(T, lang) ++ " " ++ name else name ++ ": " ++ scalar(T, lang);
}

// The foreign compilers apply natural C layout. Never silently discard an
// explicit Zig field-alignment contract; adding one requires an exact foreign
// representation here before the canonical mirror can be emitted.
fn requireNaturalExternLayout(comptime T: type) void {
    const info = @typeInfo(T).@"struct";
    if (info.layout != .@"extern") @compileError("foreign ABI lock requires an extern struct");
    for (info.field_names, info.field_attrs) |field_name, field_attrs| {
        if (field_attrs.@"align" != null) {
            @compileError("explicit alignment needs a foreign ABI spelling: " ++ @typeName(T) ++ "." ++ field_name);
        }
    }
}

fn lock(comptime T: type, comptime generated: []const u8, comptime canonical: []const u8, comptime names: []const []const u8, comptime lang: Language) []const u8 {
    requireNaturalExternLayout(T);
    const fields = @typeInfo(T).@"struct".field_names;
    if (fields.len != names.len) @compileError("canonical ABI field count changed: update the glue template and lock field mapping");
    for (fields, names) |field_name, name| {
        const canonical_name = if (std.mem.eql(u8, name, "elements")) "bytes" else name;
        if (!std.mem.eql(u8, field_name, canonical_name)) @compileError("canonical ABI field order changed");
    }
    var result: []const u8 = if (lang == .c) "typedef struct {\n" else "#[repr(C)]\nstruct " ++ canonical ++ " {\n";
    for (fields) |field_name| result = result ++ "    " ++ declaration(@FieldType(T, field_name), field_name, lang) ++ (if (lang == .c) ";\n" else ",\n");
    result = result ++ (if (lang == .c) "} " ++ canonical ++ ";\n" else "}\n");
    if (lang == .c) {
        result = result ++ "ROC_STATIC_ASSERT(sizeof(" ++ generated ++ ") == sizeof(" ++ canonical ++ "), \"canonical size mismatch\");\n";
        result = result ++ "ROC_STATIC_ASSERT(ROC_ALIGNOF(" ++ generated ++ ") == ROC_ALIGNOF(" ++ canonical ++ "), \"canonical alignment mismatch\");\n";
        for (fields, names) |field_name, name| {
            const canonical_name = if (std.mem.eql(u8, name, "elements")) "bytes" else name;
            result = result ++ "ROC_STATIC_ASSERT(offsetof(" ++ generated ++ ", " ++ name ++ ") == offsetof(" ++ canonical ++ ", " ++ canonical_name ++ "), \"canonical offset mismatch\");\n";
            result = result ++ "ROC_STATIC_ASSERT(sizeof(((" ++ generated ++ "*)0)->" ++ name ++ ") == sizeof(((" ++ canonical ++ "*)0)->" ++ canonical_name ++ "), \"canonical field size mismatch\");\n";
            if (std.mem.eql(u8, name, "elements")) {
                // C intentionally erases List(U8)'s element pointer to void*.
                if (@FieldType(T, field_name) != ?[*]u8) @compileError("canonical list element pointer changed");
                result = result ++ "ROC_STATIC_ASSERT(__builtin_types_compatible_p(__typeof__(((" ++ generated ++ "*)0)->elements), void*), \"list element pointer type mismatch\");\n";
            } else {
                result = result ++ "ROC_STATIC_ASSERT(__builtin_types_compatible_p(__typeof__(((" ++ generated ++ "*)0)->" ++ name ++ "), __typeof__(((" ++ canonical ++ "*)0)->" ++ canonical_name ++ ")), \"canonical field type mismatch\");\n";
            }
        }
    } else {
        result = result ++ "const _: () = assert!(core::mem::size_of::<" ++ generated ++ ">() == core::mem::size_of::<" ++ canonical ++ ">());\n";
        result = result ++ "const _: () = assert!(core::mem::align_of::<" ++ generated ++ ">() == core::mem::align_of::<" ++ canonical ++ ">());\n";
        for (fields, names) |field_name, name| {
            result = result ++ "const _: () = assert!(core::mem::offset_of!(" ++ generated ++ ", " ++ name ++ ") == core::mem::offset_of!(" ++ canonical ++ ", " ++ field_name ++ "));\n";
            result = result ++ "const _: fn(&" ++ generated ++ ", &mut " ++ canonical ++ ") = |value, canonical| { canonical." ++ field_name ++ " = value." ++ name ++ "; };\n";
        }
    }
    return result;
}

fn output(comptime lang: Language) []const u8 {
    @setEvalBranchQuota(100000);
    var result: []const u8 = lock(builtins.str.RocStr, "RocStr", "CanonicalStr", &.{ "bytes", "capacity_or_alloc_ptr", "length" }, lang) ++
        lock(builtins.list.RocList, if (lang == .c) "RocList" else "RocList<u8>", "CanonicalList", &.{ "elements", "length", "capacity_or_alloc_ptr" }, lang);
    if (lang == .c) {
        result = result ++ lock(builtins.erased_callable.Payload, "RocErasedCallablePayload", "CanonicalCallablePayload", &.{ "callable_fn_ptr", "on_drop" }, lang);
        for (@typeInfo(builtins.host_abi.ExternHostFns).@"struct".decl_names) |decl_name| {
            const T = @field(builtins.host_abi.ExternHostFns, decl_name);
            result = result ++ "ROC_STATIC_ASSERT(__builtin_types_compatible_p(__typeof__(&" ++ decl_name ++ "), " ++ declaration(T, "", lang) ++ "), \"runtime symbol signature mismatch\");\n";
        }
    }
    if (lang == .rust) {
        // RocHost is the explicit helper-only prefix. Hosted dispatch is not a
        // host helper operation; the complete RocOps stays interpreter-internal.
        requireNaturalExternLayout(Ops);
        const fields = @typeInfo(Ops).@"struct".field_names;
        if (!std.mem.eql(u8, fields[fields.len - 1], "hosted_fns")) @compileError("RocOps must end with hosted_fns");
        result = result ++ "#[repr(C)]\nstruct CanonicalHostPrefix {\n";
        for (fields[0 .. fields.len - 1]) |field_name| result = result ++ declaration(@FieldType(Ops, field_name), field_name, lang) ++ ",\n";
        result = result ++ "}\nconst _: () = assert!(core::mem::size_of::<RocHost>() == core::mem::size_of::<CanonicalHostPrefix>());\nconst _: () = assert!(core::mem::align_of::<RocHost>() == core::mem::align_of::<CanonicalHostPrefix>());\n";
        for (fields[0 .. fields.len - 1]) |field_name| {
            result = result ++ "const _: () = assert!(core::mem::offset_of!(RocHost, " ++ field_name ++ ") == core::mem::offset_of!(CanonicalHostPrefix, " ++ field_name ++ "));\n";
            result = result ++ "const _: fn(&RocHost, &mut CanonicalHostPrefix) = |host, canonical| { canonical." ++ field_name ++ " = host." ++ field_name ++ "; };\n";
        }
        const callback_types = .{ builtins.erased_callable.ErasedCallableFn, builtins.erased_callable.OnDropFn };
        const callback_names = .{ "RocErasedCallableFn", "RocErasedCallableOnDrop" };
        for (callback_types, callback_names) |T, name| {
            const d = declaration(T, "_", lang);
            const colon = std.mem.findScalar(u8, d, ':').?;
            result = result ++ "const _: fn(" ++ name ++ ") = |value| { let _: " ++ d[colon + 2 ..] ++ " = value; };\n";
        }
        for (@typeInfo(builtins.host_abi.ExternHostFns).@"struct".decl_names) |decl_name| {
            // Rust foreign declarations are unsafe to call, unlike vtable callbacks.
            const T = @field(builtins.host_abi.ExternHostFns, decl_name);
            const d = declaration(T, "_", lang);
            const colon = std.mem.findScalar(u8, d, ':').?;
            result = result ++ "const _: unsafe " ++ d[colon + 2 ..] ++ " = " ++ decl_name ++ ";\n";
        }
    }
    return result;
}

const GenerateError = std.process.Args.ToSliceError || std.Io.Dir.WriteFileError || std.Io.Dir.ReadFileAllocError || error{ExpectedHeaderRustInputRustOutput};

/// Write the C canonical header and append canonical Rust compile assertions.
pub fn main(init: std.process.Init) GenerateError!void {
    const args = try init.minimal.args.toSlice(init.arena.allocator());
    if (args.len != 4) return error.ExpectedHeaderRustInputRustOutput;
    try std.Io.Dir.cwd().writeFile(init.io, .{ .sub_path = args[1], .data = comptime output(.c) });
    const rust = try std.Io.Dir.cwd().readFileAlloc(init.io, args[2], init.arena.allocator(), .limited(16 * 1024 * 1024));
    const combined = try std.mem.concat(init.arena.allocator(), u8, &.{ rust, "\n", comptime output(.rust) });
    try std.Io.Dir.cwd().writeFile(init.io, .{ .sub_path = args[3], .data = combined });
}
