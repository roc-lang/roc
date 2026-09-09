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
            (if (p.is_const) "const " else "") ++ scalar(p.child, lang) ++ "*"
        else
            (if (p.is_const) "*const " else "*mut ") ++ scalar(p.child, lang),
        .type, .void, .bool, .noreturn, .int, .float, .array, .@"struct", .comptime_float, .comptime_int, .undefined, .null, .error_union, .error_set, .@"enum", .@"union", .@"fn", .@"opaque", .frame, .@"anyframe", .vector, .enum_literal => @compileError("add an explicit foreign ABI spelling for " ++ @typeName(T)),
    };
}

fn declaration(comptime T: type, comptime name: []const u8, comptime lang: Language) []const u8 {
    if (@typeInfo(T) == .optional) return declaration(@typeInfo(T).optional.child, name, lang);
    if (@typeInfo(T) == .pointer and @typeInfo(@typeInfo(T).pointer.child) == .@"fn") {
        const f = @typeInfo(@typeInfo(T).pointer.child).@"fn";
        if (!std.meta.eql(f.calling_convention, std.builtin.CallingConvention.c)) @compileError("host callback must use C ABI");
        if (f.is_var_args) @compileError("host callbacks must have fixed arity");
        var args: []const u8 = "";
        for (f.params, 0..) |param, i| {
            if (i != 0) args = args ++ ", ";
            args = args ++ scalar(param.type.?, lang);
        }
        if (lang == .c and args.len == 0) args = "void";
        return if (lang == .c)
            scalar(f.return_type.?, lang) ++ " (*" ++ name ++ ")(" ++ args ++ ")"
        else
            name ++ ": extern \"C\" fn(" ++ args ++ ") -> " ++ scalar(f.return_type.?, lang);
    }
    return if (lang == .c) scalar(T, lang) ++ " " ++ name else name ++ ": " ++ scalar(T, lang);
}

// The foreign compilers apply natural C layout. Never silently discard an
// explicit Zig field-alignment contract; adding one requires an exact foreign
// representation here before the canonical mirror can be emitted.
fn requireNaturalExternLayout(comptime T: type) void {
    const info = @typeInfo(T).@"struct";
    if (info.layout != .@"extern") @compileError("foreign ABI lock requires an extern struct");
    for (info.fields) |field| {
        if (field.alignment != null) {
            @compileError("explicit alignment needs a foreign ABI spelling: " ++ @typeName(T) ++ "." ++ field.name);
        }
    }
}

fn lock(comptime T: type, comptime generated: []const u8, comptime canonical: []const u8, comptime names: []const []const u8, comptime lang: Language) []const u8 {
    requireNaturalExternLayout(T);
    const fields = @typeInfo(T).@"struct".fields;
    if (fields.len != names.len) @compileError("canonical ABI field count changed: update the glue template and lock field mapping");
    for (fields, names) |field, name| {
        const canonical_name = if (std.mem.eql(u8, name, "elements")) "bytes" else name;
        if (!std.mem.eql(u8, field.name, canonical_name)) @compileError("canonical ABI field order changed");
    }
    var result: []const u8 = if (lang == .c) "typedef struct {\n" else "#[repr(C)]\nstruct " ++ canonical ++ " {\n";
    for (fields) |field| result = result ++ "    " ++ declaration(field.type, field.name, lang) ++ (if (lang == .c) ";\n" else ",\n");
    result = result ++ (if (lang == .c) "} " ++ canonical ++ ";\n" else "}\n");
    if (lang == .c) {
        result = result ++ "ROC_STATIC_ASSERT(sizeof(" ++ generated ++ ") == sizeof(" ++ canonical ++ "), \"canonical size mismatch\");\n";
        result = result ++ "ROC_STATIC_ASSERT(ROC_ALIGNOF(" ++ generated ++ ") == ROC_ALIGNOF(" ++ canonical ++ "), \"canonical alignment mismatch\");\n";
        for (fields, names) |field, name| {
            const canonical_name = if (std.mem.eql(u8, name, "elements")) "bytes" else name;
            result = result ++ "ROC_STATIC_ASSERT(offsetof(" ++ generated ++ ", " ++ name ++ ") == offsetof(" ++ canonical ++ ", " ++ canonical_name ++ "), \"canonical offset mismatch\");\n";
            result = result ++ "ROC_STATIC_ASSERT(sizeof(((" ++ generated ++ "*)0)->" ++ name ++ ") == sizeof(((" ++ canonical ++ "*)0)->" ++ canonical_name ++ "), \"canonical field size mismatch\");\n";
            if (std.mem.eql(u8, name, "elements")) {
                // C intentionally erases List(U8)'s element pointer to void*.
                if (field.type != ?[*]u8) @compileError("canonical list element pointer changed");
                result = result ++ "ROC_STATIC_ASSERT(__builtin_types_compatible_p(__typeof__(((" ++ generated ++ "*)0)->elements), void*), \"list element pointer type mismatch\");\n";
            } else {
                result = result ++ "ROC_STATIC_ASSERT(__builtin_types_compatible_p(__typeof__(((" ++ generated ++ "*)0)->" ++ name ++ "), __typeof__(((" ++ canonical ++ "*)0)->" ++ canonical_name ++ ")), \"canonical field type mismatch\");\n";
            }
        }
    } else {
        result = result ++ "const _: () = assert!(core::mem::size_of::<" ++ generated ++ ">() == core::mem::size_of::<" ++ canonical ++ ">());\n";
        result = result ++ "const _: () = assert!(core::mem::align_of::<" ++ generated ++ ">() == core::mem::align_of::<" ++ canonical ++ ">());\n";
        for (fields, names) |field, name| {
            result = result ++ "const _: () = assert!(core::mem::offset_of!(" ++ generated ++ ", " ++ name ++ ") == core::mem::offset_of!(" ++ canonical ++ ", " ++ field.name ++ "));\n";
            result = result ++ "const _: fn(&" ++ generated ++ ", &mut " ++ canonical ++ ") = |value, canonical| { canonical." ++ field.name ++ " = value." ++ name ++ "; };\n";
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
    }
    if (lang == .rust) {
        // RocHost is the explicit helper-only prefix. Hosted dispatch is not a
        // host helper operation; the complete RocOps stays interpreter-internal.
        requireNaturalExternLayout(Ops);
        const fields = @typeInfo(Ops).@"struct".fields;
        if (!std.mem.eql(u8, fields[fields.len - 1].name, "hosted_fns")) @compileError("RocOps must end with hosted_fns");
        result = result ++ "#[repr(C)]\nstruct CanonicalHostPrefix {\n";
        for (fields[0 .. fields.len - 1]) |field| result = result ++ declaration(field.type, field.name, lang) ++ ",\n";
        result = result ++ "}\nconst _: () = assert!(core::mem::size_of::<RocHost>() == core::mem::size_of::<CanonicalHostPrefix>());\nconst _: () = assert!(core::mem::align_of::<RocHost>() == core::mem::align_of::<CanonicalHostPrefix>());\n";
        for (fields[0 .. fields.len - 1]) |field| {
            result = result ++ "const _: () = assert!(core::mem::offset_of!(RocHost, " ++ field.name ++ ") == core::mem::offset_of!(CanonicalHostPrefix, " ++ field.name ++ "));\n";
            result = result ++ "const _: fn(&RocHost, &mut CanonicalHostPrefix) = |host, canonical| { canonical." ++ field.name ++ " = host." ++ field.name ++ "; };\n";
        }
        const callback_types = .{ builtins.erased_callable.ErasedCallableFn, builtins.erased_callable.OnDropFn };
        const callback_names = .{ "RocErasedCallableFn", "RocErasedCallableOnDrop" };
        for (callback_types, callback_names) |T, name| {
            const d = declaration(T, "_", lang);
            const colon = std.mem.findScalar(u8, d, ':').?;
            result = result ++ "const _: fn(" ++ name ++ ") = |value| { let _: " ++ d[colon + 2 ..] ++ " = value; };\n";
        }
        for (@typeInfo(builtins.host_abi.ExternHostFns).@"struct".decls) |decl| {
            // Rust foreign declarations are unsafe to call, unlike vtable callbacks.
            const T = @field(builtins.host_abi.ExternHostFns, decl.name);
            const d = declaration(T, "_", lang);
            const colon = std.mem.findScalar(u8, d, ':').?;
            result = result ++ "const _: unsafe " ++ d[colon + 2 ..] ++ " = " ++ decl.name ++ ";\n";
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
