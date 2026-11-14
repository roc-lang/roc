//! A compile-time root whose checked expression is in value form is
//! completed by the checked-value writer, every other root by an evaluator
//! (design.md "Value-Form Roots"). These tests pin both sides of the rule:
//! the writer stores exactly the value and stored type the evaluator stores
//! for every kind of value-form root, and an expression that calls,
//! dispatches, reads a definition, converts through a custom type, or that
//! checking rejects stays with the evaluator.

const std = @import("std");
const base = @import("base");
const build_options = @import("build_options");
const check = @import("check");
const eval = @import("eval");
const roc_target = @import("roc_target");
const CoreCtx = @import("ctx").CoreCtx;
const Coordinator = @import("../coordinator.zig").Coordinator;
const is_freestanding = @import("../threading.zig").is_freestanding;

const Artifact = check.CheckedArtifact.CheckedModuleArtifact;
const CompileTimeRoot = check.CheckedArtifact.CompileTimeRoot;
const ConstStore = check.ConstStore;
const ValueFormRoots = eval.CompileTimeFinalization.Options.ValueFormRoots;

const SourceFile = struct { path: []const u8, source: []const u8 };

const platform_files = [_]SourceFile{
    .{ .path = ".roc_echo_platform/main.roc", .source =
    \\platform ""
    \\    requires {} { main! : List(Str) => Try({}, [Exit(I8), ..]) }
    \\    exposes [Echo]
    \\    packages {}
    \\    provides { "roc_main": main_for_host! }
    \\    hosted { "roc_echo_line": Echo.line! }
    \\import Echo
    \\main_for_host! : List(Str) => I8
    \\main_for_host! = |args|
    \\    match main!(args) {
    \\        Ok({}) => 0
    \\        Err(Exit(code)) => code
    \\        Err(other) => {
    \\            Echo.line!("Program exited with error: ${Str.inspect(other)}")
    \\            1
    \\        }
    \\    }
    },
    .{ .path = ".roc_echo_platform/Echo.roc", .source =
    \\Echo := [].{
    \\    line! : Str => {}
    \\}
    },
};

/// Every kind of value-form root: each builtin number type at its extremes,
/// strings short, long and on several lines, booleans, empty and non-empty
/// lists of numbers, strings, records, tuples, tags and zero-sized values,
/// records, tuples, single-tag, enum-like and payload unions, `Try`,
/// nominal tags and records, a recursive nominal, equal long literals that
/// one evaluation reads at one address, and the hoisted constants of a
/// function body.
const corpus =
    \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
    \\import pf.Echo
    \\
    \\Color := [Red, Green, Blue(U8)]
    \\Point := { x : I32, y : I32 }
    \\Tree := [Leaf, Node(Tree, U8, Tree)]
    \\
    \\u8_value : U8
    \\u8_value = 255
    \\i8_value : I8
    \\i8_value = -128
    \\u16_value : U16
    \\u16_value = 65535
    \\i16_value : I16
    \\i16_value = -32768
    \\u32_value : U32
    \\u32_value = 4294967295
    \\i32_value : I32
    \\i32_value = -2147483648
    \\u64_value : U64
    \\u64_value = 18446744073709551615
    \\i64_value : I64
    \\i64_value = -9223372036854775808
    \\u128_value : U128
    \\u128_value = 340282366920938463463374607431768211455
    \\i128_value : I128
    \\i128_value = -170141183460469231731687303715884105728
    \\dec_value : Dec
    \\dec_value = -3.141592653589793238
    \\f32_value : F32
    \\f32_value = 0.1
    \\f64_value : F64
    \\f64_value = 2.718281828459045
    \\zero_value : U64
    \\zero_value = 0
    \\
    \\empty_str = ""
    \\short_str = "short"
    \\inline_limit_str = "exactly twenty three by"
    \\long_str = "a string long enough that no target holds it inline"
    \\multiline_str =
    \\    \\the first line of a string on several lines
    \\    \\and the second
    \\
    \\yes = True
    \\no : Bool
    \\no = False
    \\
    \\empty_numbers : List(U8)
    \\empty_numbers = []
    \\empty_records : List({ a : U8 })
    \\empty_records = []
    \\bytes : List(U8)
    \\bytes = [0, 1, 2, 255]
    \\wide_numbers : List(I128)
    \\wide_numbers = [-1, 0, 170141183460469231731687303715884105727]
    \\floats : List(F64)
    \\floats = [0.5, -1.25, 3.0]
    \\singles : List(F32)
    \\singles = [0.1, 0.2]
    \\decs : List(Dec)
    \\decs = [1.5, -2.25]
    \\strings = ["a", "a string long enough that no target holds it inline", "a string long enough that no target holds it inline"]
    \\shared_lists : List(List(U8))
    \\shared_lists = [[1, 2, 3], [1, 2, 3], [4]]
    \\points : List({ x : I32, y : I32 })
    \\points = [{ x: 1, y: -2 }, { x: 3, y: 4 }]
    \\pairs : List((U8, U32))
    \\pairs = [(1, 2), (3, 4)]
    \\mixed_products : List({ id : U64, scale : Dec, small : U8 })
    \\mixed_products = [{ id: 1, scale: 0.5, small: 2 }, { id: 3, scale: 4.25, small: 5 }]
    \\units : List({})
    \\units = [{}, {}]
    \\tags : List([A, B(U8)])
    \\tags = [A, B(1), A]
    \\flags : List(Bool)
    \\flags = [True, False]
    \\nested_lists : List(List(Str))
    \\nested_lists = [["x"], [], ["y", "z"]]
    \\
    \\unit = {}
    \\record : { b : Str, a : U64, c : List(U8) }
    \\record = { b: "bee", a: 7, c: [9] }
    \\nested : { inner : { deep : (U8, Str, List([A, B])) }, other : List({ p : I64 }) }
    \\nested = { inner: { deep: (1, "x", [A, B]) }, other: [{ p: 1 }] }
    \\long_strings = { first: "a string long enough that no target holds it inline", second: "a string long enough that no target holds it inline" }
    \\tuple : (U8, Str, Dec)
    \\tuple = (1, "two", 3.5)
    \\
    \\single_tag : [Only]
    \\single_tag = Only
    \\enum_tag : [North, South, East, West]
    \\enum_tag = West
    \\payload_tag : [Point(I64, I64), Nothing]
    \\payload_tag = Point(-1, 2)
    \\ok : Try(U32, [Bad])
    \\ok = Ok(5)
    \\err : Try(U32, [Bad])
    \\err = Err(Bad)
    \\color : Color
    \\color = Color.Blue(9)
    \\plain_color : Color
    \\plain_color = Color.Red
    \\point : Point
    \\point = Point.{ x: 1, y: 2 }
    \\tree : Tree
    \\tree = Tree.Node(Tree.Leaf, 5, Tree.Node(Tree.Leaf, 6, Tree.Leaf))
    \\
    \\parse : List(U8) -> Try({ x : U8, y : U8 }, [Empty, Long])
    \\parse = |input| {
    \\    padded = List.concat(input, [7, 8, 9])
    \\    labels = List.concat([Str.inspect(List.len(padded))], ["a label long enough to live on the heap", "b"])
    \\    if List.len(padded) == 3 {
    \\        Err(Empty)
    \\    } else if List.len(labels) > 9 {
    \\        Err(Long)
    \\    } else {
    \\        Ok({ x: 0, y: 1 })
    \\    }
    \\}
    \\
    \\main! = |args| {
    \\    Echo.line!(Str.inspect(parse([])))
    \\    Echo.line!(Str.inspect(List.len(args)))
    \\    Ok({})
    \\}
;

/// Literals at custom types, interpolation, a record update, calls,
/// operators and lookups: none is in value form.
const near_misses =
    \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
    \\import pf.Echo
    \\
    \\N := [Val(I64)].{
    \\    from_numeral : Numeral -> Try(N, [InvalidNumeral(Str)])
    \\    from_numeral = |_| Ok(N.Val(0))
    \\}
    \\Word := { text : Str }.{
    \\    from_quote : Str -> Try(Word, [BadQuotedBytes(Str)])
    \\    from_quote = |text| if text == "bad" Err(BadQuotedBytes("bad is not a word")) else Ok({ text: text })
    \\}
    \\
    \\base_record = { a: 1.U8, b: 2.U8 }
    \\value_form = [1.U8, 2]
    \\custom_numeral : N
    \\custom_numeral = 5
    \\custom_quote : Word
    \\custom_quote = "hello"
    \\custom_in_list : List(N)
    \\custom_in_list = [1, 2]
    \\interpolated = "value: ${Str.inspect(1.U8)}"
    \\updated = { ..base_record, b: 3 }
    \\called = List.len([1.U8])
    \\operator = 1.U8 + 2
    \\negated = -(1.I64)
    \\lookup = value_form
    \\list_with_lookup = [base_record]
    \\
    \\main! = |_args| {
    \\    Echo.line!(Str.inspect(custom_numeral))
    \\    Ok({})
    \\}
;

const Checked = struct {
    allocator: std.mem.Allocator,
    tmp_dir: std.testing.TmpDir,
    builtin_modules: *eval.BuiltinModules,
    coord: *Coordinator,
    arena: base.SingleThreadArena,

    fn deinit(self: *Checked) void {
        self.coord.deinit();
        self.allocator.destroy(self.coord);
        self.builtin_modules.deinit();
        self.allocator.destroy(self.builtin_modules);
        self.arena.deinit();
        self.tmp_dir.cleanup();
    }

    fn app(self: *Checked) *const Artifact {
        return self.coord.appRootCheckedArtifact();
    }

    fn rootsCompletedDirectly(self: *const Checked) u64 {
        return self.coord.ctfe_timing.roots_completed_directly.load() + self.coord.module_ctfe_timing.roots_completed_directly.load();
    }

    /// The reports checking produced, for a failing test to show.
    fn expectErrors(self: *Checked, expected: bool) error{UnexpectedUserErrors}!void {
        if (self.coord.hasUserErrors() == expected) return;
        var reports = self.coord.iterReports();
        while (reports.next()) |entry| std.debug.print("unexpected report: {s}\n", .{entry.report.title});
        return error.UnexpectedUserErrors;
    }
};

const CheckAppError = std.mem.Allocator.Error ||
    std.Io.Dir.CreateDirPathError ||
    std.Io.Dir.WriteFileError ||
    std.Io.Dir.RealPathFileAllocError ||
    eval.BuiltinModules.InitError ||
    std.Thread.SpawnError ||
    Coordinator.AppDiscoveryError ||
    @import("../coordinator.zig").CoordinatorError;

/// Check an echo-platform app as `roc check` does, completing its value-form
/// roots with `value_form_roots`.
fn checkApp(allocator: std.mem.Allocator, source: []const u8, strategy: base.SpecializationStrategy, value_form_roots: ValueFormRoots) CheckAppError!Checked {
    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    errdefer tmp_dir.cleanup();
    try tmp_dir.dir.createDirPath(io, ".roc_echo_platform");
    for (platform_files) |file| try tmp_dir.dir.writeFile(io, .{ .sub_path = file.path, .data = file.source });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "main.roc", .data = source });
    const app_path = try tmp_dir.dir.realPathFileAlloc(io, "main.roc", allocator);
    defer allocator.free(app_path);

    const builtin_modules = try allocator.create(eval.BuiltinModules);
    errdefer allocator.destroy(builtin_modules);
    builtin_modules.* = try eval.BuiltinModules.init(allocator, std.testing.io);
    errdefer builtin_modules.deinit();
    const coord = try allocator.create(Coordinator);
    errdefer allocator.destroy(coord);
    coord.* = try Coordinator.init(
        allocator,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        builtin_modules,
        build_options.compiler_compatibility_id,
        null,
        CoreCtx.os(allocator, allocator, io),
    );
    errdefer coord.deinit();
    coord.enable_hosted_transform = true;
    coord.specialization_strategy = strategy;
    coord.value_form_roots = value_form_roots;
    var arena = base.SingleThreadArena.init(allocator);
    errdefer arena.deinit();
    try coord.start();
    try coord.discoverAppFromPath(arena.allocator(), .{ .entry_path = app_path });
    try coord.coordinatorLoop();
    try coord.finishCheckedProgram(.executable_artifacts);
    return .{
        .allocator = allocator,
        .tmp_dir = tmp_dir,
        .builtin_modules = builtin_modules,
        .coord = coord,
        .arena = arena,
    };
}

/// The name a top-level constant root binds, or null for any other root.
fn rootName(artifact: *const Artifact, root: CompileTimeRoot) ?[]const u8 {
    if (root.kind != .constant) return null;
    const def_idx = switch (root.source) {
        .def => |def| def,
        .expr, .statement, .required_binding, .hoisted, .literal, .literal_argument => return null,
    };
    const env = artifact.moduleEnvConst();
    const pattern = env.store.getPattern(env.store.getDef(def_idx).pattern);
    if (pattern != .assign) return null;
    return env.getIdent(pattern.assign.ident);
}

/// The stored type a completed constant or hoisted constant root is
/// restored at.
fn storedRootType(artifact: *const Artifact, root: CompileTimeRoot) error{ NoTopLevelValue, NotAConstant, NoHoistedConst, NotAStoredKind, NotStored }!ConstStore.ConstTypeId {
    const const_ref = switch (root.kind) {
        .constant => switch ((artifact.top_level_values.lookupByDef(root.source.def) orelse return error.NoTopLevelValue).value) {
            .const_ref => |ref| ref,
            .procedure_binding => return error.NotAConstant,
        },
        .hoisted_constant => (artifact.hoisted_constants.lookupByRoot(root.id) orelse return error.NoHoistedConst).const_ref,
        .hoisted_validation, .callable_binding, .expect, .numeral_conversion, .quote_conversion, .repl_expr, .literal_argument => return error.NotAStoredKind,
    };
    const state = artifact.const_templates.get(const_ref).state;
    if (state != .stored_const) return error.NotStored;
    return state.stored_const.root_type;
}

const HashContext = struct {
    pub fn moduleDigest(_: HashContext, module: check.CheckedArtifact.CheckedModuleDigest) [32]u8 {
        return module.bytes;
    }
    pub fn fnValueDigest(_: HashContext, _: ConstStore.ConstFnId) [32]u8 {
        return @splat(0);
    }
};

/// A stored value's structure, sharing included: its graph hashed with
/// every repeated node named by the order it was first reached in.
fn graphDigest(store: *const ConstStore.ConstStore, node: ConstStore.ConstNodeId) std.mem.Allocator.Error![32]u8 {
    var hasher = base.Sha256.init(.{});
    try store.hashGraph(std.testing.allocator, node, &hasher, HashContext{});
    return hasher.finalResult();
}

/// Require the two stored types to be one type: each is cloned into a fresh
/// table with one shared name store, so equal types clone to equal tables.
fn expectSameType(left: *const Artifact, left_type: ConstStore.ConstTypeId, right: *const Artifact, right_type: ConstStore.ConstTypeId) !void {
    const allocator = std.testing.allocator;
    var names = check.CanonicalNames.CanonicalNameStore.init(allocator);
    defer names.deinit();
    var left_store = ConstStore.ConstTypeStore.init(allocator);
    defer left_store.deinit();
    var right_store = ConstStore.ConstTypeStore.init(allocator);
    defer right_store.deinit();
    const left_id = try left_store.cloneTypeFromTranslated(&left.const_store.type_store, &left.canonical_names, &names, left_type);
    const right_id = try right_store.cloneTypeFromTranslated(&right.const_store.type_store, &right.canonical_names, &names, right_type);
    try std.testing.expectEqual(left_id, right_id);
    try std.testing.expectEqualDeep(left_store.types.items(), right_store.types.items());
    try std.testing.expectEqualDeep(left_store.named_pool.items(), right_store.named_pool.items());
    try std.testing.expectEqualDeep(left_store.erased_pool.items(), right_store.erased_pool.items());
    try std.testing.expectEqualDeep(left_store.type_pool.items(), right_store.type_pool.items());
    try std.testing.expectEqualDeep(left_store.field_pool.items(), right_store.field_pool.items());
    try std.testing.expectEqualDeep(left_store.tag_pool.items(), right_store.tag_pool.items());
    try std.testing.expectEqualDeep(left_store.declared_field_pool.items(), right_store.declared_field_pool.items());
}

fn expectWriterMatchesEvaluator(strategy: base.SpecializationStrategy) !void {
    const allocator = std.testing.allocator;
    var evaluated = try checkApp(allocator, corpus, strategy, .evaluate);
    defer evaluated.deinit();
    try evaluated.expectErrors(false);
    var written = try checkApp(allocator, corpus, strategy, .write);
    defer written.deinit();
    try written.expectErrors(false);

    try std.testing.expectEqual(@as(u64, 0), evaluated.rootsCompletedDirectly());
    const left = evaluated.app();
    const right = written.app();
    const left_roots = left.compile_time_roots.roots.items();
    const right_roots = right.compile_time_roots.roots.items();
    // One source checked twice: the roots line up by id.
    try std.testing.expectEqual(left_roots.len, right_roots.len);
    var constants: usize = 0;
    var hoisted: usize = 0;
    for (left_roots, right_roots) |left_root, right_root| {
        try std.testing.expectEqual(left_root.kind, right_root.kind);
        try std.testing.expectEqual(left_root.completion, right_root.completion);
        // Every top-level constant of the corpus is in value form.
        if (rootName(right, right_root)) |name| if (right_root.completion != .write_value) {
            std.debug.print("corpus constant {s} is not in value form\n", .{name});
            return error.CorpusConstantNotValueForm;
        };
        if (right_root.completion != .write_value) continue;
        switch (right_root.kind) {
            .constant => constants += 1,
            .hoisted_constant => hoisted += 1,
            .hoisted_validation, .callable_binding, .expect, .numeral_conversion, .quote_conversion, .repl_expr, .literal_argument => return error.WrongKind,
        }
        if (left_root.payload != .const_node or right_root.payload != .const_node) {
            std.debug.print("value-form root {d} ({?s}) was not completed\n", .{ @backingInt(right_root.id), rootName(right, right_root) });
            return error.RootNotCompleted;
        }
        const left_node = left_root.payload.const_node;
        const right_node = right_root.payload.const_node;
        if (!try left.const_store.graphEql(allocator, left_node, &right.const_store, right_node)) {
            std.debug.print("value-form root {d} ({?s}) stored a different value\n", .{ @backingInt(right_root.id), rootName(right, right_root) });
            return error.ValueDiffers;
        }
        if (!std.mem.eql(u8, &try graphDigest(&left.const_store, left_node), &try graphDigest(&right.const_store, right_node))) {
            std.debug.print("value-form root {d} ({?s}) shares its parts differently\n", .{ @backingInt(right_root.id), rootName(right, right_root) });
            return error.SharingDiffers;
        }
        const left_type = storedRootType(left, left_root) catch |err| {
            std.debug.print("value-form root {d} ({?s}) has no stored type\n", .{ @backingInt(right_root.id), rootName(right, right_root) });
            return err;
        };
        const right_type = storedRootType(right, right_root) catch |err| {
            std.debug.print("value-form root {d} ({?s}) has no stored type when written\n", .{ @backingInt(right_root.id), rootName(right, right_root) });
            return err;
        };
        expectSameType(left, left_type, right, right_type) catch |err| {
            std.debug.print("value-form root {d} ({?s}) stored a different type\n", .{ @backingInt(right_root.id), rootName(right, right_root) });
            return err;
        };
    }
    try std.testing.expectEqual(@as(usize, 51), constants);
    // The lists `parse` concatenates.
    if (hoisted < 2) return error.TooFewHoistedValueFormRoots;
    if (written.rootsCompletedDirectly() < constants + hoisted) return error.TooFewRootsCompletedDirectly;
}

test "the checked-value writer stores each value-form root as the boxy evaluator does" {
    if (is_freestanding) return error.SkipZigTest;
    try expectWriterMatchesEvaluator(.boxy);
}

test "the checked-value writer stores each value-form root as an lss compilation's evaluator does" {
    if (is_freestanding) return error.SkipZigTest;
    try expectWriterMatchesEvaluator(.lss);
}

test "a literal at a custom type, interpolation, an update, a call, an operator, or a lookup is evaluated" {
    if (is_freestanding) return error.SkipZigTest;
    var checked = try checkApp(std.testing.allocator, near_misses, .boxy, .write);
    defer checked.deinit();
    try checked.expectErrors(false);
    const app = checked.app();
    var seen: usize = 0;
    for (app.compile_time_roots.roots.items()) |root| {
        const name = rootName(app, root) orelse continue;
        const value_form = std.mem.eql(u8, name, "base_record") or std.mem.eql(u8, name, "value_form");
        try std.testing.expectEqual(if (value_form) check.CheckedArtifact.CompileTimeRootCompletion.write_value else .evaluate, root.completion);
        // Every root completed, whichever producer completed it.
        try std.testing.expect(root.payload == .const_node);
        seen += 1;
    }
    try std.testing.expectEqual(@as(usize, 12), seen);
}

test "a literal checking rejects is evaluated and reported" {
    if (is_freestanding) return error.SkipZigTest;
    var checked = try checkApp(std.testing.allocator,
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\too_big : U8
        \\too_big = 256
        \\main! = |_args| {
        \\    Echo.line!(Str.inspect(too_big))
        \\    Ok({})
        \\}
    , .boxy, .write);
    defer checked.deinit();
    try checked.expectErrors(true);
    const app = checked.app();
    var found = false;
    for (app.compile_time_roots.roots.items()) |root| {
        const name = rootName(app, root) orelse continue;
        if (!std.mem.eql(u8, name, "too_big")) continue;
        found = true;
        try std.testing.expectEqual(check.CheckedArtifact.CompileTimeRootCompletion.evaluate, root.completion);
    }
    try std.testing.expect(found);
}
