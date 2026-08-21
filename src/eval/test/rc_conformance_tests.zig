//! Per-op conformance sweep for the `RcEffect` table.
//!
//! Every low-level op whose `rcEffect()` is not `none()` makes a claim about
//! refcounts that nothing else checks. This sweep runs Roc programs through the
//! interpreter with the observer in `eval/rc_conformance.zig` watching, so each
//! executed op is judged against its row, and then fails for any op with a
//! nontrivial row that no case drove—a new builtin cannot ship an unverified
//! row.
//!
//! Cases drive the copy-on-write ops in both uniqueness regimes. A value that
//! is only used once is unique, so its op takes the in-place path and a wrong
//! `result_unique` claim looks true; the same op applied to a value a list also
//! holds runs on a count above 1, where the claim has to stand on its own.
//! That second regime is what catches the #10023 shape.

const std = @import("std");
const base = @import("base");
const eval = @import("eval");

const helpers = eval.test_helpers;
const rc_conformance = eval.rc_conformance;
const Interpreter = eval.Interpreter;
const RuntimeHostEnv = eval.RuntimeHostEnv;
const LowLevel = base.LowLevel;
const RcEffect = LowLevel.RcEffect;
const Allocator = std.mem.Allocator;

const SweepError = helpers.TestHelperError || eval.BuiltinModules.InitError || Interpreter.Error ||
    RuntimeHostEnv.LeakError || error{
    RcEffectConformanceFailed,
    CaseSourceDoesNotCheck,
};

/// A program whose execution drives one or more nontrivial rows.
const Case = struct {
    name: []const u8,
    source: []const u8,
    source_kind: helpers.SourceKind = .expr,
    /// Lower with wrapper inlining, the way optimized builds do. Some ops only
    /// exist after a wrapper-shaped proc survives to the LIR passes that
    /// rewrite it.
    inline_wrappers: bool = false,
    /// Allocations still live when the program returns. Cases whose value is a
    /// number leave nothing behind; a case that returns a heap value leaves it
    /// live, because the harness never releases the entrypoint's result.
    expected_live_allocations: u32 = 0,
};

/// An op no interpreted Roc program can reach, with the reason it cannot.
const Exemption = struct {
    op: LowLevel,
    reason: []const u8,
};

/// Strings long enough that the small-string representation cannot hold them,
/// built at runtime so they are heap allocations rather than static data.
/// Static data has a constant refcount, which no op can move.
const cases = [_]Case{
    .{
        .name = "str copy-on-write ops, unique and shared inputs",
        .source =
        \\{
        \\    shared = Str.concat("a string long enough to need the heap ", "plus a tail")
        \\    holder = [shared, shared]
        \\    unique_trim = Str.trim(Str.concat("   a string long enough to need the heap  ", "  "))
        \\    shared_trim = Str.trim(shared)
        \\    unique_start = Str.trim_start(Str.concat("   another heap string with leading space ", "tail"))
        \\    shared_start = Str.trim_start(shared)
        \\    unique_end = Str.trim_end(Str.concat("another heap string with trailing space  ", "  "))
        \\    shared_end = Str.trim_end(shared)
        \\    unique_lower = Str.with_ascii_lowercased(Str.concat("A HEAP STRING LONG ENOUGH ", "TO ALLOCATE"))
        \\    shared_lower = Str.with_ascii_lowercased(shared)
        \\    unique_upper = Str.with_ascii_uppercased(Str.concat("a heap string long enough ", "to allocate"))
        \\    shared_upper = Str.with_ascii_uppercased(shared)
        \\    unique_reserve = Str.reserve(Str.concat("a heap string long enough ", "to allocate"), 128)
        \\    shared_reserve = Str.reserve(shared, 128)
        \\    trimmed_capacity = Str.release_excess_capacity(unique_reserve)
        \\    shared_capacity = Str.release_excess_capacity(shared_reserve)
        \\    Str.count_utf8_bytes(unique_trim)
        \\        + Str.count_utf8_bytes(shared_trim)
        \\        + Str.count_utf8_bytes(unique_start)
        \\        + Str.count_utf8_bytes(shared_start)
        \\        + Str.count_utf8_bytes(unique_end)
        \\        + Str.count_utf8_bytes(shared_end)
        \\        + Str.count_utf8_bytes(unique_lower)
        \\        + Str.count_utf8_bytes(shared_lower)
        \\        + Str.count_utf8_bytes(unique_upper)
        \\        + Str.count_utf8_bytes(shared_upper)
        \\        + Str.count_utf8_bytes(trimmed_capacity)
        \\        + Str.count_utf8_bytes(shared_capacity)
        \\        + List.len(holder)
        \\}
        ,
    },
    .{
        // Concatenating nothing onto a value is the path where a
        // copy-on-write op can hand back its argument without either copying
        // it or checking whether anyone else holds it.
        .name = "concatenating an empty value onto a shared one",
        .source =
        \\{
        \\    shared_str = Str.concat("a string long enough to need the heap ", "plus a tail")
        \\    str_holder = [shared_str, shared_str]
        \\    shared_list = List.concat(
        \\        ["a list element long enough to allocate", "another list element long enough"],
        \\        ["a third list element long enough to allocate"],
        \\    )
        \\    list_holder = [shared_list, shared_list]
        \\    nothing_str = Str.repeat("x", 0)
        \\    nothing_list : List(Str)
        \\    nothing_list = List.with_capacity(0)
        \\    str_tail = Str.concat(shared_str, nothing_str)
        \\    list_tail = List.concat(shared_list, nothing_list)
        \\    list_head = List.concat(nothing_list, shared_list)
        \\    Str.count_utf8_bytes(str_tail)
        \\        + List.len(list_tail)
        \\        + List.len(list_head)
        \\        + List.len(str_holder)
        \\        + List.len(list_holder)
        \\}
        ,
    },
    .{
        .name = "str slice ops share their argument's allocation",
        .source =
        \\{
        \\    shared = Str.concat("prefix-a string long enough to need the heap ", "plus a tail")
        \\    holder = [shared, shared]
        \\    unique = Str.concat("prefix-another string long enough for the heap ", "and a tail")
        \\    unique_dropped = Str.drop_prefix(unique, "prefix-")
        \\    shared_dropped = Str.drop_prefix(shared, "prefix-")
        \\    unique_suffix = Str.drop_suffix(unique, "a tail")
        \\    shared_suffix = Str.drop_suffix(shared, "a tail")
        \\    unique_caseless = Str.drop_prefix_caseless_ascii(unique, "PREFIX-").ok_or("")
        \\    shared_caseless = Str.drop_prefix_caseless_ascii(shared, "PREFIX-").ok_or("")
        \\    unique_first = Str.split_first(unique, "-").ok_or({ before: "", after: "" }).after
        \\    shared_first = Str.split_first(shared, "-").ok_or({ before: "", after: "" }).after
        \\    unique_last = Str.split_last(unique, " ").ok_or({ before: "", after: "" }).before
        \\    shared_last = Str.split_last(shared, " ").ok_or({ before: "", after: "" }).before
        \\    unique_sub = Str.drop_last_bytes(unique, 6).ok_or("")
        \\    shared_sub = Str.drop_last_bytes(shared, 6).ok_or("")
        \\    Str.count_utf8_bytes(unique_dropped)
        \\        + Str.count_utf8_bytes(shared_dropped)
        \\        + Str.count_utf8_bytes(unique_suffix)
        \\        + Str.count_utf8_bytes(shared_suffix)
        \\        + Str.count_utf8_bytes(unique_caseless)
        \\        + Str.count_utf8_bytes(shared_caseless)
        \\        + Str.count_utf8_bytes(unique_first)
        \\        + Str.count_utf8_bytes(shared_first)
        \\        + Str.count_utf8_bytes(unique_last)
        \\        + Str.count_utf8_bytes(shared_last)
        \\        + Str.count_utf8_bytes(unique_sub)
        \\        + Str.count_utf8_bytes(shared_sub)
        \\        + List.len(holder)
        \\}
        ,
    },
    .{
        .name = "str utf8, split, and join",
        .source =
        \\{
        \\    shared = Str.concat("alpha,beta,gamma,", "delta epsilon zeta eta theta")
        \\    holder = [shared, shared]
        \\    unique = Str.concat("alpha,beta,gamma,", "delta epsilon zeta eta iota")
        \\    unique_bytes = Str.to_utf8(unique)
        \\    shared_bytes = Str.to_utf8(shared)
        \\    lossy = Str.from_utf8_lossy(shared_bytes)
        \\    decoded = Str.from_utf8(unique_bytes).ok_or("")
        \\    unique_parts = Str.split_on(unique, ",")
        \\    shared_parts = Str.split_on(shared, ",")
        \\    joined = Str.join_with(unique_parts, "-")
        \\    repeated = Str.repeat("a string long enough to need the heap ", 3)
        \\    empty_with_capacity = Str.with_capacity(128)
        \\    Str.count_utf8_bytes(lossy)
        \\        + Str.count_utf8_bytes(decoded)
        \\        + Str.count_utf8_bytes(joined)
        \\        + Str.count_utf8_bytes(repeated)
        \\        + Str.count_utf8_bytes(empty_with_capacity)
        \\        + List.len(shared_parts)
        \\        + List.len(holder)
        \\}
        ,
    },
    .{
        .name = "numeric to_str allocates a fresh string",
        .source =
        \\{
        \\    parts = [
        \\        U8.to_str(37),
        \\        I8.to_str(-37),
        \\        U16.to_str(1037),
        \\        I16.to_str(-1037),
        \\        U32.to_str(100037),
        \\        I32.to_str(-100037),
        \\        U64.to_str(10000000037),
        \\        I64.to_str(-10000000037),
        \\        U128.to_str(10000000037),
        \\        I128.to_str(-10000000037),
        \\        Dec.to_str(3.25),
        \\        F32.to_str(3.25),
        \\        F64.to_str(3.25),
        \\    ]
        \\    List.len(parts) + Str.count_utf8_bytes(Str.join_with(parts, ","))
        \\}
        ,
    },
    .{
        .name = "list copy-on-write and slice ops, unique and shared inputs",
        .source =
        \\{
        \\    shared = List.concat(
        \\        ["a list element long enough to allocate", "another list element long enough"],
        \\        ["a third list element long enough to allocate"],
        \\    )
        \\    holder = [shared, shared]
        \\    unique = List.concat(
        \\        ["a list element long enough to allocate", "another list element long enough"],
        \\        ["a fourth list element long enough to allocate"],
        \\    )
        \\    unique_reversed = List.rev(unique)
        \\    shared_reversed = List.rev(shared)
        \\    unique_reserved = List.reserve(unique, 64)
        \\    shared_reserved = List.reserve(shared, 64)
        \\    unique_trimmed = List.release_excess_capacity(unique_reserved)
        \\    shared_trimmed = List.release_excess_capacity(shared_reserved)
        \\    unique_dropped = List.drop_at(unique, 1)
        \\    shared_dropped = List.drop_at(shared, 1)
        \\    unique_sublist = List.sublist(unique, { start: 1, len: 2 })
        \\    shared_sublist = List.sublist(shared, { start: 1, len: 2 })
        \\    List.len(unique_reversed)
        \\        + List.len(shared_reversed)
        \\        + List.len(unique_trimmed)
        \\        + List.len(shared_trimmed)
        \\        + List.len(unique_dropped)
        \\        + List.len(shared_dropped)
        \\        + List.len(unique_sublist)
        \\        + List.len(shared_sublist)
        \\        + List.len(holder)
        \\}
        ,
    },
    .{
        .name = "list element ops move ownership in and out",
        .source =
        \\{
        \\    shared = List.concat(
        \\        ["a list element long enough to allocate", "another list element long enough"],
        \\        ["a third list element long enough to allocate"],
        \\    )
        \\    holder = [shared, shared]
        \\    unique = List.concat(
        \\        ["a list element long enough to allocate", "another list element long enough"],
        \\        ["a fourth list element long enough to allocate"],
        \\    )
        \\    element = Str.concat("an element long enough to allocate ", "on the heap")
        \\    unique_prepended = List.prepend(unique, element)
        \\    shared_prepended = List.prepend(shared, element)
        \\    unique_appended = List.append(unique, element)
        \\    shared_appended = List.append(shared, element)
        \\    unique_set = List.set(unique, 0, element).ok_or([])
        \\    shared_set = List.set(shared, 0, element).ok_or([])
        \\    unique_replaced = List.replace(unique, 1, element).ok_or({ list: [], prev: "" })
        \\    shared_replaced = List.replace(shared, 1, element).ok_or({ list: [], prev: "" })
        \\    unique_swapped = List.swap(unique, 0, 1).ok_or([])
        \\    shared_swapped = List.swap(shared, 0, 1).ok_or([])
        \\    with_capacity : List(Str)
        \\    with_capacity = List.with_capacity(8)
        \\    first_len = Str.count_utf8_bytes(List.first(shared).ok_or(""))
        \\    last_len = Str.count_utf8_bytes(List.last(shared).ok_or(""))
        \\    got_len = Str.count_utf8_bytes(List.get(shared, 1).ok_or(""))
        \\    List.len(unique_prepended)
        \\        + List.len(shared_prepended)
        \\        + List.len(unique_appended)
        \\        + List.len(shared_appended)
        \\        + List.len(unique_set)
        \\        + List.len(shared_set)
        \\        + List.len(unique_replaced.list)
        \\        + List.len(shared_replaced.list)
        \\        + List.len(unique_swapped)
        \\        + List.len(shared_swapped)
        \\        + List.len(with_capacity)
        \\        + first_len
        \\        + last_len
        \\        + got_len
        \\        + List.len(holder)
        \\}
        ,
    },
    .{
        .name = "list map reuses or rebuilds its input allocation",
        .source =
        \\{
        \\    shared = List.concat(
        \\        ["a list element long enough to allocate", "another list element long enough"],
        \\        ["a third list element long enough to allocate"],
        \\    )
        \\    holder = [shared, shared]
        \\    unique = List.concat(
        \\        ["a list element long enough to allocate", "another list element long enough"],
        \\        ["a fourth list element long enough to allocate"],
        \\    )
        \\    unique_mapped = List.map(unique, |text| Str.concat(text, "!"))
        \\    shared_mapped = List.map(shared, |text| Str.concat(text, "!"))
        \\    lengths = List.map(shared, |text| Str.count_utf8_bytes(text))
        \\    List.len(unique_mapped) + List.len(shared_mapped) + List.len(lengths) + List.len(holder)
        \\}
        ,
    },
    .{
        .name = "boxed values are allocated, read, and updated",
        .source =
        \\{
        \\    payload = Str.concat("a boxed payload long enough ", "to allocate on the heap")
        \\    holder = [payload, payload]
        \\    read_then_store = |boxed| {
        \\        unboxed = Box.unbox(boxed)
        \\        boxes = [boxed]
        \\        Str.count_utf8_bytes(unboxed) + List.len(boxes)
        \\    }
        \\    read_then_store(Box.box(payload)) + List.len(holder)
        \\}
        ,
    },
    .{
        .name = "crypto hashers allocate their state and digests",
        .source =
        \\{
        \\    message = Str.to_utf8(Str.concat("a message long enough to allocate ", "on the heap"))
        \\    sha_digest = Crypto.SHA256.hash(message)
        \\    sha_incremental = Crypto.SHA256.Hasher.empty()
        \\        .write(message)
        \\        .finish()
        \\    blake_digest = Crypto.BLAKE3.hash(message)
        \\    blake_incremental = Crypto.BLAKE3.Hasher.empty()
        \\        .write(message)
        \\        .finish()
        \\    Str.count_utf8_bytes(Crypto.SHA256.Digest.to_hex(sha_digest))
        \\        + List.len(Crypto.SHA256.Digest.to_bytes(sha_incremental))
        \\        + Str.count_utf8_bytes(Crypto.BLAKE3.Digest.to_hex(blake_digest))
        \\        + List.len(Crypto.BLAKE3.Digest.to_bytes(blake_incremental))
        \\}
        ,
    },
    .{
        .name = "list slice ops return a slice or a copy",
        .source =
        \\{
        \\    shared = List.concat(
        \\        ["a list element long enough to allocate", "another list element long enough"],
        \\        ["a third list element long enough to allocate"],
        \\    )
        \\    holder = [shared, shared]
        \\    unique = List.concat(
        \\        ["a list element long enough to allocate", "another list element long enough"],
        \\        ["a fourth list element long enough to allocate"],
        \\    )
        \\    unique_head = List.drop_first(unique, 1)
        \\    shared_head = List.drop_first(shared, 1)
        \\    unique_tail = List.drop_last(unique, 1)
        \\    shared_tail = List.drop_last(shared, 1)
        \\    unique_taken = List.take_first(unique, 2)
        \\    shared_taken = List.take_first(shared, 2)
        \\    unique_taken_last = List.take_last(unique, 2)
        \\    shared_taken_last = List.take_last(shared, 2)
        \\    unique_split = List.split_first(unique, "another list element long enough")
        \\        .ok_or({ before: [], after: [] })
        \\    shared_split = List.split_first(shared, "another list element long enough")
        \\        .ok_or({ before: [], after: [] })
        \\    unique_split_last = List.split_last(unique, "another list element long enough")
        \\        .ok_or({ before: [], after: [] })
        \\    shared_split_last = List.split_last(shared, "another list element long enough")
        \\        .ok_or({ before: [], after: [] })
        \\    List.len(unique_head)
        \\        + List.len(shared_head)
        \\        + List.len(unique_tail)
        \\        + List.len(shared_tail)
        \\        + List.len(unique_taken)
        \\        + List.len(shared_taken)
        \\        + List.len(unique_taken_last)
        \\        + List.len(shared_taken_last)
        \\        + List.len(unique_split.after)
        \\        + List.len(shared_split.after)
        \\        + List.len(unique_split_last.before)
        \\        + List.len(shared_split_last.before)
        \\        + List.len(holder)
        \\}
        ,
    },
    .{
        .name = "inspect renders values as fresh strings",
        .source =
        \\{
        \\    text = Str.concat("a value long enough to allocate ", "on the heap")
        \\    holder = [text, text]
        \\    rendered = Str.inspect(text)
        \\    rendered_list = Str.inspect([text, text])
        \\    rendered_num = Str.inspect(1234567890.I64)
        \\    Str.count_utf8_bytes(rendered)
        \\        + Str.count_utf8_bytes(rendered_list)
        \\        + Str.count_utf8_bytes(rendered_num)
        \\        + List.len(holder)
        \\}
        ,
    },
    .{
        .name = "SIMD byte vectors load from and store into byte lists",
        .source =
        \\{
        \\    bytes = Str.to_utf8(Str.concat("0123456789abcdef", "0123456789abcdef"))
        \\    holder = [bytes, bytes]
        \\    vector = U8x16.load(bytes, 0).ok_or(U8x16.default())
        \\    stored_unique = U8x16.store(vector, Str.to_utf8(Str.concat("0123456789abcdef", "ghijklmnop")), 0)
        \\        .ok_or([])
        \\    stored_shared = U8x16.store(vector, bytes, 16).ok_or([])
        \\    appended = U8x16.append_to(vector, Str.to_utf8(Str.concat("0123456789abcdef", "qrstuv")))
        \\    appended_shared = U8x16.append_to(vector, bytes)
        \\    List.len(stored_unique)
        \\        + List.len(stored_shared)
        \\        + List.len(appended)
        \\        + List.len(appended_shared)
        \\        + List.len(holder)
        \\}
        ,
    },
    .{
        .name = "boxed payload updates reuse the box allocation",
        .source_kind = .module,
        .source =
        \\grow : Box(Str) -> Box(Str)
        \\grow = |boxed| Box.box(Str.concat(Box.unbox(boxed), "!"))
        \\
        \\main = || {
        \\    payload = Str.concat("a boxed payload long enough ", "to allocate on the heap")
        \\    unique_grown = grow(Box.box(payload))
        \\    shared_box = Box.box(payload)
        \\    holder = [shared_box, shared_box]
        \\    shared_grown = grow(shared_box)
        \\    Str.count_utf8_bytes(Box.unbox(unique_grown))
        \\        + Str.count_utf8_bytes(Box.unbox(shared_grown))
        \\        + List.len(holder)
        \\}
        ,
    },
    .{
        .name = "closures carry captured allocations through erased callables",
        .source_kind = .module,
        .source =
        \\apply : Box((Str -> Str)), Str -> Str
        \\apply = |boxed, text| Box.unbox(boxed)(text)
        \\
        \\main = || {
        \\    prefix = Str.concat("a captured prefix long enough ", "to allocate on the heap")
        \\    holder = [prefix, prefix]
        \\    prepend = Box.box(|text| Str.concat(prefix, text))
        \\    once = apply(prepend, "one")
        \\    twice = apply(prepend, "two")
        \\    Str.count_utf8_bytes(once) + Str.count_utf8_bytes(twice) + List.len(holder)
        \\}
        ,
    },
    .{
        // The TRMC pass rewrites this recursion into a loop that writes each
        // cell through a pointer, which is where `ptr_alloca`,
        // `box_alloc_zeroed`, and `ptr_store` come from.
        .name = "tail-recursion modulo cons builds cells through pointer stores",
        .source_kind = .module,
        .source =
        \\StrList := [Nil, Cons(Str, StrList)]
        \\
        \\repeat : Str, I64 -> StrList
        \\repeat = |value, n| if n <= 0.I64 StrList.Nil else StrList.Cons(value, repeat(value, n - 1))
        \\
        \\length : StrList -> I64
        \\length = |list| match list {
        \\    Nil => 0
        \\    Cons(_, rest) => 1 + length(rest)
        \\}
        \\
        \\main = || {
        \\    text = Str.concat("a repeated element long enough ", "to allocate on the heap")
        \\    holder = [text, text]
        \\    length(repeat(text, 4.I64)) + List.len(holder).to_i64_wrap()
        \\}
        ,
    },
    .{
        // List rest patterns lower to `list_take_first`/`list_take_last` over
        // the matched list, and to `list_get_unsafe` for the fixed elements.
        .name = "list rest patterns slice the matched list",
        .source_kind = .module,
        .source =
        \\count_tail : List(Str) -> U64
        \\count_tail = |items| match items {
        \\    [first, .. as rest] => Str.count_utf8_bytes(first) + List.len(rest)
        \\    [] => 0
        \\}
        \\
        \\count_middle : List(Str) -> U64
        \\count_middle = |items| match items {
        \\    [first, .. as rest, last] =>
        \\        Str.count_utf8_bytes(first) + List.len(rest) + Str.count_utf8_bytes(last)
        \\    _ => 0
        \\}
        \\
        \\main = || {
        \\    shared = List.concat(
        \\        ["a list element long enough to allocate", "another list element long enough"],
        \\        ["a third list element long enough to allocate"],
        \\    )
        \\    holder = [shared, shared]
        \\    count_tail(shared) + count_middle(shared) + List.len(holder)
        \\}
        ,
    },
    .{
        // A wrapper that unboxes, updates, and reboxes is rewritten by
        // `lir/box_reuse.zig` into `box_prepare_update` plus pointer traffic.
        .name = "boxed model updates reuse the box allocation",
        .inline_wrappers = true,
        // The result is the updated box, so the box and the string it holds are
        // still live when the program returns.
        .expected_live_allocations = 2,
        .source =
        \\{
        \\    update : { tick : U64, label : Str } -> { tick : U64, label : Str }
        \\    update = |model| {
        \\        tick = model.tick + 1
        \\        { ..model, tick }
        \\    }
        \\
        \\    step : Box({ tick : U64, label : Str }) -> Box({ tick : U64, label : Str })
        \\    step = |boxed| Box.box(update(Box.unbox(boxed)))
        \\
        \\    label = Str.concat("a model label long enough ", "to allocate on the heap")
        \\    step(Box.box({ tick: 0, label }))
        \\}
        ,
    },
    .{
        .name = "flat boxed update exercises allocation reuse",
        .inline_wrappers = true,
        .expected_live_allocations = 1,
        .source =
        \\{
        \\    step : Box(U64) -> Box(U64)
        \\    step = |boxed| Box.box(Box.unbox(boxed) + 1)
        \\    step(Box.box(0))
        \\}
        ,
    },
    .{
        .name = "list range copies within, unique and shared inputs",
        .source =
        \\{
        \\    shared = List.concat(
        \\        ["a list element long enough to allocate", "another list element long enough"],
        \\        ["a third list element long enough to allocate", "a fourth list element long enough"],
        \\    )
        \\    holder = [shared, shared]
        \\    unique = List.concat(
        \\        ["a list element long enough to allocate", "another list element long enough"],
        \\        ["a fifth list element long enough to allocate", "a sixth list element long enough"],
        \\    )
        \\    # Unique receiver, forward overlap: source range [0, 3) lands at 1.
        \\    unique_fwd = unique.copy_range_within(1, 0, 3).ok_or([])
        \\    # Shared receiver clones; the originals in `holder` stay intact.
        \\    shared_back = shared.copy_range_within(0, 1, 3).ok_or([])
        \\    bytes : List(U8)
        \\    bytes = List.concat([1, 2], [3, 4])
        \\    swapped = bytes.copy_range_within(2, 0, 2).ok_or([])
        \\    # The out-of-bounds copy fails, so `ok_or` yields the empty
        \\    # fallback; going through it keeps the failing op covered with
        \\    # no branch on the impossible success.
        \\    oob_len = List.len(bytes.copy_range_within(3, 0, 2).ok_or([])) + 1
        \\    (swapped.get(2) ?? 0).to_u64()
        \\        + oob_len
        \\        + List.len(unique_fwd)
        \\        + List.len(shared_back)
        \\        + List.len(holder)
        \\}
        ,
    },
    .{
        .name = "list bulk appends, unique and shared inputs",
        .source =
        \\{
        \\    shared = List.concat(
        \\        ["a list element long enough to allocate", "another list element long enough"],
        \\        ["a third list element long enough to allocate"],
        \\    )
        \\    holder = [shared, shared]
        \\    unique = List.concat(
        \\        ["a list element long enough to allocate", "another list element long enough"],
        \\        ["a fourth list element long enough to allocate"],
        \\    )
        \\    unique_range = unique.append_range_within(0, 2).ok_or([])
        \\    shared_range = shared.append_range_within(1, 2).ok_or([])
        \\    source = List.concat(
        \\        ["a source element long enough to allocate", "another source element long enough"],
        \\        ["a third source element long enough to allocate"],
        \\    )
        \\    unique_sub = unique.append_sublist(source, { start: 1, len: 2 })
        \\    shared_sub = shared.append_sublist(source, { start: 0, len: 1 })
        \\    shared_bytes : List(U8)
        \\    shared_bytes = List.concat([1, 2], [3])
        \\    byte_holder = [shared_bytes, shared_bytes]
        \\    unique_bytes : List(U8)
        \\    unique_bytes = List.concat([4, 5], [6])
        \\    unique_le = 0x1234.U64.append_le_bytes_to(unique_bytes, 2).ok_or([])
        \\    shared_le = 0xFF.U64.append_le_bytes_to(shared_bytes, 1).ok_or([])
        \\    List.len(unique_range)
        \\        + List.len(shared_range)
        \\        + List.len(unique_sub)
        \\        + List.len(shared_sub)
        \\        + List.len(unique_le)
        \\        + List.len(shared_le)
        \\        + List.len(holder)
        \\        + List.len(byte_holder)
        \\}
        ,
    },
    .{
        // A self-append inside a promoted loop is rewritten by
        // `lir/loop_append_promote.zig` into a slack-guarded diamond whose hot
        // side is `list_append_range_within_unsafe`; an element overwrite on
        // the same carried list is rewritten into an owned-guarded pair whose
        // hot side is `list_set_in_place_unsafe`.
        .name = "loop self-appends and sets promote to the unchecked variants",
        .source_kind = .module,
        .inline_wrappers = true,
        .source =
        \\main = || {
        \\    start : List(U8)
        \\    start = List.concat([1, 2], [3])
        \\    var $acc = List.reserve(start, 64)
        \\    for _step in 0..<6 {
        \\        appended = $acc.append(7)
        \\        stamped = appended.set(0, 9) ?? appended
        \\        $acc = match stamped.append_range_within(0, 2) {
        \\            Ok(next) => next
        \\            Err(_) => stamped
        \\        }
        \\    }
        \\    List.len($acc)
        \\}
        ,
    },
};

/// Ops that this source-level sweep cannot reach. Each needs a reason; the
/// sweep fails when one turns out to be covered after all.
///
/// Every entry here is an op nothing produces: no name in `Builtin.roc` maps to
/// it through `canonicalize/BuiltinLowLevel.zig`, and no lowering pass emits
/// it. They are reachable only from a backend's switch, which is why their rows
/// have gone unchecked. Wiring one up is what makes its row matter, and doing
/// that removes it from this table.
const exemptions = [_]Exemption{
    .{ .op = .box_unbox, .reason = "allocation-consuming compiled variant is pinned by focused LIR and runtime-helper tests" },
    .{ .op = .list_first, .reason = "no producer: List.first lowers through list_get_unsafe" },
    .{ .op = .list_last, .reason = "no producer: List.last lowers through list_get_unsafe" },
    .{ .op = .list_drop_first, .reason = "no producer: List.drop_first lowers through list_sublist" },
    .{ .op = .list_drop_last, .reason = "no producer: List.drop_last lowers through list_sublist" },
    .{ .op = .list_reverse, .reason = "no producer: List.rev is written in Roc over list_get_unsafe" },
    .{ .op = .list_split_first, .reason = "no producer: List.split_first is written in Roc" },
    .{ .op = .list_split_last, .reason = "no producer: List.split_last is written in Roc" },
    .{ .op = .num_to_str, .reason = "no producer: each numeric type maps to its own <type>_to_str op" },
};

/// Loaded once and kept for the life of the test binary: publishing the
/// Builtin module for every case would dominate the sweep's runtime.
var shared_builtins: ?eval.BuiltinModules = null;

fn sharedPrePublishedBuiltin(allocator: Allocator) SweepError!helpers.PrePublishedBuiltin {
    if (shared_builtins == null) {
        shared_builtins = try eval.BuiltinModules.init(allocator);
    }
    return .{
        .env = shared_builtins.?.builtin_module.env,
        .indices = shared_builtins.?.builtin_indices,
        .artifact = &shared_builtins.?.checked_artifact,
    };
}

/// Fail with rendered diagnostics when a case's source does not check.
///
/// A source with problems still lowers to something runnable, but not to the
/// program the case meant to write, and its ops quietly go uncovered.
fn assertCaseChecks(allocator: Allocator, case: Case) SweepError!void {
    var resources = try helpers.parseAndCheckProgramForProblemsWithBuiltin(
        allocator,
        case.source_kind,
        case.source,
        &.{},
        try sharedPrePublishedBuiltin(allocator),
    );
    defer resources.deinit(allocator);

    const diagnostics = try resources.main.module_env.getDiagnostics();
    defer allocator.free(diagnostics);

    const problems = resources.main.parse_ast.tokenize_diagnostics.items.len +
        resources.main.parse_ast.parse_diagnostics.items.len +
        diagnostics.len +
        resources.main.checker.problems.problems.items.len;
    if (problems == 0) return;

    const rendered = try helpers.renderProblems(allocator, case.source_kind, case.source);
    defer allocator.free(rendered);
    std.debug.print("rc conformance: [{s}] source does not check:\n{s}\n", .{ case.name, rendered });
    return error.CaseSourceDoesNotCheck;
}

/// Compile and run one case with the observer watching every low-level op.
fn runCase(allocator: Allocator, case: Case) SweepError!void {
    // `compileAllocationProgram` is the only public entry that lowers with
    // wrapper inlining, and it does not take a pre-published Builtin, so cases
    // pay for loading one only when they need that lowering.
    var wrapper_compiled = if (case.inline_wrappers)
        try helpers.compileAllocationProgram(allocator, std.testing.io, case.source_kind, case.source, &.{})
    else
        null;
    defer if (wrapper_compiled) |*compiled| compiled.deinit(allocator);

    var plain_compiled = if (case.inline_wrappers)
        null
    else
        try helpers.compileProgramForTargetWithBuiltin(
            allocator,
            std.testing.io,
            case.source_kind,
            case.source,
            &.{},
            .native,
            try sharedPrePublishedBuiltin(allocator),
        );
    defer if (plain_compiled) |*compiled| compiled.deinit(allocator);

    const lowered = if (wrapper_compiled) |*compiled| &compiled.lowered else &plain_compiled.?.lowered;

    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();

    var interp = try Interpreter.initWithBoxyTables(
        allocator,
        &lowered.view.store,
        &lowered.view.layouts,
        Interpreter.BoxyTables.fromImageView(&lowered.view),
        runtime_env.get_ops(),
        .preserve,
    );
    defer interp.deinit();

    const arg_layouts = try helpers.mainProcArgLayouts(allocator, lowered);
    defer allocator.free(arg_layouts);

    rc_conformance.begin();
    defer rc_conformance.end();

    switch (try interp.eval(.{
        .proc_id = lowered.mainProc(),
        .arg_layouts = arg_layouts,
    })) {
        .value => {},
    }

    // A row that leaks a reference shows up here as well as in the per-op
    // findings: whatever the program still holds when it returns is exactly
    // what its value keeps alive, and nothing more.
    var snapshot = try runtime_env.snapshot(allocator);
    defer snapshot.deinit(allocator);
    if (snapshot.allocation_count != case.expected_live_allocations) {
        std.debug.print(
            "rc conformance: [{s}] {d} allocations live at return, expected {d}\n",
            .{ case.name, snapshot.allocation_count, case.expected_live_allocations },
        );
        return error.RcEffectConformanceFailed;
    }
}

fn reportFindings(case_name: []const u8) usize {
    const found = rc_conformance.findings();
    for (found) |finding| {
        std.debug.print("rc conformance: [{s}] {f}\n", .{ case_name, finding });
    }
    if (rc_conformance.droppedFindings() > 0) {
        std.debug.print(
            "rc conformance: [{s}] {d} further findings dropped\n",
            .{ case_name, rc_conformance.droppedFindings() },
        );
    }
    return found.len + rc_conformance.droppedFindings();
}

fn exemptOps() rc_conformance.OpSet {
    var set = rc_conformance.OpSet.initEmpty();
    for (exemptions) |exemption| set.insert(exemption.op);
    return set;
}

test "rc effect conformance: every case source checks cleanly" {
    const allocator = base.defaultGpa();
    var failures: usize = 0;
    for (cases) |case| {
        assertCaseChecks(allocator, case) catch |err| {
            std.debug.print("rc conformance: [{s}] {s}\n", .{ case.name, @errorName(err) });
            failures += 1;
        };
    }
    if (failures != 0) return error.CaseSourceDoesNotCheck;
}

test "rc effect conformance: every executed op matches its row" {
    if (!rc_conformance.enabled) return error.SkipZigTest;

    const allocator = base.defaultGpa();
    var covered = rc_conformance.OpSet.initEmpty();
    var failures: usize = 0;

    for (cases) |case| {
        runCase(allocator, case) catch |err| {
            std.debug.print("rc conformance: [{s}] run failed: {s}\n", .{ case.name, @errorName(err) });
            failures += 1;
        };
        covered.setUnion(rc_conformance.covered());
        failures += reportFindings(case.name);
    }

    var gaps = rc_conformance.OpSet.initEmpty();
    rc_conformance.coverageGaps(covered, exemptOps(), &gaps);
    if (gaps.count() > 0) {
        var it = gaps.iterator();
        while (it.next()) |op| {
            std.debug.print(
                "rc conformance: op {s} has a nontrivial RcEffect row and no case drives it\n",
                .{@tagName(op)},
            );
        }
        failures += gaps.count();
    }

    var stale = rc_conformance.OpSet.initEmpty();
    rc_conformance.staleExemptions(covered, exemptOps(), &stale);
    if (stale.count() > 0) {
        var it = stale.iterator();
        while (it.next()) |op| {
            std.debug.print(
                "rc conformance: op {s} is exempt from coverage but the sweep covered it\n",
                .{@tagName(op)},
            );
        }
        failures += stale.count();
    }

    if (failures != 0) return error.RcEffectConformanceFailed;
}

test "rc effect conformance: the #10023 row fails against the real builtin" {
    if (!rc_conformance.enabled) return error.SkipZigTest;

    const allocator = base.defaultGpa();

    // `Str.drop_prefix` returns a slice of its argument's allocation. Before PR
    // roc-lang/roc#10023 its row also claimed `result_unique`, so ARC counted a
    // birth on top of the link to the lender and every call leaked one
    // reference to the input string.
    var reintroduced = RcEffect.retainsSharingArgs(1);
    reintroduced.result_unique = true;
    rc_conformance.overrideRow(.str_drop_prefix, reintroduced);
    defer rc_conformance.clearOverrides();

    const case = Case{
        .name = "shared input to Str.drop_prefix",
        .source =
        \\{
        \\    shared = Str.concat("prefix-a string long enough to need the heap ", "plus a tail")
        \\    holder = [shared, shared]
        \\    dropped = Str.drop_prefix(shared, "prefix-")
        \\    Str.count_utf8_bytes(dropped) + List.len(holder)
        \\}
        ,
    };

    try runCase(allocator, case);

    var saw_uniqueness_finding = false;
    for (rc_conformance.findings()) |finding| {
        if (finding.op != .str_drop_prefix) continue;
        if (finding.rule != .result_outlives_uniqueness_claim) continue;
        saw_uniqueness_finding = true;
    }
    try std.testing.expect(saw_uniqueness_finding);
}
