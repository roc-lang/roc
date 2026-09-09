//! Hash-backed Set API and representation regression tests.

const TestCase = @import("parallel_runner.zig").TestCase;

/// Set API and storage regressions shared by the evaluator backends.
pub const tests = [_]TestCase{
    .{
        .name = "Set.JSON parsing deduplicates and encoding preserves iteration order",
        .source =
        \\{
        \\    parsed : Try(Set(Str), [InvalidJson(Str)])
        \\    parsed = Json.parse("[\"first\",\"second\",\"first\"]")
        \\    match parsed {
        \\        Ok(set) => set.to_list() == ["first", "second"] and Json.to_str(set) == "[\"first\",\"second\"]"
        \\        Err(_) => Bool.False
        \\    }
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "Set.capacity reserve clear and release preserve shared values",
        .source =
        \\{
        \\    original = Set.with_capacity(20).insert("a long refcounted value for the original set").insert("another long refcounted value for the original set")
        \\    reserved = original.reserve(100)
        \\    cleared = reserved.clear()
        \\    reused = cleared.insert("a new long refcounted value after clearing")
        \\    shrunk = reserved.release_excess_capacity()
        \\    original.len() == 2 and original.capacity() >= 20 and reserved.capacity() >= 102 and cleared.is_empty() and cleared.capacity() == reserved.capacity() and reused.len() == 1 and !reused.contains("a long refcounted value for the original set") and shrunk == original and shrunk.capacity() < reserved.capacity() and original.contains("a long refcounted value for the original set") and cleared.release_excess_capacity().capacity() == 0
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "Set.empty capacity operations and subscript",
        .source =
        \\{
        \\    s : Set(U64)
        \\    s = Set.with_capacity(0)
        \\    s.capacity() == 0 and s.reserve(0).capacity() == 0 and s.clear().capacity() == 0 and !s.subscript(1) and s.insert(1).subscript(1)
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "Set.from_iter skips deduplicates and collects",
        .source =
        \\{
        \\    values = [3.U64, 1, 3, 2, 4, 1].iter().keep_if(|n| n != 2)
        \\    s : Set(U64)
        \\    s = values.collect()
        \\    empty : List(U64)
        \\    empty = []
        \\    s.to_list() == [3, 1, 4] and s.iter().size_hint() == Known(3) and List.from_iter(s.iter_rev()) == [4, 1, 3] and Set.from_iter(empty.iter()).len() == 0
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "Set.fold_until stops before later callbacks",
        .source =
        \\{
        \\    s = Set.from_list([1.U64, 2, 3])
        \\    early = s.fold_until(0, |acc, item| if item == 2 { Break(acc + item) } else if item == 3 { crash "fold continued after Break" } else { Continue(acc + item) })
        \\    all = s.fold_until(0, |acc, item| Continue(acc + item))
        \\    empty = Set.empty().fold_until(7.U64, |_, _| crash "empty set called step")
        \\    early == 3 and all == 6 and empty == 7
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "Set.join_map collapses overlaps in traversal order",
        .source =
        \\{
        \\    s = Set.from_list([1.U64, 2, 3])
        \\    joined = s.join_map(|n| Set.from_list([n, n + 1]))
        \\    empty : Set(U64)
        \\    empty = Set.empty()
        \\    joined.to_list() == [1, 2, 3, 4] and s.join_map(|_| empty).is_empty() and empty.join_map(|n| Set.single(n + 1)).is_empty()
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "Set.insert retains first representative with coherent custom hash",
        .source_kind = .module,
        .source =
        \\Key := { key : U64, label : Str }.{
        \\    is_eq : Key, Key -> Bool
        \\    is_eq = |a, b| a.key == b.key
        \\    to_hash : Key, Hasher -> Hasher
        \\    to_hash = |value, hasher| value.key.to_hash(hasher)
        \\}
        \\main = {
        \\    first = Key.{ key: 1, label: "first representative with refcounted storage" }
        \\    second = Key.{ key: 1, label: "second representative with refcounted storage" }
        \\    s = Set.from_iter([first, second].iter()).insert(second)
        \\    List.map(s.to_list(), |x| x.label) == [first.label] and s.len() == 1 and s.contains(second)
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "Set.collisions growth removal and rebuild keep every live key",
        .source_kind = .module,
        .source =
        \\Key := { value : U64 }.{
        \\    is_eq : Key, Key -> Bool
        \\    is_eq = |a, b| a.value == b.value
        \\    to_hash : Key, Hasher -> Hasher
        \\    to_hash = |_, hasher| Hasher.write_u64(hasher, 0)
        \\}
        \\main = {
        \\    var $set = Set.empty()
        \\    for n in 0.U64..<80 {
        \\        $set = $set.insert(Key.{ value: n })
        \\    }
        \\    original = $set
        \\    for n in 0.U64..<40 {
        \\        $set = $set.remove(Key.{ value: n })
        \\    }
        \\    rebuilt = $set.reserve(120).release_excess_capacity()
        \\    var $ok = rebuilt.len() == 40 and original.len() == 80
        \\    for n in 0.U64..<80 {
        \\        $ok = $ok and original.contains(Key.{ value: n }) and (rebuilt.subscript(Key.{ value: n }) == (n >= 40))
        \\    }
        \\    $ok
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "Set.algebra and map use set membership",
        .source =
        \\{
        \\    a = Set.from_list([1.U64, 2, 3, 4])
        \\    b = Set.from_list([3.U64, 4, 5])
        \\    a.union(b).to_list() == [1, 2, 3, 4, 5] and a.intersection(b) == Set.from_list([3.U64, 4]) and a.difference(b) == Set.from_list([1.U64, 2]) and a.keep_if(|n| n > 2) == Set.from_list([3.U64, 4]) and a.drop_if(|n| n > 2) == Set.from_list([1.U64, 2]) and a.map(|n| n % 2) == Set.from_list([0.U64, 1]) and a.to_list() == [1, 2, 3, 4]
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "Set.nested sets hash independently of insertion order",
        .source =
        \\{
        \\    a = Set.from_list([1.U64, 2])
        \\    b = Set.from_list([2.U64, 1])
        \\    nested = Set.from_list([a, b, Set.single(3.U64)])
        \\    nested.len() == 2 and nested.contains(b) and nested.remove(b) == Set.single(Set.single(3.U64))
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "Set.zero sized items",
        .source =
        \\{
        \\    s = Set.with_capacity(10).insert({}).insert({})
        \\    s.len() == 1 and s.contains({}) and s.to_list() == [{}] and s.remove({}).is_empty() and s.clear().capacity() == s.capacity()
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "Set.remove uses last entry and duplicate insert preserves position",
        .source =
        \\Set.from_list([1.U64, 2, 3, 4]).insert(2).remove(1).to_list()
        ,
        .expected = .{ .inspect_str = "[4, 2, 3]" },
    },
};
