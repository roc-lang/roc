//! Ported lambda/closure/recursion/container eval coverage from origin/main.

const TestCase = @import("parallel_runner.zig").TestCase;

/// Public value `tests`.
pub const tests = [_]TestCase{
    // Ported from interpreter_style_test.zig
    .{
        .name = "inspect: inline fold sum lambda",
        .source =
        \\(|list, init, step| {
        \\    var $state = init
        \\    for item in list {
        \\        $state = step($state, item)
        \\    }
        \\    $state
        \\})([1, 2, 3, 4], 0, |acc, x| acc + x)
        ,
        .expected = .{ .inspect_str = "10.0" },
    },
    .{
        .name = "inspect: inline fold product lambda",
        .source =
        \\(|list, init, step| {
        \\    var $state = init
        \\    for item in list {
        \\        $state = step($state, item)
        \\    }
        \\    $state
        \\})([2, 3, 4], 1, |acc, x| acc * x)
        ,
        .expected = .{ .inspect_str = "24.0" },
    },
    .{
        .name = "inspect: inline fold empty list lambda",
        .source =
        \\(|list, init, step| {
        \\    var $state = init
        \\    for item in list {
        \\        $state = step($state, item)
        \\    }
        \\    $state
        \\})([], 42, |acc, x| acc + x)
        ,
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "inspect: inline fold counts elements lambda",
        .source =
        \\(|list, init, step| {
        \\    var $state = init
        \\    for item in list {
        \\        $state = step($state, item)
        \\    }
        \\    $state
        \\})([10, 20, 30, 40], 0, |acc, _| acc + 1)
        ,
        .expected = .{ .inspect_str = "4.0" },
    },
    .{
        .name = "inspect: recursive function with var keeps outer binding",
        .source =
        \\{
        \\    f = |n| {
        \\        var $state = n
        \\        if n > 0 {
        \\            inner = f(n - 1)
        \\            $state + inner
        \\        } else {
        \\            $state
        \\        }
        \\    }
        \\    f(3)
        \\}
        ,
        .expected = .{ .inspect_str = "6.0" },
    },
    .{
        .name = "inspect: simple early return from function via bool",
        .source =
        \\{
        \\    f = |x| if x { return True } else { False }
        \\    f(True)
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: early return in for loop predicate function",
        .source =
        \\{
        \\    f = |list| {
        \\        for item in list {
        \\            if item == 2 {
        \\                return True
        \\            }
        \\        }
        \\        False
        \\    }
        \\    f([1, 2, 3])
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: tuple pattern var reassignment in while loop",
        .source =
        \\{
        \\    get_pair = |n| ("word", n + 1)
        \\    var $index = 0
        \\    while $index < 3 {
        \\        (word, $index) = get_pair($index)
        \\        word
        \\    }
        \\    $index
        \\}
        ,
        .expected = .{ .inspect_str = "3.0" },
    },

    // Ported from list_refcount_alias.zig
    .{
        .name = "inspect: list alias variable aliasing",
        .source =
        \\{
        \\    x = [1, 2, 3]
        \\    y = x
        \\    match y { [a, b, c] => a + b + c, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "6.0" },
    },
    .{
        .name = "inspect: list alias return original after aliasing",
        .source =
        \\{
        \\    x = [1, 2, 3]
        \\    _y = x
        \\    match x { [a, b, c] => a + b + c, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "6.0" },
    },
    .{
        .name = "inspect: list alias triple aliasing",
        .source =
        \\{
        \\    x = [1, 2]
        \\    y = x
        \\    z = y
        \\    match z { [a, b] => a + b, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "3.0" },
    },
    .{
        .name = "inspect: list alias mutable reassignment",
        .source =
        \\{
        \\    var $x = [1, 2]
        \\    $x = [3, 4]
        \\    match $x { [a, b] => a + b, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "7.0" },
    },
    .{
        .name = "inspect: list alias multiple independent lists",
        .source =
        \\{
        \\    x = [1, 2]
        \\    _y = [3, 4]
        \\    match x { [a, b] => a + b, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "3.0" },
    },
    .{
        .name = "inspect: list alias empty list aliasing",
        .source =
        \\{
        \\    x = []
        \\    y = x
        \\    match y { [] => 42, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "inspect: list alias shadow after alias",
        .source =
        \\{
        \\    var $x = [1, 2]
        \\    y = $x
        \\    $x = [3, 4]
        \\    match y { [a, b] => a + b, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "3.0" },
    },
    .{
        .name = "inspect: list alias both references used",
        .source =
        \\{
        \\    x = [1, 2]
        \\    y = x
        \\    a = match x { [first, ..] => first, _ => 0 }
        \\    b = match y { [first, ..] => first, _ => 0 }
        \\    a + b
        \\}
        ,
        .expected = .{ .inspect_str = "2.0" },
    },

    // Ported from list_refcount_function.zig
    .{
        .name = "inspect: list through identity function",
        .source =
        \\{
        \\    id = |lst| lst
        \\    x = [1, 2]
        \\    result = id(x)
        \\    match result { [a, b] => a + b, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "3.0" },
    },
    .{
        .name = "inspect: list returned from function",
        .source =
        \\{
        \\    f = |_| [1, 2]
        \\    result = f(0)
        \\    match result { [a, b] => a + b, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "3.0" },
    },
    .{
        .name = "inspect: closure captures list and returns it",
        .source =
        \\{
        \\    x = [1, 2]
        \\    f = |_| x
        \\    result = f(0)
        \\    match result { [a, b] => a + b, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "3.0" },
    },
    .{
        .name = "inspect: function called multiple times with same list",
        .source =
        \\{
        \\    f = |lst| lst
        \\    x = [1, 2]
        \\    a = f(x)
        \\    _b = f(x)
        \\    match a { [first, ..] => first, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "1.0" },
    },
    .{
        .name = "inspect: string list through function",
        .source =
        \\{
        \\    f = |lst| lst
        \\    x = ["a", "b"]
        \\    result = f(x)
        \\    match result { [first, ..] => first, _ => "" }
        \\}
        ,
        .expected = .{ .inspect_str = "\"a\"" },
    },
    .{
        .name = "inspect: function extracts from list",
        .source =
        \\{
        \\    x = [10, 20, 30]
        \\    match x { [first, ..] => first, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "10.0" },
    },
    .{
        .name = "inspect: closure captures string list",
        .source =
        \\{
        \\    x = ["captured", "list"]
        \\    f = |_| x
        \\    result = f(0)
        \\    match result { [first, ..] => first, _ => "" }
        \\}
        ,
        .expected = .{ .inspect_str = "\"captured\"" },
    },
    .{
        .name = "inspect: nested function calls with lists",
        .source =
        \\{
        \\    x = [5, 10]
        \\    match x { [first, ..] => first + first, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "10.0" },
    },
    .{
        .name = "inspect: function returns tuple with same list twice",
        .source =
        \\{
        \\    make_pair = |lst| (lst, lst)
        \\    x = [1, 2]
        \\    t = make_pair(x)
        \\    match t { (first, _) => match first { [a, b] => a + b, _ => 0 } }
        \\}
        ,
        .expected = .{ .inspect_str = "3.0" },
    },
    .{
        .name = "inspect: same list passed twice to function",
        .source =
        \\{
        \\    add_lens = |a, b|
        \\        match a {
        \\            [first, ..] => match b { [second, ..] => first + second, _ => 0 },
        \\            _ => 0
        \\        }
        \\    x = [1, 2]
        \\    add_lens(x, x)
        \\}
        ,
        .expected = .{ .inspect_str = "2.0" },
    },

    // Ported from list_refcount_containers.zig
    .{
        .name = "inspect: list in tuple single list",
        .source =
        \\{
        \\    x = [1, 2]
        \\    match x { [a, b] => a + b, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "3.0" },
    },
    .{
        .name = "inspect: multiple lists in tuple",
        .source =
        \\{
        \\    x = [1, 2]
        \\    y = [3, 4]
        \\    t = (x, y)
        \\    match t { (first, _) => match first { [a, b] => a + b, _ => 0 } }
        \\}
        ,
        .expected = .{ .inspect_str = "3.0" },
    },
    .{
        .name = "inspect: same list twice in tuple",
        .source =
        \\{
        \\    x = [1, 2]
        \\    t = (x, x)
        \\    match t { (first, _) => match first { [a, b] => a + b, _ => 0 } }
        \\}
        ,
        .expected = .{ .inspect_str = "3.0" },
    },
    .{
        .name = "inspect: tuple with string list",
        .source =
        \\{
        \\    x = ["a", "b"]
        \\    t = (x, 42)
        \\    match t { (lst, _) => match lst { [first, ..] => first, _ => "" } }
        \\}
        ,
        .expected = .{ .inspect_str = "\"a\"" },
    },
    .{
        .name = "inspect: record with list field",
        .source =
        \\{
        \\    lst = [1, 2, 3]
        \\    r = { items: lst }
        \\    match r.items { [a, b, c] => a + b + c, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "6.0" },
    },
    .{
        .name = "inspect: record with multiple list fields",
        .source =
        \\{
        \\    x = [1, 2]
        \\    y = [3, 4]
        \\    r = { first: x, second: y }
        \\    match r.first { [a, b] => a + b, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "3.0" },
    },
    .{
        .name = "inspect: same list in multiple record fields",
        .source =
        \\{
        \\    lst = [10, 20]
        \\    r = { a: lst, b: lst }
        \\    match r.a { [x, y] => x + y, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "30.0" },
    },
    .{
        .name = "inspect: nested record with list",
        .source =
        \\{
        \\    lst = [5, 6]
        \\    inner = { data: lst }
        \\    outer = { nested: inner }
        \\    match outer.nested.data { [a, b] => a + b, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "11.0" },
    },
    .{
        .name = "inspect: record with string list",
        .source =
        \\{
        \\    lst = ["hello", "world"]
        \\    r = { items: lst }
        \\    match r.items { [first, ..] => first, _ => "" }
        \\}
        ,
        .expected = .{ .inspect_str = "\"hello\"" },
    },
    .{
        .name = "inspect: record with mixed count and list",
        .source =
        \\{
        \\    lst = [1, 2, 3]
        \\    r = { count: 42, items: lst }
        \\    r.count
        \\}
        ,
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "inspect: tag with list payload",
        .source =
        \\match Some([1, 2]) { Some(lst) => match lst { [a, b] => a + b, _ => 0 }, None => 0 }
        ,
        .expected = .{ .inspect_str = "3.0" },
    },
    .{
        .name = "inspect: tag with multiple list payloads",
        .source =
        \\{
        \\    x = [1, 2]
        \\    y = [3, 4]
        \\    tag = Pair(x, y)
        \\    match tag { Pair(first, _) => match first { [a, b] => a + b, _ => 0 }, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "3.0" },
    },
    .{
        .name = "inspect: tag with string list payload",
        .source =
        \\match Some(["tag", "value"]) { Some(lst) => match lst { [first, ..] => first, _ => "" }, None => "" }
        ,
        .expected = .{ .inspect_str = "\"tag\"" },
    },
    .{
        .name = "inspect: result with list payload",
        .source =
        \\match Ok([1, 2, 3]) { Ok(lst) => match lst { [a, b, c] => a + b + c, _ => 0 }, Err(_) => 0 }
        ,
        .expected = .{ .inspect_str = "6.0" },
    },
    .{
        .name = "inspect: tuple of records with lists",
        .source =
        \\{
        \\    lst1 = [1, 2]
        \\    lst2 = [3, 4]
        \\    r1 = { items: lst1 }
        \\    r2 = { items: lst2 }
        \\    t = (r1, r2)
        \\    match t { (first, _) => match first.items { [a, b] => a + b, _ => 0 } }
        \\}
        ,
        .expected = .{ .inspect_str = "3.0" },
    },
    .{
        .name = "inspect: record of tuples with lists",
        .source =
        \\{
        \\    lst = [5, 6]
        \\    t = (lst, 99)
        \\    r = { data: t }
        \\    match r.data { (items, _) => match items { [a, b] => a + b, _ => 0 } }
        \\}
        ,
        .expected = .{ .inspect_str = "11.0" },
    },
    .{
        .name = "inspect: tag with record containing list",
        .source =
        \\{
        \\    lst = [7, 8]
        \\    r = { items: lst }
        \\    tag = Some(r)
        \\    match tag { Some(rec) => match rec.items { [a, b] => a + b, _ => 0 }, None => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "15.0" },
    },
    .{
        .name = "inspect: empty list in record",
        .source =
        \\{
        \\    empty = []
        \\    r = { lst: empty }
        \\    match r.lst { [] => 42, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "42.0" },
    },

    // Ported from list_refcount_pattern.zig
    .{
        .name = "inspect: destructure list from record",
        .source =
        \\{
        \\    r = { lst: [1, 2] }
        \\    match r { { lst } => match lst { [a, b] => a + b, _ => 0 } }
        \\}
        ,
        .expected = .{ .inspect_str = "3.0" },
    },
    .{
        .name = "inspect: wildcard discards list field",
        .source =
        \\{
        \\    pair = { a: [1, 2], b: [3, 4] }
        \\    match pair { { a, b: _ } => match a { [x, y] => x + y, _ => 0 } }
        \\}
        ,
        .expected = .{ .inspect_str = "3.0" },
    },
    .{
        .name = "inspect: list rest pattern on integers",
        .source =
        \\match [1, 2, 3, 4] { [first, .. as rest] => match rest { [second, ..] => first + second, _ => 0 }, _ => 0 }
        ,
        .expected = .{ .inspect_str = "3.0" },
    },
    .{
        .name = "inspect: string list rest pattern",
        .source =
        \\match ["a", "b", "c"] { [_first, .. as rest] => match rest { [second, ..] => second, _ => "" }, _ => "" }
        ,
        .expected = .{ .inspect_str = "\"b\"" },
    },
    .{
        .name = "inspect: nested list patterns through record",
        .source =
        \\{
        \\    data = { values: [10, 20, 30] }
        \\    match data { { values } => match values { [a, b, c] => a + b + c, _ => 0 } }
        \\}
        ,
        .expected = .{ .inspect_str = "60.0" },
    },
    .{
        .name = "inspect: tag with extracted list payload",
        .source =
        \\match Some([5, 10]) { Some(lst) => match lst { [a, b] => a + b, _ => 0 }, None => 0 }
        ,
        .expected = .{ .inspect_str = "15.0" },
    },
    .{
        .name = "inspect: empty list pattern through record",
        .source =
        \\match { lst: [] } { { lst } => match lst { [] => 42, _ => 0 } }
        ,
        .expected = .{ .inspect_str = "42.0" },
    },

    // Ported from list_refcount_nested.zig
    .{
        .name = "inspect: simple nested list",
        .source =
        \\{
        \\    inner = [1, 2]
        \\    outer = [inner]
        \\    match outer { [lst] => match lst { [a, b] => a + b, _ => 0 }, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "3.0" },
    },
    .{
        .name = "inspect: multiple inner lists",
        .source =
        \\{
        \\    a = [1, 2]
        \\    b = [3, 4]
        \\    outer = [a, b]
        \\    match outer { [first, ..] => match first { [x, y] => x + y, _ => 0 }, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "3.0" },
    },
    .{
        .name = "inspect: same inner list multiple times",
        .source =
        \\{
        \\    inner = [1, 2]
        \\    outer = [inner, inner, inner]
        \\    match outer { [first, ..] => match first { [a, b] => a + b, _ => 0 }, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "3.0" },
    },
    .{
        .name = "inspect: two level inline nested list",
        .source =
        \\match [[1, 2], [3, 4]] { [first, ..] => match first { [a, b] => a + b, _ => 0 }, _ => 0 }
        ,
        .expected = .{ .inspect_str = "3.0" },
    },
    .{
        .name = "inspect: three level nested list",
        .source =
        \\{
        \\    a = [1]
        \\    b = [a]
        \\    c = [b]
        \\    match c { [lst] => match lst { [lst2] => match lst2 { [x] => x, _ => 0 }, _ => 0 }, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "1.0" },
    },
    .{
        .name = "inspect: nested empty inner list",
        .source =
        \\{
        \\    inner = []
        \\    outer = [inner]
        \\    match outer { [lst] => match lst { [] => 42, _ => 0 }, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "inspect: list of string lists",
        .source =
        \\{
        \\    a = ["x", "y"]
        \\    b = ["z"]
        \\    outer = [a, b]
        \\    match outer { [first, ..] => match first { [s, ..] => s, _ => "" }, _ => "" }
        \\}
        ,
        .expected = .{ .inspect_str = "\"x\"" },
    },
    .{
        .name = "inspect: inline string nested lists",
        .source =
        \\match [["a", "b"], ["c"]] { [first, ..] => match first { [s, ..] => s, _ => "" }, _ => "" }
        ,
        .expected = .{ .inspect_str = "\"a\"" },
    },
    .{
        .name = "inspect: nested list then aliased",
        .source =
        \\{
        \\    inner = [1, 2]
        \\    outer = [inner]
        \\    outer2 = outer
        \\    match outer2 { [lst] => match lst { [a, b] => a + b, _ => 0 }, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "3.0" },
    },
    .{
        .name = "inspect: access second inner list",
        .source =
        \\{
        \\    a = [1, 2]
        \\    b = [3, 4]
        \\    outer = [a, b]
        \\    match outer { [_, second] => match second { [x, y] => x + y, _ => 0 }, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "7.0" },
    },
    .{
        .name = "inspect: deeply nested inline list",
        .source =
        \\match [[[1]]] { [lst] => match lst { [lst2] => match lst2 { [x] => x, _ => 0 }, _ => 0 }, _ => 0 }
        ,
        .expected = .{ .inspect_str = "1.0" },
    },
    .{
        .name = "inspect: mixed nested and flat lists",
        .source =
        \\match [[1, 2], [3]] { [first, second] => {
        \\    a = match first { [x, ..] => x, _ => 0 }
        \\    b = match second { [y] => y, _ => 0 }
        \\    a + b
        \\}, _ => 0 }
        ,
        .expected = .{ .inspect_str = "4.0" },
    },

    // Ported from list_refcount_simple.zig
    .{
        .name = "inspect: minimal empty list pattern match",
        .source = "match [] { [] => 42, _ => 0 }",
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "inspect: minimal single element list pattern match",
        .source = "match [1] { [x] => x, _ => 0 }",
        .expected = .{ .inspect_str = "1.0" },
    },
    .{
        .name = "inspect: minimal multi element list pattern match",
        .source = "match [1, 2, 3] { [a, b, c] => a + b + c, _ => 0 }",
        .expected = .{ .inspect_str = "6.0" },
    },

    // Ported from list_refcount_basic.zig
    .{
        .name = "inspect: basic various small list sizes",
        .source = "match [5] { [x] => x, _ => 0 }",
        .expected = .{ .inspect_str = "5.0" },
    },
    .{
        .name = "inspect: basic two element list pattern match",
        .source = "match [10, 20] { [a, b] => a + b, _ => 0 }",
        .expected = .{ .inspect_str = "30.0" },
    },
    .{
        .name = "inspect: basic five element list pattern match",
        .source = "match [1, 2, 3, 4, 5] { [a, b, c, d, e] => a + b + c + d + e, _ => 0 }",
        .expected = .{ .inspect_str = "15.0" },
    },
    .{
        .name = "inspect: basic larger list with rest pattern",
        .source = "match [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] { [first, second, ..] => first + second, _ => 0 }",
        .expected = .{ .inspect_str = "3.0" },
    },
    .{
        .name = "inspect: basic sequential independent lists",
        .source =
        \\{
        \\    a = [1]
        \\    _b = [2, 3]
        \\    _c = [4, 5, 6]
        \\    match a { [x] => x, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "1.0" },
    },
    .{
        .name = "inspect: basic return middle list",
        .source =
        \\{
        \\    _a = [1]
        \\    b = [2, 3]
        \\    _c = [4, 5, 6]
        \\    match b { [x, y] => x + y, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "5.0" },
    },
    .{
        .name = "inspect: basic return last list",
        .source =
        \\{
        \\    _a = [1]
        \\    _b = [2, 3]
        \\    c = [4, 5, 6]
        \\    match c { [x, y, z] => x + y + z, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "15.0" },
    },
    .{
        .name = "inspect: basic mix of empty and non empty lists",
        .source =
        \\{
        \\    _x = []
        \\    y = [1, 2]
        \\    _z = []
        \\    match y { [a, b] => a + b, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "3.0" },
    },
    .{
        .name = "inspect: basic return empty from mixed lists",
        .source =
        \\{
        \\    x = []
        \\    _y = [1, 2]
        \\    _z = []
        \\    match x { [] => 42, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "inspect: basic nested blocks with lists",
        .source =
        \\{
        \\    outer = [1, 2, 3]
        \\    result = {
        \\        inner = outer
        \\        match inner { [a, b, c] => a + b + c, _ => 0 }
        \\    }
        \\    result
        \\}
        ,
        .expected = .{ .inspect_str = "6.0" },
    },
    .{
        .name = "inspect: basic list created and used in inner block",
        .source =
        \\{
        \\    result = {
        \\        lst = [10, 20, 30]
        \\        match lst { [a, b, c] => a + b + c, _ => 0 }
        \\    }
        \\    result
        \\}
        ,
        .expected = .{ .inspect_str = "60.0" },
    },
    .{
        .name = "inspect: basic multiple lists chained through aliases",
        .source =
        \\{
        \\    a = [1]
        \\    b = a
        \\    c = [2, 3]
        \\    d = c
        \\    x = match b { [v] => v, _ => 0 }
        \\    y = match d { [v1, v2] => v1 + v2, _ => 0 }
        \\    x + y
        \\}
        ,
        .expected = .{ .inspect_str = "6.0" },
    },

    // Ported from list_refcount_conditional.zig
    .{
        .name = "inspect: conditional chooses list from then branch",
        .source =
        \\{
        \\    x = [1, 2]
        \\    result = if True {x} else {[3, 4]}
        \\    match result { [a, b] => a + b, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "3.0" },
    },
    .{
        .name = "inspect: conditional chooses list from else branch",
        .source =
        \\{
        \\    x = [1, 2]
        \\    result = if False {x} else {[3, 4]}
        \\    match result { [a, b] => a + b, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "7.0" },
    },
    .{
        .name = "inspect: conditional reuses same list in both branches",
        .source =
        \\{
        \\    x = [1, 2]
        \\    result = if True {x} else {x}
        \\    match result { [a, b] => a + b, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "3.0" },
    },
    .{
        .name = "inspect: conditional drops unused branch list",
        .source =
        \\{
        \\    x = [1, 2]
        \\    y = [3, 4]
        \\    result = if True {x} else {y}
        \\    match result { [a, b] => a + b, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "3.0" },
    },
    .{
        .name = "inspect: nested conditional list result",
        .source =
        \\{
        \\    x = [1]
        \\    result = if True {if False {x} else {[2]}} else {[3]}
        \\    match result { [a] => a, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "2.0" },
    },
    .{
        .name = "inspect: conditional string list result",
        .source =
        \\{
        \\    x = ["a", "b"]
        \\    result = if True {x} else {["c"]}
        \\    match result { [first, ..] => first, _ => "" }
        \\}
        ,
        .expected = .{ .inspect_str = "\"a\"" },
    },
    .{
        .name = "inspect: conditional inline list literals",
        .source =
        \\{
        \\    result = if True {[10, 20]} else {[30, 40]}
        \\    match result { [a, b] => a + b, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "30.0" },
    },
    .{
        .name = "inspect: conditional empty list branch",
        .source =
        \\{
        \\    result = if True {[]} else {[1, 2]}
        \\    match result { [] => 42, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "42.0" },
    },

    // Ported from list_refcount_strings.zig
    .{
        .name = "inspect: string list single captured string",
        .source =
        \\{
        \\    x = "hi"
        \\    lst = [x]
        \\    match lst { [s] => s, _ => "" }
        \\}
        ,
        .expected = .{ .inspect_str = "\"hi\"" },
    },
    .{
        .name = "inspect: string list multiple captured strings",
        .source =
        \\{
        \\    x = "a"
        \\    y = "b"
        \\    lst = [x, y]
        \\    match lst { [first, ..] => first, _ => "" }
        \\}
        ,
        .expected = .{ .inspect_str = "\"a\"" },
    },
    .{
        .name = "inspect: string list return second string",
        .source =
        \\{
        \\    x = "a"
        \\    y = "b"
        \\    lst = [x, y]
        \\    match lst { [_, second] => second, _ => "" }
        \\}
        ,
        .expected = .{ .inspect_str = "\"b\"" },
    },
    .{
        .name = "inspect: string list same string multiple times",
        .source =
        \\{
        \\    x = "hi"
        \\    lst = [x, x, x]
        \\    match lst { [first, ..] => first, _ => "" }
        \\}
        ,
        .expected = .{ .inspect_str = "\"hi\"" },
    },
    .{
        .name = "inspect: string list empty string",
        .source =
        \\{
        \\    x = ""
        \\    lst = [x]
        \\    match lst { [s] => s, _ => "fallback" }
        \\}
        ,
        .expected = .{ .inspect_str = "\"\"" },
    },
    .{
        .name = "inspect: string list small and large strings",
        .source =
        \\{
        \\    small = "hi"
        \\    large = "This is a very long string that will be heap allocated for sure"
        \\    lst = [small, large]
        \\    match lst { [first, ..] => first, _ => "" }
        \\}
        ,
        .expected = .{ .inspect_str = "\"hi\"" },
    },
    .{
        .name = "inspect: string list return large string",
        .source =
        \\{
        \\    small = "hi"
        \\    large = "This is a very long string that will be heap allocated for sure"
        \\    lst = [small, large]
        \\    match lst { [_, second] => second, _ => "" }
        \\}
        ,
        .expected = .{ .inspect_str = "\"This is a very long string that will be heap allocated for sure\"" },
    },
    .{
        .name = "inspect: string list literal head",
        .source = "match [\"a\", \"b\", \"c\"] { [first, ..] => first, _ => \"\" }",
        .expected = .{ .inspect_str = "\"a\"" },
    },
    .{
        .name = "inspect: string list literal second element",
        .source = "match [\"a\", \"b\", \"c\"] { [_, second, ..] => second, _ => \"\" }",
        .expected = .{ .inspect_str = "\"b\"" },
    },
    .{
        .name = "inspect: empty list then string list",
        .source =
        \\{
        \\    _empty = []
        \\    strings = ["x", "y"]
        \\    match strings { [first, ..] => first, _ => "" }
        \\}
        ,
        .expected = .{ .inspect_str = "\"x\"" },
    },
    .{
        .name = "inspect: aliased string list",
        .source =
        \\{
        \\    lst1 = ["a", "b"]
        \\    lst2 = lst1
        \\    match lst2 { [first, ..] => first, _ => "" }
        \\}
        ,
        .expected = .{ .inspect_str = "\"a\"" },
    },
    .{
        .name = "inspect: aliased string list returns original",
        .source =
        \\{
        \\    lst1 = ["a", "b"]
        \\    _lst2 = lst1
        \\    match lst1 { [first, ..] => first, _ => "" }
        \\}
        ,
        .expected = .{ .inspect_str = "\"a\"" },
    },
    .{
        .name = "inspect: mutable string list reassigned",
        .source =
        \\{
        \\    var $lst = ["old1", "old2"]
        \\    $lst = ["new1", "new2"]
        \\    match $lst { [first, ..] => first, _ => "" }
        \\}
        ,
        .expected = .{ .inspect_str = "\"new1\"" },
    },
    .{
        .name = "inspect: three string lists chooses middle",
        .source =
        \\{
        \\    _a = ["a1", "a2"]
        \\    b = ["b1", "b2"]
        \\    _c = ["c1", "c2"]
        \\    match b { [first, ..] => first, _ => "" }
        \\}
        ,
        .expected = .{ .inspect_str = "\"b1\"" },
    },
    .{
        .name = "inspect: extract string from nested match",
        .source =
        \\{
        \\    lst = ["x", "y", "z"]
        \\    match lst {
        \\        [_first, .. as rest] => match rest {
        \\            [second, ..] => second,
        \\            _ => ""
        \\        },
        \\        _ => ""
        \\    }
        \\}
        ,
        .expected = .{ .inspect_str = "\"y\"" },
    },

    // Ported from list_refcount_complex.zig
    .{
        .name = "inspect: list of records with strings",
        .source =
        \\{
        \\    r1 = {s: "a"}
        \\    r2 = {s: "b"}
        \\    lst = [r1, r2]
        \\    match lst { [first, ..] => first.s, _ => "" }
        \\}
        ,
        .expected = .{ .inspect_str = "\"a\"" },
    },
    .{
        .name = "inspect: list of records with integers",
        .source =
        \\{
        \\    r1 = {val: 10}
        \\    r2 = {val: 20}
        \\    lst = [r1, r2]
        \\    match lst { [first, ..] => first.val, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "10.0" },
    },
    .{
        .name = "inspect: same record multiple times in list",
        .source =
        \\{
        \\    r = {val: 42}
        \\    lst = [r, r, r]
        \\    match lst { [first, ..] => first.val, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "inspect: list of records with nested data",
        .source =
        \\{
        \\    r1 = {inner: {val: 10}}
        \\    r2 = {inner: {val: 20}}
        \\    lst = [r1, r2]
        \\    match lst { [first, ..] => first.inner.val, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "10.0" },
    },
    .{
        .name = "inspect: list of tuples with integers",
        .source =
        \\{
        \\    t1 = (1, 2)
        \\    t2 = (3, 4)
        \\    lst = [t1, t2]
        \\    match lst { [first, ..] => match first { (a, b) => a + b }, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "3.0" },
    },
    .{
        .name = "inspect: list of tuples with strings",
        .source =
        \\{
        \\    t1 = ("a", "b")
        \\    t2 = ("c", "d")
        \\    lst = [t1, t2]
        \\    match lst { [first, ..] => match first { (s, _) => s }, _ => "" }
        \\}
        ,
        .expected = .{ .inspect_str = "\"a\"" },
    },
    .{
        .name = "inspect: tag containing list of integers",
        .source = "match Some([10, 20]) { Some(lst) => match lst { [x, ..] => x, _ => 0 }, None => 0 }",
        .expected = .{ .inspect_str = "10.0" },
    },
    .{
        .name = "inspect: tag containing list of strings",
        .source = "match Some([\"hello\", \"world\"]) { Some(lst) => match lst { [s, ..] => s, _ => \"\" }, None => \"\" }",
        .expected = .{ .inspect_str = "\"hello\"" },
    },
    .{
        .name = "inspect: list of records of lists of strings",
        .source =
        \\{
        \\    r1 = {items: ["a", "b"]}
        \\    r2 = {items: ["c", "d"]}
        \\    lst = [r1, r2]
        \\    match lst { [first, ..] => match first.items { [s, ..] => s, _ => "" }, _ => "" }
        \\}
        ,
        .expected = .{ .inspect_str = "\"a\"" },
    },
    .{
        .name = "inspect: inline complex structure list",
        .source =
        \\{
        \\    data = [{val: 1}, {val: 2}]
        \\    match data { [first, ..] => first.val, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "1.0" },
    },
    .{
        .name = "inspect: deeply nested mixed structures list",
        .source =
        \\{
        \\    inner = {x: 42}
        \\    outer = {nested: inner}
        \\    lst = [outer]
        \\    match lst { [first, ..] => first.nested.x, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "inspect: list of Ok Err tags through payload match",
        .source = "match Ok([1, 2]) { Ok(lst) => match lst { [x, ..] => x, _ => 0 }, Err(_) => 0 }",
        .expected = .{ .inspect_str = "1.0" },
    },
};
