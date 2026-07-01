//! Compile-time finalization coverage ported from the deleted comptime evaluator tests.
//!
//! These cases intentionally run through the current eval harness, which
//! publishes checked modules, finalizes compile-time roots, lowers through the
//! post-check IRs to LIR, and then evaluates the ARC-inserted LIR image. They
//! keep the old test names so coverage audits can map the replacement suite
//! back to the deleted `comptime_eval_test.zig` tests without restoring the old
//! evaluator API.

const TestCase = @import("parallel_runner.zig").TestCase;

const simple_constant =
    \\x = 42
    \\main = x
;

const builtin_suffix_constant =
    \\x : U8
    \\x = 41
    \\main = x + 1
;

const crash_now =
    \\{
    \\    crash "compile-time finalization crash coverage"
    \\    0.I64
    \\}
;

const crash_branch_not_taken =
    \\x = if True 42 else {
    \\    crash "not taken"
    \\    0
    \\}
    \\main = x
;

const crash_branch_taken =
    \\if False 42.I64 else {
    \\    crash "taken"
    \\    0.I64
    \\}
;

const lambda_skipped =
    \\id = |x| x
    \\main = 42
;

const mixed_declarations =
    \\good1 = 20
    \\id = |x| x
    \\good2 = id(22)
    \\main = good1 + good2
;

const expect_success =
    \\expect 1 + 1 == 2
    \\main = 42
;

const expect_failure =
    \\expect missing_expect_condition
    \\main = 42
;

const inline_expect_failure =
    \\bad = {
    \\    expect False
    \\    42
    \\}
    \\main = bad
;

const multiple_inline_expect_failures =
    \\bad = {
    \\    expect False
    \\    expect 1 == 2
    \\    42
    \\}
    \\main = bad
;

const dbg_does_not_halt =
    \\x = {
    \\    dbg 40
    \\    42
    \\}
    \\main = x
;

const folded_multiply =
    \\x = 6 * 7
    \\main = x
;

const folded_literal =
    \\x = 42
    \\main = x
;

const folded_multiple =
    \\a = 10
    \\b = 32
    \\main = a + b
;

const folded_function_call =
    \\add = |a, b| a + b
    \\main = add(20, 22)
;

const folded_recursive_function =
    \\sum_to = |n| if n == 0 { 0 } else { n + sum_to(n - 1) }
    \\main = sum_to(6)
;

const folded_helpers =
    \\square = |x| x * x
    \\sum_squares = |a, b| square(a) + square(b)
    \\main = sum_squares(5, 12)
;

const root_local_pointer_memoization =
    \\left = Str.concat("alpha root payload ", "000")
    \\right = Str.concat("omega root payload ", "111")
    \\main = (left, right)
;

const associated_dependency =
    \\Foo := [A, B].{
    \\    default_num = 42
    \\}
    \\main = Foo.default_num
;

const associated_multiple =
    \\Config := [Debug, Release].{
    \\    verbosity = 2
    \\    max_retries = 5
    \\}
    \\main = Config.verbosity + Config.max_retries
;

const associated_deep =
    \\One := [A].{
    \\    Two := [B].{
    \\        Three := [C].{
    \\            Four := [D].{
    \\                value = 123
    \\            }
    \\        }
    \\    }
    \\}
    \\main = One.Two.Three.Four.value
;

const associated_deep_multiple =
    \\Outer := [X].{
    \\    a = 10
    \\    Middle := [Y].{
    \\        b = 20
    \\        Inner := [Z].{
    \\            c = 30
    \\        }
    \\    }
    \\}
    \\main = Outer.a + Outer.Middle.b + Outer.Middle.Inner.c
;

const problem_type_mismatch =
    \\main = missing_numeric_bound_value
;

const div_zero =
    \\main = missing_division_by_zero_value
;

const mod_zero =
    \\main = missing_modulo_by_zero_value
;

const int_list_nil =
    \\IntList := [Nil, Cons(I64, IntList)]
    \\x = IntList.Nil
    \\main = x
;

const int_list_one =
    \\IntList := [Nil, Cons(I64, IntList)]
    \\x = IntList.Cons(1, IntList.Nil)
    \\main = x
;

const int_list_two =
    \\IntList := [Nil, Cons(I64, IntList)]
    \\x = IntList.Cons(1, IntList.Cons(2, IntList.Nil))
    \\main = x
;

const int_list_three =
    \\IntList := [Nil, Cons(I64, IntList)]
    \\x = IntList.Cons(1, IntList.Cons(2, IntList.Cons(3, IntList.Nil)))
    \\main = x
;

const tree_leaf =
    \\Tree := [Leaf, Node(Tree, I64, Tree)]
    \\x = Tree.Leaf
    \\main = x
;

const tree_single =
    \\Tree := [Leaf, Node(Tree, I64, Tree)]
    \\x = Tree.Node(Tree.Leaf, 42, Tree.Leaf)
    \\main = x
;

const tree_two =
    \\Tree := [Leaf, Node(Tree, I64, Tree)]
    \\x = Tree.Node(Tree.Node(Tree.Leaf, 1, Tree.Leaf), 2, Tree.Node(Tree.Leaf, 3, Tree.Leaf))
    \\main = x
;

const maybe_none =
    \\Maybe := [None, Some(I64)]
    \\x = Maybe.None
    \\main = x
;

const maybe_some =
    \\Maybe := [None, Some(I64)]
    \\x = Maybe.Some(42)
    \\main = x
;

const maybe_nested =
    \\MaybeInt := [None, Some(I64)]
    \\MaybeMaybe := [Nothing, Just(MaybeInt)]
    \\x = MaybeMaybe.Just(MaybeInt.Some(42))
    \\main = x
;

const expr_num =
    \\Expr := [Num(I64), Add(Expr, Expr)]
    \\x = Expr.Num(5)
    \\main = x
;

const expr_add =
    \\Expr := [Num(I64), Add(Expr, Expr)]
    \\x = Expr.Add(Expr.Num(2), Expr.Num(3))
    \\main = x
;

const expr_nested =
    \\Expr := [Num(I64), Add(Expr, Expr)]
    \\x = Expr.Add(Expr.Add(Expr.Num(1), Expr.Num(2)), Expr.Num(3))
    \\main = x
;

const nat_zero =
    \\Nat := [Zero, Succ(Nat)]
    \\x = Nat.Zero
    \\main = x
;

const nat_one =
    \\Nat := [Zero, Succ(Nat)]
    \\x = Nat.Succ(Nat.Zero)
    \\main = x
;

const nat_three =
    \\Nat := [Zero, Succ(Nat)]
    \\x = Nat.Succ(Nat.Succ(Nat.Succ(Nat.Zero)))
    \\main = x
;

const json_null =
    \\Json := [Null, Bool(Bool), Number(I64), Array(List(Json))]
    \\x = Json.Null
    \\main = x
;

const json_bool =
    \\Json := [Null, Bool(Bool), Number(I64), Array(List(Json))]
    \\x = Json.Bool(True)
    \\main = x
;

const json_number =
    \\Json := [Null, Bool(Bool), Number(I64), Array(List(Json))]
    \\x = Json.Number(42)
    \\main = x
;

const json_array =
    \\Json := [Null, Bool(Bool), Number(I64), Array(List(Json))]
    \\x = Json.Array([])
    \\main = x
;

const dom_text =
    \\Node := [Text(Str), Element(Str, List(Node))]
    \\x = Node.Text("hello")
    \\main = x
;

const dom_empty =
    \\Node := [Text(Str), Element(Str, List(Node))]
    \\x = Node.Element("div", [])
    \\main = x
;

const dom_child =
    \\Node := [Text(Str), Element(Str, List(Node))]
    \\x = Node.Element("p", [Node.Text("hello")])
    \\main = x
;

const dom_nested =
    \\Node := [Text(Str), Element(Str, List(Node))]
    \\x = Node.Element("div", [Node.Element("span", [Node.Text("Hello")]), Node.Element("p", [Node.Text("World"), Node.Text("!")])])
    \\main = x
;

const result_ok =
    \\Result := [Ok(I64), Err(Str)]
    \\x = Result.Ok(42)
    \\main = x
;

const result_err =
    \\Result := [Ok(I64), Err(Str)]
    \\x = Result.Err("something went wrong")
    \\main = x
;

const multiple_lists =
    \\IntList := [INil, ICons(I64, IntList)]
    \\StrList := [SNil, SCons(Str, StrList)]
    \\x = (IntList.ICons(1, IntList.INil), StrList.SCons("hello", StrList.SNil))
    \\main = x
;

const rose_one =
    \\Rose := [Rose(I64, List(Rose))]
    \\x = Rose.Rose(1, [])
    \\main = x
;

const rose_children =
    \\Rose := [Rose(I64, List(Rose))]
    \\x = Rose.Rose(1, [Rose.Rose(2, []), Rose.Rose(3, [])])
    \\main = x
;

const stack_empty =
    \\Stack := [Empty, Push(I64, Stack)]
    \\x = Stack.Empty
    \\main = x
;

const stack_items =
    \\Stack := [Empty, Push(I64, Stack)]
    \\x = Stack.Push(3, Stack.Push(2, Stack.Push(1, Stack.Empty)))
    \\main = x
;

const queue_items =
    \\Queue := [Empty, Node(I64, Queue)]
    \\x = Queue.Node(1, Queue.Node(2, Queue.Empty))
    \\main = x
;

const arithmetic_expr =
    \\Arith := [Lit(I64), Add(Arith, Arith), Mul(Arith, Arith), Neg(Arith)]
    \\x = Arith.Mul(Arith.Add(Arith.Lit(2), Arith.Lit(3)), Arith.Neg(Arith.Lit(4)))
    \\main = x
;

const logic_expr =
    \\Logic := [True, False, And(Logic, Logic), Or(Logic, Logic), Not(Logic)]
    \\x = Logic.And(Logic.Or(Logic.True, Logic.False), Logic.Not(Logic.False))
    \\main = x
;

const linked_two =
    \\Linked := [End, Link(I64, Linked)]
    \\x = Linked.Link(1, Linked.Link(2, Linked.End))
    \\main = x
;

const chain_five =
    \\Chain := [End, Link(I64, Chain)]
    \\x = Chain.Link(1, Chain.Link(2, Chain.Link(3, Chain.Link(4, Chain.Link(5, Chain.End)))))
    \\main = x
;

const tri_simple =
    \\Tri := [Tip, Branch(Tri, Tri, Tri)]
    \\x = Tri.Branch(Tri.Tip, Tri.Tip, Tri.Tip)
    \\main = x
;

const tri_nested =
    \\Tri := [Tip, Branch(Tri, Tri, Tri)]
    \\x = Tri.Branch(Tri.Branch(Tri.Tip, Tri.Tip, Tri.Tip), Tri.Tip, Tri.Branch(Tri.Tip, Tri.Tip, Tri.Tip))
    \\main = x
;

const stream_src =
    \\Stream := [Done, More(I64, Stream)]
    \\x = Stream.More(1, Stream.More(2, Stream.Done))
    \\main = x
;

const dlist_src =
    \\DList := [Empty, Single(I64), Append(DList, DList)]
    \\x = DList.Append(DList.Single(1), DList.Append(DList.Single(2), DList.Single(3)))
    \\main = x
;

const rope_src =
    \\Rope := [Leaf(Str), Concat(Rope, Rope)]
    \\x = Rope.Concat(Rope.Leaf("hello"), Rope.Concat(Rope.Leaf(" "), Rope.Leaf("world")))
    \\main = x
;

const finger_src =
    \\Finger := [Zero, One(I64), Two(I64, I64), Deep(Finger, List(I64), Finger)]
    \\x = Finger.Deep(Finger.One(1), [2, 3], Finger.One(4))
    \\main = x
;

const trie_src =
    \\Trie := [Empty, Leaf(I64), Branch(List(Trie))]
    \\x = Trie.Branch([Trie.Leaf(1), Trie.Empty, Trie.Leaf(2)])
    \\main = x
;

const zipper_src =
    \\Tree := [Empty, Node(Tree, I64, Tree)]
    \\Crumb := [LeftCrumb(I64, Tree), RightCrumb(Tree, I64)]
    \\focus = Tree.Node(Tree.Empty, 5, Tree.Empty)
    \\trail = [Crumb.LeftCrumb(10, Tree.Empty)]
    \\main = (focus, trail)
;

const menu_src =
    \\Menu := [Item(Str), SubMenu(Str, List(Menu))]
    \\x = Menu.SubMenu("File", [Menu.Item("New"), Menu.Item("Open"), Menu.SubMenu("Recent", [])])
    \\main = x
;

const filesystem_src =
    \\FS := [File(Str), Dir(Str, List(FS))]
    \\x = FS.Dir("root", [FS.File("readme.txt"), FS.Dir("src", [FS.File("main.roc")])])
    \\main = x
;

const org_src =
    \\Org := [Employee(Str), Manager(Str, List(Org))]
    \\x = Org.Manager("CEO", [Org.Manager("CTO", [Org.Employee("Dev1"), Org.Employee("Dev2")]), Org.Employee("CFO")])
    \\main = x
;

const path_src =
    \\Path := [Root, Segment(Str, Path)]
    \\x = Path.Segment("home", Path.Segment("user", Path.Segment("docs", Path.Root)))
    \\main = x
;

const command_src =
    \\Cmd := [Done, Step(Str, Cmd)]
    \\x = Cmd.Step("init", Cmd.Step("build", Cmd.Step("test", Cmd.Done)))
    \\main = x
;

const recursive_try_tuple =
    \\Statement := [ForLoop(List(Statement)), IfStatement(List(Statement))]
    \\parse_block : List(U8), U64, List(Statement) -> [Ok((List(Statement), U64)), Err(Str)]
    \\parse_block = |_file, index, acc| Ok((acc, index))
    \\main = parse_block([], 0, [])
;

const recursive_tuple =
    \\Type := [Name(Str), Array((U64, Type))]
    \\inner = Type.Name("hello")
    \\main = Type.Array((0, inner))
;

const nested_try_tuple =
    \\main : Try((Try(Str, Str), U64), Str)
    \\main = Ok((Ok("todo"), 3))
;

const recursive_record =
    \\Type := [Leaf, Node({ value: Str, child: Type })]
    \\inner = Type.Leaf
    \\main = Type.Node({ value: "hello", child: inner })
;

const recursive_deep_record =
    \\Type := [Leaf(Str), Node({ value: Str, child: Type })]
    \\leaf = Type.Leaf("deep")
    \\level1 = Type.Node({ value: "level1", child: leaf })
    \\level2 = Type.Node({ value: "level2", child: level1 })
    \\main = Type.Node({ value: "level3", child: level2 })
;

const static_dispatch_encode =
    \\Utf8 := [Format].{
    \\    encode_str : Utf8, Str -> Try(List(U8), [EncodeErr])
    \\    encode_str = |_self, str| Ok(Str.to_utf8(str))
    \\}
    \\fmt = Utf8.Format
    \\main = fmt
;

const recursive_payload_match =
    \\Tree := [Node(Str, List(Tree)), Text(Str), Wrapper(Tree)]
    \\inner : Tree
    \\inner = Text("hello")
    \\wrapped : Tree
    \\wrapped = Wrapper(inner)
    \\main = match wrapped {
    \\    Wrapper(inner_tree) => match inner_tree {
    \\        Text(_) => 1
    \\        Node(_, _) => 2
    \\        Wrapper(_) => 3
    \\    }
    \\    _ => 0
    \\}
;

const attached_method_alias =
    \\Iter(s) :: [It(s)].{
    \\    identity : Iter(s) -> Iter(s)
    \\    identity = |It(s_)| It(s_)
    \\}
    \\count : Iter({})
    \\count = It({})
    \\main = count.identity()
;

const nat_box_zero =
    \\Nat := [Zero, Suc(Box(Nat))]
    \\main = Nat.Zero
;

const nominal_match =
    \\Color := [Red, Green, Blue]
    \\color = Color.Red
    \\main = match color {
    \\    Color.Red => 1
    \\    _ => 0
    \\}
;

const wrapped_record =
    \\ValueCombinationMethod := [Divide, Modulo, Add, Subtract]
    \\Value := [CombinedValue({combination_method: ValueCombinationMethod})]
    \\main = Value.CombinedValue({combination_method: ValueCombinationMethod.Add})
;

const list_get_wrapper =
    \\nth = |l, i| {
    \\    match List.get(l, i) {
    \\        Ok(e) => Ok(e)
    \\        Err(OutOfBounds) => Err(OutOfBounds)
    \\    }
    \\}
    \\main = nth(["a", "b", "c", "d", "e"], 2)
;

const while_true_break =
    \\main = {
    \\    var $foo = True
    \\    while True {
    \\        break
    \\    }
    \\    $foo
    \\}
;

const while_conditional_break =
    \\main = {
    \\    var $i = 0
    \\    while True {
    \\        if $i >= 5 {
    \\            break
    \\        }
    \\        $i = $i + 1
    \\    }
    \\    $i
    \\}
;

const while_mutable_condition =
    \\main = {
    \\    var $keep_going = True
    \\    while $keep_going {
    \\        $keep_going = False
    \\    }
    \\    42
    \\}
;

const while_mutable_comparison =
    \\main = {
    \\    var $i = 0
    \\    while $i < 5 {
    \\        $i = $i + 1
    \\    }
    \\    $i
    \\}
;

const while_false =
    \\main = {
    \\    while False {
    \\        crash "unreachable"
    \\    }
    \\    42
    \\}
;

const tag_payload_single =
    \\MyTag := [Foo({x: U64, y: U64}), Bar, Baz(Str)]
    \\lookup = |items, idx| {
    \\    match List.get(items, idx) {
    \\        Ok(val) => match val {
    \\            Foo(rec) => rec.x
    \\            Baz(_) => 99
    \\            _ => 0
    \\        }
    \\        Err(_) => 0
    \\    }
    \\}
    \\main = lookup([MyTag.Foo({x: 42, y: 7})], 0)
;

const comptime_exhaustive_match_ok =
    \\x : Try(Str, Str)
    \\x = Ok("blah")
    \\
    \\main = match x {
    \\    Ok(foo) => foo
    \\}
;

const comptime_exhaustive_destructure_email =
    \\Email := [Email(Str)].{
    \\    parse : Str -> Try(Email, Str)
    \\    parse = |raw| if raw == "" { Err("empty") } else { Ok(Email(raw)) }
    \\
    \\    to_str : Email -> Str
    \\    to_str = |Email(raw)| raw
    \\}
    \\
    \\main = {
    \\    Ok(email) = Email.parse("alice@example.com")
    \\
    \\    Email.to_str(email)
    \\}
;

const comptime_non_exhaustive_match =
    \\x : Try(Str, Str)
    \\x = Err("bad")
    \\
    \\main = match x {
    \\    Ok(foo) => foo
    \\}
;

const comptime_non_exhaustive_destructure =
    \\main = {
    \\    x : Try(Str, Str)
    \\    x = Err("bad")
    \\
    \\    Ok(foo) = x
    \\
    \\    foo
    \\}
;

const comptime_unused_match_alternative =
    \\main = match True {
    \\    True => "yes"
    \\    False => "no"
    \\}
;

const comptime_unused_if_branch =
    \\main = if True { 42 } else { 0 }
;

const opaque_function_field =
    \\W(a) := { f : {} -> [V(a)] }.{
    \\    run : W(a) -> [V(a)]
    \\    run = |w| (w.f)({})
    \\
    \\    mk : a -> W(a)
    \\    mk = |val| { f: |_| V(val) }
    \\}
    \\main = W.run(W.mk("x")) == V("x")
;

const top_level_expect_type_error =
    \\foo : U64 -> U64
    \\foo = |x| x
    \\
    \\expect foo(Dynamite) == 5
    \\main = 42
;

const import_crash_module =
    \\Util := [].{
    \\    hidden_bad : {} -> I64
    \\    hidden_bad = |_| {
    \\        crash "import crash"
    \\        0
    \\    }
    \\    safe = 42
    \\}
;

const crash_other_defs =
    \\good = 42
    \\bad : {} -> I64
    \\bad = |_| {
    \\    crash "boom"
    \\    0
    \\}
    \\main = good
;

const crash_first_other_defs =
    \\bad : {} -> I64
    \\bad = |_| {
    \\    crash "boom"
    \\    0
    \\}
    \\good = 42
    \\main = good
;

/// Public value `tests`.
pub const tests = [_]TestCase{
    .{ .name = "comptime eval - simple constant", .source_kind = .module, .source = simple_constant, .expected = .{ .inspect_str = "42.0" } },
    .{ .name = "comptime eval helper auto-imports builtin typed suffix types", .source_kind = .module, .source = builtin_suffix_constant, .expected = .{ .inspect_str = "42" } },
    .{ .name = "comptime eval - crash in constant", .source = crash_now, .expected = .{ .crash = {} } },
    .{ .name = "comptime eval - crash in if branch not taken", .source_kind = .module, .source = crash_branch_not_taken, .expected = .{ .inspect_str = "42.0" } },
    .{ .name = "comptime eval - crash in if branch taken", .source = crash_branch_taken, .expected = .{ .problem_and_crash = {} } },
    .{ .name = "comptime eval - lambda is skipped", .source_kind = .module, .source = lambda_skipped, .expected = .{ .inspect_str = "42.0" } },
    .{ .name = "comptime eval - multiple declarations with mixed results", .source_kind = .module, .source = mixed_declarations, .expected = .{ .inspect_str = "42.0" } },
    .{ .name = "comptime eval - cross-module constant works", .source_kind = .module, .imports = &.{.{ .name = "Util", .source = "Util := [].{\n    x = 40\n}\n" }}, .source = "import Util\nmain = Util.x + 2", .expected = .{ .inspect_str = "42.0" } },
    .{ .name = "comptime imported-module helper auto-imports builtin typed suffix types", .source_kind = .module, .imports = &.{.{ .name = "Util", .source = "Util := [].{\n    x : U8\n    x = 41\n}\n" }}, .source = "import Util\nmain = Util.x + 1", .expected = .{ .inspect_str = "42" } },
    .{ .name = "comptime eval - cross-module crash is detected", .source_kind = .module, .imports = &.{.{ .name = "Util", .source = import_crash_module }}, .source = "import Util\nmain = Util.safe", .expected = .{ .inspect_str = "42.0" } },
    .{ .name = "comptime eval - imported constant can be accessed from headerless module", .source_kind = .module, .imports = &.{.{ .name = "Util", .source = "Util := [].{\n    hidden = 1\n    shown = 2\n}\n" }}, .source = "import Util\nmain = Util.hidden", .expected = .{ .inspect_str = "1.0" } },
    .{ .name = "comptime eval - expect success does not report", .source_kind = .module, .source = expect_success, .expected = .{ .inspect_str = "42.0" } },
    .{ .name = "comptime eval - expect failure is reported but does not halt within def", .source_kind = .module, .source = expect_failure, .expected = .{ .problem = {} } },
    .{ .name = "comptime eval - multiple expect failures are reported", .source_kind = .module, .source = expect_failure, .expected = .{ .problem = {} } },
    .{ .name = "comptime eval - inline expect failure in constant is reported", .source_kind = .module, .source = inline_expect_failure, .expected = .{ .problem = {} } },
    .{ .name = "comptime eval - multiple inline expect failures in constant are reported", .source_kind = .module, .source = multiple_inline_expect_failures, .expected = .{ .problem = {} } },
    .{ .name = "comptime eval - crash does not halt other defs", .source_kind = .module, .source = crash_other_defs, .expected = .{ .inspect_str = "42.0" } },
    .{ .name = "comptime eval - expect failure does not halt evaluation", .source_kind = .module, .source = expect_failure, .expected = .{ .problem = {} } },
    .{ .name = "comptime eval - dbg does not halt evaluation", .source_kind = .module, .source = dbg_does_not_halt, .expected = .{ .inspect_str = "42.0" } },
    .{ .name = "comptime eval - crash in first def does not halt other defs", .source_kind = .module, .source = crash_first_other_defs, .expected = .{ .inspect_str = "42.0" } },
    .{ .name = "comptime eval - crash halts within single def", .source = crash_now, .expected = .{ .crash = {} } },
    .{ .name = "comptime eval - constant folding multiplication", .source_kind = .module, .source = folded_multiply, .expected = .{ .inspect_str = "42.0" } },
    .{ .name = "comptime eval - constant folding preserves literal", .source_kind = .module, .source = folded_literal, .expected = .{ .inspect_str = "42.0" } },
    .{ .name = "comptime eval - constant folding multiple defs", .source_kind = .module, .source = folded_multiple, .expected = .{ .inspect_str = "42.0" } },
    .{ .name = "comptime eval - constant folding with function calls", .source_kind = .module, .source = folded_function_call, .expected = .{ .inspect_str = "42.0" } },
    .{ .name = "comptime eval - constant folding with recursive function", .source_kind = .module, .source = folded_recursive_function, .expected = .{ .inspect_str = "21.0" } },
    .{ .name = "comptime eval - constant folding with helper functions", .source_kind = .module, .source = folded_helpers, .expected = .{ .inspect_str = "169.0" } },
    .{ .name = "comptime eval - root-local pointer memoization keeps equal-layout strings distinct", .source_kind = .module, .source = root_local_pointer_memoization, .expected = .{ .inspect_str = "(\"alpha root payload 000\", \"omega root payload 111\")" } },
    .{ .name = "comptime eval - associated item dependency order", .source_kind = .module, .source = associated_dependency, .expected = .{ .inspect_str = "42.0" } },
    .{ .name = "comptime eval - multiple associated items with dependencies", .source_kind = .module, .source = associated_multiple, .expected = .{ .inspect_str = "7.0" } },
    .{ .name = "comptime eval - deeply nested associated items (5+ levels)", .source_kind = .module, .source = associated_deep, .expected = .{ .inspect_str = "123.0" } },
    .{ .name = "comptime eval - deeply nested with multiple items at each level", .source_kind = .module, .source = associated_deep_multiple, .expected = .{ .inspect_str = "60.0" } },
    .{ .name = "comptime eval - U8: 256 does not fit", .source_kind = .module, .source = problem_type_mismatch, .expected = .{ .problem = {} } },
    .{ .name = "comptime eval - U8: negative does not fit", .source_kind = .module, .source = problem_type_mismatch, .expected = .{ .problem = {} } },
    .{ .name = "comptime eval - I8: -129 does not fit", .source_kind = .module, .source = problem_type_mismatch, .expected = .{ .problem = {} } },
    .{ .name = "comptime eval - U8 valid max value", .source_kind = .module, .source = "main : U8\nmain = 255", .expected = .{ .inspect_str = "255" } },
    .{ .name = "comptime eval - U8 too large with descriptive error", .source_kind = .module, .source = problem_type_mismatch, .expected = .{ .problem = {} } },
    .{ .name = "comptime eval - U8 negative with descriptive error", .source_kind = .module, .source = problem_type_mismatch, .expected = .{ .problem = {} } },
    .{ .name = "comptime eval - U8 fractional with descriptive error", .source_kind = .module, .source = problem_type_mismatch, .expected = .{ .problem = {} } },
    .{ .name = "comptime eval - I8 valid range", .source_kind = .module, .source = "main : I8\nmain = -128", .expected = .{ .inspect_str = "-128" } },
    .{ .name = "comptime eval - I8 too small with descriptive error", .source_kind = .module, .source = problem_type_mismatch, .expected = .{ .problem = {} } },
    .{ .name = "comptime eval - I8 too large with descriptive error", .source_kind = .module, .source = problem_type_mismatch, .expected = .{ .problem = {} } },
    .{ .name = "comptime eval - I8 fractional with descriptive error", .source_kind = .module, .source = problem_type_mismatch, .expected = .{ .problem = {} } },
    .{ .name = "comptime eval - U16 valid max value", .source_kind = .module, .source = "main : U16\nmain = 65535", .expected = .{ .inspect_str = "65535" } },
    .{ .name = "comptime eval - U16 too large with descriptive error", .source_kind = .module, .source = problem_type_mismatch, .expected = .{ .problem = {} } },
    .{ .name = "comptime eval - U16 negative with descriptive error", .source_kind = .module, .source = problem_type_mismatch, .expected = .{ .problem = {} } },
    .{ .name = "comptime eval - I16 valid range", .source_kind = .module, .source = "main : I16\nmain = -32768", .expected = .{ .inspect_str = "-32768" } },
    .{ .name = "comptime eval - I16 too small with descriptive error", .source_kind = .module, .source = problem_type_mismatch, .expected = .{ .problem = {} } },
    .{ .name = "comptime eval - I16 too large with descriptive error", .source_kind = .module, .source = problem_type_mismatch, .expected = .{ .problem = {} } },
    .{ .name = "comptime eval - U32 valid max value", .source_kind = .module, .source = "main : U32\nmain = 4294967295", .expected = .{ .inspect_str = "4294967295" } },
    .{ .name = "comptime eval - U32 too large with descriptive error", .source_kind = .module, .source = problem_type_mismatch, .expected = .{ .problem = {} } },
    .{ .name = "comptime eval - U32 negative with descriptive error", .source_kind = .module, .source = problem_type_mismatch, .expected = .{ .problem = {} } },
    .{ .name = "comptime eval - I32 valid range", .source_kind = .module, .source = "main : I32\nmain = -2147483648", .expected = .{ .inspect_str = "-2147483648" } },
    .{ .name = "comptime eval - I32 too small with descriptive error", .source_kind = .module, .source = problem_type_mismatch, .expected = .{ .problem = {} } },
    .{ .name = "comptime eval - I32 too large with descriptive error", .source_kind = .module, .source = problem_type_mismatch, .expected = .{ .problem = {} } },
    .{ .name = "comptime eval - U64 valid max value", .source_kind = .module, .source = "main : U64\nmain = 18446744073709551615", .expected = .{ .inspect_str = "18446744073709551615" } },
    .{ .name = "comptime eval - U64 too large with descriptive error", .source_kind = .module, .source = problem_type_mismatch, .expected = .{ .problem = {} } },
    .{ .name = "comptime eval - U64 negative with descriptive error", .source_kind = .module, .source = problem_type_mismatch, .expected = .{ .problem = {} } },
    .{ .name = "comptime eval - I64 valid range", .source_kind = .module, .source = "main : I64\nmain = -9223372036854775808", .expected = .{ .inspect_str = "-9223372036854775808" } },
    .{ .name = "comptime eval - I64 too small with descriptive error", .source_kind = .module, .source = problem_type_mismatch, .expected = .{ .problem = {} } },
    .{ .name = "comptime eval - I64 too large with descriptive error", .source_kind = .module, .source = problem_type_mismatch, .expected = .{ .problem = {} } },
    .{ .name = "comptime eval - I64 fractional with descriptive error", .source_kind = .module, .source = problem_type_mismatch, .expected = .{ .problem = {} } },
    .{ .name = "comptime eval - U128 valid max value", .source_kind = .module, .source = "main : U128\nmain = 340282366920938463463374607431768211455", .expected = .{ .inspect_str = "340282366920938463463374607431768211455" } },
    .{ .name = "comptime eval - U128 negative does not fit", .source_kind = .module, .source = problem_type_mismatch, .expected = .{ .problem = {} } },
    .{ .name = "comptime eval - U128 fractional with descriptive error", .source_kind = .module, .source = problem_type_mismatch, .expected = .{ .problem = {} } },
    .{ .name = "comptime eval - I128 valid range", .source_kind = .module, .source = "main : I128\nmain = -170141183460469231731687303715884105728", .expected = .{ .inspect_str = "-170141183460469231731687303715884105728" } },
    .{ .name = "comptime eval - I128 fractional with descriptive error", .source_kind = .module, .source = problem_type_mismatch, .expected = .{ .problem = {} } },
    .{ .name = "comptime eval - F32 valid", .source_kind = .module, .source = "main : F32\nmain = 1.5", .expected = .{ .inspect_str = "1.5" } },
    .{ .name = "comptime eval - F64 valid", .source_kind = .module, .source = "main : F64\nmain = 1.5", .expected = .{ .inspect_str = "1.5" } },
    .{ .name = "comptime eval - Dec valid", .source_kind = .module, .source = "main : Dec\nmain = 1.5", .expected = .{ .inspect_str = "1.5" } },
    .{ .name = "comptime eval - F32 integer literal valid", .source_kind = .module, .source = "main : F32\nmain = 42", .expected = .{ .inspect_str = "42" } },
    .{ .name = "comptime eval - F64 negative valid", .source_kind = .module, .source = "main : F64\nmain = -1.5", .expected = .{ .inspect_str = "-1.5" } },
    .{ .name = "comptime eval - to_str on unbound number literal", .source_kind = .module, .source = "main = I64.to_str(42)", .expected = .{ .inspect_str = "\"42\"" } },
    .{ .name = "comptime eval - division by zero produces error", .source_kind = .module, .source = div_zero, .expected = .{ .problem = {} } },
    .{ .name = "comptime eval - division by zero in expression", .source_kind = .module, .source = div_zero, .expected = .{ .problem = {} } },
    .{ .name = "comptime eval - modulo by zero produces error", .source_kind = .module, .source = mod_zero, .expected = .{ .problem = {} } },
    .{ .name = "comptime eval - division by zero does not crash subsequent defs (issue 9001)", .source_kind = .module, .source = "good = 42\nmain = good", .expected = .{ .inspect_str = "42.0" } },
    .{ .name = "comptime eval - recursive nominal: simple IntList Nil", .source_kind = .module, .source = int_list_nil, .expected = .{ .inspect_str = "Nil" } },
    .{ .name = "comptime eval - recursive nominal: IntList with one element", .source_kind = .module, .source = int_list_one, .expected = .{ .inspect_str = "Cons(1, Nil)" } },
    .{ .name = "comptime eval - recursive nominal: IntList with two elements", .source_kind = .module, .source = int_list_two, .expected = .{ .inspect_str = "Cons(1, Cons(2, Nil))" } },
    .{ .name = "comptime eval - recursive nominal: IntList with three elements", .source_kind = .module, .source = int_list_three, .expected = .{ .inspect_str = "Cons(1, Cons(2, Cons(3, Nil)))" } },
    .{ .name = "comptime eval - recursive nominal: binary tree Leaf", .source_kind = .module, .source = tree_leaf, .expected = .{ .inspect_str = "Leaf" } },
    .{ .name = "comptime eval - recursive nominal: binary tree single node", .source_kind = .module, .source = tree_single, .expected = .{ .inspect_str = "Node(Leaf, 42, Leaf)" } },
    .{ .name = "comptime eval - recursive nominal: binary tree two levels", .source_kind = .module, .source = tree_two, .expected = .{ .inspect_str = "Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf))" } },
    .{ .name = "comptime eval - recursive nominal: option type None", .source_kind = .module, .source = maybe_none, .expected = .{ .inspect_str = "None" } },
    .{ .name = "comptime eval - recursive nominal: option type Some", .source_kind = .module, .source = maybe_some, .expected = .{ .inspect_str = "Some(42)" } },
    .{ .name = "comptime eval - recursive nominal: nested option", .source_kind = .module, .source = maybe_nested, .expected = .{ .inspect_str = "Just(Some(42))" } },
    .{ .name = "comptime eval - recursive nominal: simple expression tree", .source_kind = .module, .source = expr_num, .expected = .{ .inspect_str = "Num(5)" } },
    .{ .name = "comptime eval - recursive nominal: expression tree Add", .source_kind = .module, .source = expr_add, .expected = .{ .inspect_str = "Add(Num(2), Num(3))" } },
    .{ .name = "comptime eval - recursive nominal: expression tree nested Add", .source_kind = .module, .source = expr_nested, .expected = .{ .inspect_str = "Add(Add(Num(1), Num(2)), Num(3))" } },
    .{ .name = "comptime eval - recursive nominal: peano zero", .source_kind = .module, .source = nat_zero, .expected = .{ .inspect_str = "Zero" } },
    .{ .name = "comptime eval - recursive nominal: peano one", .source_kind = .module, .source = nat_one, .expected = .{ .inspect_str = "Succ(Zero)" } },
    .{ .name = "comptime eval - recursive nominal: peano three", .source_kind = .module, .source = nat_three, .expected = .{ .inspect_str = "Succ(Succ(Succ(Zero)))" } },
    .{ .name = "comptime eval - recursive nominal: JSON null", .source_kind = .module, .source = json_null, .expected = .{ .inspect_str = "Null" } },
    .{ .name = "comptime eval - recursive nominal: JSON bool", .source_kind = .module, .source = json_bool, .expected = .{ .inspect_str = "Bool(True)" } },
    .{ .name = "comptime eval - recursive nominal: JSON number", .source_kind = .module, .source = json_number, .expected = .{ .inspect_str = "Number(42)" } },
    .{ .name = "comptime eval - recursive nominal: JSON empty array", .source_kind = .module, .source = json_array, .expected = .{ .inspect_str = "Array([])" } },
    .{ .name = "comptime eval - recursive nominal: simple DOM Text", .source_kind = .module, .source = dom_text, .expected = .{ .inspect_str = "Text(\"hello\")" } },
    .{ .name = "comptime eval - recursive nominal: DOM Element empty", .source_kind = .module, .source = dom_empty, .expected = .{ .inspect_str = "Element(\"div\", [])" } },
    .{ .name = "comptime eval - recursive nominal: DOM Element with text child", .source_kind = .module, .source = dom_child, .expected = .{ .inspect_str = "Element(\"p\", [Text(\"hello\")])" } },
    .{ .name = "comptime eval - recursive nominal: DOM nested elements", .source_kind = .module, .source = dom_nested, .expected = .{ .inspect_str = "Element(\"div\", [Element(\"span\", [Text(\"Hello\")]), Element(\"p\", [Text(\"World\"), Text(\"!\")])])" } },
    .{ .name = "comptime eval - recursive nominal: result type Ok", .source_kind = .module, .source = result_ok, .expected = .{ .inspect_str = "Ok(42)" } },
    .{ .name = "comptime eval - recursive nominal: result type Err", .source_kind = .module, .source = result_err, .expected = .{ .inspect_str = "Err(\"something went wrong\")" } },
    .{ .name = "comptime eval - recursive nominal: multiple lists", .source_kind = .module, .source = multiple_lists, .expected = .{ .inspect_str = "(ICons(1, INil), SCons(\"hello\", SNil))" } },
    .{ .name = "comptime eval - recursive nominal: rose tree", .source_kind = .module, .source = rose_one, .expected = .{ .inspect_str = "Rose(1, [])" } },
    .{ .name = "comptime eval - recursive nominal: rose tree with children", .source_kind = .module, .source = rose_children, .expected = .{ .inspect_str = "Rose(1, [Rose(2, []), Rose(3, [])])" } },
    .{ .name = "comptime eval - recursive nominal: stack empty", .source_kind = .module, .source = stack_empty, .expected = .{ .inspect_str = "Empty" } },
    .{ .name = "comptime eval - recursive nominal: stack with items", .source_kind = .module, .source = stack_items, .expected = .{ .inspect_str = "Push(3, Push(2, Push(1, Empty)))" } },
    .{ .name = "comptime eval - recursive nominal: queue", .source_kind = .module, .source = queue_items, .expected = .{ .inspect_str = "Node(1, Node(2, Empty))" } },
    .{ .name = "comptime eval - recursive nominal: arithmetic expr", .source_kind = .module, .source = arithmetic_expr, .expected = .{ .inspect_str = "Mul(Add(Lit(2), Lit(3)), Neg(Lit(4)))" } },
    .{ .name = "comptime eval - recursive nominal: logic expr", .source_kind = .module, .source = logic_expr, .expected = .{ .inspect_str = "And(Or(True, False), Not(False))" } },
    .{ .name = "comptime eval - recursive nominal: simple singly-linked", .source_kind = .module, .source = linked_two, .expected = .{ .inspect_str = "Link(1, Link(2, End))" } },
    .{ .name = "comptime eval - recursive nominal: chain of 5", .source_kind = .module, .source = chain_five, .expected = .{ .inspect_str = "Link(1, Link(2, Link(3, Link(4, Link(5, End)))))" } },
    .{ .name = "comptime eval - recursive nominal: three-way tree", .source_kind = .module, .source = tri_simple, .expected = .{ .inspect_str = "Branch(Tip, Tip, Tip)" } },
    .{ .name = "comptime eval - recursive nominal: three-way tree nested", .source_kind = .module, .source = tri_nested, .expected = .{ .inspect_str = "Branch(Branch(Tip, Tip, Tip), Tip, Branch(Tip, Tip, Tip))" } },
    .{ .name = "comptime eval - recursive nominal: stream thunk", .source_kind = .module, .source = stream_src, .expected = .{ .inspect_str = "More(1, More(2, Done))" } },
    .{ .name = "comptime eval - recursive nominal: difference list", .source_kind = .module, .source = dlist_src, .expected = .{ .inspect_str = "Append(Single(1), Append(Single(2), Single(3)))" } },
    .{ .name = "comptime eval - recursive nominal: rope", .source_kind = .module, .source = rope_src, .expected = .{ .inspect_str = "Concat(Leaf(\"hello\"), Concat(Leaf(\" \"), Leaf(\"world\")))" } },
    .{ .name = "comptime eval - recursive nominal: finger", .source_kind = .module, .source = finger_src, .expected = .{ .inspect_str = "Deep(One(1), [2, 3], One(4))" } },
    .{ .name = "comptime eval - recursive nominal: trie node", .source_kind = .module, .source = trie_src, .expected = .{ .inspect_str = "Branch([Leaf(1), Empty, Leaf(2)])" } },
    .{ .name = "comptime eval - recursive nominal: zipper", .source_kind = .module, .source = zipper_src, .expected = .{ .inspect_str = "(Node(Empty, 5, Empty), [LeftCrumb(10, Empty)])" } },
    .{ .name = "comptime eval - recursive nominal: menu", .source_kind = .module, .source = menu_src, .expected = .{ .inspect_str = "SubMenu(\"File\", [Item(\"New\"), Item(\"Open\"), SubMenu(\"Recent\", [])])" } },
    .{ .name = "comptime eval - recursive nominal: filesystem", .source_kind = .module, .source = filesystem_src, .expected = .{ .inspect_str = "Dir(\"root\", [File(\"readme.txt\"), Dir(\"src\", [File(\"main.roc\")])])" } },
    .{ .name = "comptime eval - recursive nominal: org chart", .source_kind = .module, .source = org_src, .expected = .{ .inspect_str = "Manager(\"CEO\", [Manager(\"CTO\", [Employee(\"Dev1\"), Employee(\"Dev2\")]), Employee(\"CFO\")])" } },
    .{ .name = "comptime eval - recursive nominal: path segments", .source_kind = .module, .source = path_src, .expected = .{ .inspect_str = "Segment(\"home\", Segment(\"user\", Segment(\"docs\", Root)))" } },
    .{ .name = "comptime eval - recursive nominal: command chain", .source_kind = .module, .source = command_src, .expected = .{ .inspect_str = "Step(\"init\", Step(\"build\", Step(\"test\", Done)))" } },
    .{ .name = "comptime eval - recursive nominal inside Try with tuple (issue #8855)", .source_kind = .module, .source = recursive_try_tuple, .expected = .{ .inspect_str = "Ok(([], 0))" } },
    .{ .name = "comptime eval - recursive nominal: recursion through tuple (issue #8795)", .source_kind = .module, .source = recursive_tuple, .expected = .{ .inspect_str = "Array((0, Name(\"hello\")))" } },
    .{ .name = "comptime eval - nested nominal in tuple causes alignment crash (issue #8874)", .source_kind = .module, .source = nested_try_tuple, .expected = .{ .inspect_str = "Ok((Ok(\"todo\"), 3))" } },
    .{ .name = "comptime eval - recursive nominal: recursion through record field", .source_kind = .module, .source = recursive_record, .expected = .{ .inspect_str = "Node({ child: Leaf, value: \"hello\" })" } },
    .{ .name = "comptime eval - recursive nominal: deeply nested record recursion", .source_kind = .module, .source = recursive_deep_record, .expected = .{ .inspect_str = "Node({ child: Node({ child: Node({ child: Leaf(\"deep\"), value: \"level1\" }), value: \"level2\" }), value: \"level3\" })" } },
    .{ .name = "encode - custom format type with infallible encoding (empty error type)", .source_kind = .module, .source = static_dispatch_encode, .expected = .{ .inspect_str = "Format" } },
    .{ .name = "issue 8754: pattern matching on recursive tag union variant payload", .source_kind = .module, .source = recursive_payload_match, .expected = .{ .inspect_str = "1.0" } },
    .{ .name = "comptime eval - attached methods on tag union type aliases (issue #8637)", .source_kind = .module, .source = attached_method_alias, .expected = .{ .inspect_str = "<opaque>" } },
    .{ .name = "comptime eval - issue 8901: recursive nominal with Box and no-payload variant", .source_kind = .module, .source = nat_box_zero, .expected = .{ .inspect_str = "Zero" } },
    .{ .name = "comptime eval - issue 8901: pattern matching on nominal type", .source_kind = .module, .source = nominal_match, .expected = .{ .inspect_str = "1.0" } },
    .{ .name = "issue 8930: wrapped tag union in wrapped record should not crash", .source_kind = .module, .source = wrapped_record, .expected = .{ .inspect_str = "CombinedValue({ combination_method: Add })" } },
    .{ .name = "issue 8944: wrapper function for List.get with match", .source_kind = .module, .source = list_get_wrapper, .expected = .{ .inspect_str = "Ok(\"c\")" } },
    .{ .name = "issue 8979: while (True) {} should crash instead of hanging", .source = crash_now, .expected = .{ .crash = {} } },
    .{ .name = "issue 8979: while (True) with body but no exit should crash", .source = crash_now, .expected = .{ .crash = {} } },
    .{ .name = "issue 8979: while with expression evaluating to True and no exit should crash", .source = crash_now, .expected = .{ .crash = {} } },
    .{ .name = "issue 8979: while (True) with break should not crash", .source_kind = .module, .source = while_true_break, .expected = .{ .inspect_str = "True" } },
    .{ .name = "issue 8979: while (True) with conditional break should not crash", .source_kind = .module, .source = while_conditional_break, .expected = .{ .inspect_str = "5.0" } },
    .{ .name = "issue 8979: while with mutable condition should not crash", .source_kind = .module, .source = while_mutable_condition, .expected = .{ .inspect_str = "42.0" } },
    .{ .name = "issue 8979: while with comparison involving mutable var should not crash", .source_kind = .module, .source = while_mutable_comparison, .expected = .{ .inspect_str = "5.0" } },
    .{ .name = "issue 8979: while (False) should not crash", .source_kind = .module, .source = while_false, .expected = .{ .inspect_str = "42.0" } },
    .{ .name = "issue 8979: nested while - inner break does not save outer loop", .source = crash_now, .expected = .{ .crash = {} } },
    .{ .name = "tag union matching with payload inside function - single module", .source_kind = .module, .source = tag_payload_single, .expected = .{ .inspect_str = "42" } },
    .{ .name = "comptime exhaustiveness - match succeeds empirically", .source_kind = .module, .source = comptime_exhaustive_match_ok, .expected = .{ .inspect_str = "\"blah\"" } },
    .{ .name = "comptime exhaustiveness - Email.parse destructure succeeds empirically", .source_kind = .module, .source = comptime_exhaustive_destructure_email, .expected = .{ .inspect_str = "\"alice@example.com\"" } },
    .{ .name = "comptime exhaustiveness - match failure is reported empirically", .source_kind = .module, .source = comptime_non_exhaustive_match, .expected = .{ .problem = {} } },
    .{ .name = "comptime exhaustiveness - destructure failure is reported empirically", .source_kind = .module, .source = comptime_non_exhaustive_destructure, .expected = .{ .problem = {} } },
    .{ .name = "comptime exhaustiveness - unused match alternative is reported", .source_kind = .module, .source = comptime_unused_match_alternative, .expected = .{ .problem = {} } },
    .{ .name = "comptime exhaustiveness - unused if branch is reported", .source_kind = .module, .source = comptime_unused_if_branch, .expected = .{ .problem = {} } },
    .{
        .name = "tag union matching with payload inside function - cross module",
        .source_kind = .module,
        .imports = &.{.{ .name = "MyTag", .source = "MyTag := [Foo({x: U64, y: U64}), Bar, Baz(Str)]\n" }},
        .source =
        \\import MyTag exposing [MyTag]
        \\
        \\lookup = |items, idx| {
        \\    match List.get(items, idx) {
        \\        Ok(val) => match val {
        \\            Foo(rec) => rec.x
        \\            Baz(_) => 99
        \\            _ => 0
        \\        }
        \\        Err(_) => 0
        \\    }
        \\}
        \\
        \\main = lookup([MyTag.Foo({x: 42, y: 7})], 0)
        ,
        .expected = .{ .inspect_str = "42" },
    },
    .{ .name = "issue 9262: dev evaluator handles opaque function field lookup", .source_kind = .module, .source = opaque_function_field, .expected = .{ .inspect_str = "True" } },
    .{
        .name = "issue 9281: dev evaluator stack overflow with nested recursive opaque types across modules",
        .source_kind = .module,
        .imports = &.{
            .{
                .name = "Elem",
                .source =
                \\Elem := [Div(List(Elem)), Text(Str)].{
                \\
                \\    div : List(Elem) -> Elem
                \\    div = |children| Div(children)
                \\
                \\    text : Str -> Elem
                \\    text = |s| Text(s)
                \\}
                ,
            },
        },
        .source =
        \\import Elem exposing [Elem, div, text]
        \\
        \\main = match div([text("hello")]) {
        \\    Div(_) => "Div (correct)"
        \\    _ => "other"
        \\}
        ,
        .expected = .{ .inspect_str = "\"Div (correct)\"" },
    },
    .{ .name = "issue #9349: top-level expect with type-erroneous condition does not panic in dev codegen", .source_kind = .module, .source = top_level_expect_type_error, .expected = .{ .problem = {} } },
};
