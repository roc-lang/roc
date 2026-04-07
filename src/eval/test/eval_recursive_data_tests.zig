//! Recursive nominal, cross-module, and deep recursion eval coverage.

const TestCase = @import("parallel_runner.zig").TestCase;

pub const tests = [_]TestCase{
    // Ported recursive nominal coverage from comptime_eval_test.zig
    .{
        .name = "inspect: recursive nominal IntList Nil",
        .source_kind = .module,
        .source =
        \\IntList := [Nil, Cons(I64, IntList)]
        \\
        \\main = IntList.Nil
        ,
        .expected = .{ .inspect_str = "Nil" },
    },
    .{
        .name = "inspect: recursive nominal IntList one element",
        .source_kind = .module,
        .source =
        \\IntList := [Nil, Cons(I64, IntList)]
        \\
        \\main = IntList.Cons(1, IntList.Nil)
        ,
        .expected = .{ .inspect_str = "Cons(1, Nil)" },
    },
    .{
        .name = "inspect: recursive nominal IntList two elements",
        .source_kind = .module,
        .source =
        \\IntList := [Nil, Cons(I64, IntList)]
        \\
        \\main = IntList.Cons(1, IntList.Cons(2, IntList.Nil))
        ,
        .expected = .{ .inspect_str = "Cons(1, Cons(2, Nil))" },
    },
    .{
        .name = "inspect: recursive nominal IntList three elements",
        .source_kind = .module,
        .source =
        \\IntList := [Nil, Cons(I64, IntList)]
        \\
        \\main = IntList.Cons(1, IntList.Cons(2, IntList.Cons(3, IntList.Nil)))
        ,
        .expected = .{ .inspect_str = "Cons(1, Cons(2, Cons(3, Nil)))" },
    },
    .{
        .name = "inspect: recursive nominal binary tree leaf",
        .source_kind = .module,
        .source =
        \\Tree := [Leaf, Node(Tree, I64, Tree)]
        \\
        \\main = Tree.Leaf
        ,
        .expected = .{ .inspect_str = "Leaf" },
    },
    .{
        .name = "inspect: recursive nominal binary tree single node",
        .source_kind = .module,
        .source =
        \\Tree := [Leaf, Node(Tree, I64, Tree)]
        \\
        \\main = Tree.Node(Tree.Leaf, 42, Tree.Leaf)
        ,
        .expected = .{ .inspect_str = "Node(Leaf, 42, Leaf)" },
    },
    .{
        .name = "inspect: recursive nominal binary tree two levels",
        .source_kind = .module,
        .source =
        \\Tree := [Leaf, Node(Tree, I64, Tree)]
        \\
        \\main = Tree.Node(
        \\    Tree.Node(Tree.Leaf, 1, Tree.Leaf),
        \\    2,
        \\    Tree.Node(Tree.Leaf, 3, Tree.Leaf)
        \\)
        ,
        .expected = .{ .inspect_str = "Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf))" },
    },
    .{
        .name = "inspect: recursive nominal option none",
        .source_kind = .module,
        .source =
        \\Maybe := [None, Some(I64)]
        \\
        \\main = Maybe.None
        ,
        .expected = .{ .inspect_str = "None" },
    },
    .{
        .name = "inspect: recursive nominal option some",
        .source_kind = .module,
        .source =
        \\Maybe := [None, Some(I64)]
        \\
        \\main = Maybe.Some(42)
        ,
        .expected = .{ .inspect_str = "Some(42)" },
    },
    .{
        .name = "inspect: recursive nominal nested option",
        .source_kind = .module,
        .source =
        \\MaybeInt := [None, Some(I64)]
        \\MaybeMaybe := [Nothing, Just(MaybeInt)]
        \\
        \\main = MaybeMaybe.Just(MaybeInt.Some(42))
        ,
        .expected = .{ .inspect_str = "Just(Some(42))" },
    },
    .{
        .name = "inspect: recursive nominal expression tree num",
        .source_kind = .module,
        .source =
        \\Expr := [Num(I64), Add(Expr, Expr)]
        \\
        \\main = Expr.Num(5)
        ,
        .expected = .{ .inspect_str = "Num(5)" },
    },
    .{
        .name = "inspect: recursive nominal expression tree add",
        .source_kind = .module,
        .source =
        \\Expr := [Num(I64), Add(Expr, Expr)]
        \\
        \\main = Expr.Add(Expr.Num(2), Expr.Num(3))
        ,
        .expected = .{ .inspect_str = "Add(Num(2), Num(3))" },
    },
    .{
        .name = "inspect: recursive nominal expression tree nested add",
        .source_kind = .module,
        .source =
        \\Expr := [Num(I64), Add(Expr, Expr)]
        \\
        \\main = Expr.Add(
        \\    Expr.Add(Expr.Num(1), Expr.Num(2)),
        \\    Expr.Num(3)
        \\)
        ,
        .expected = .{ .inspect_str = "Add(Add(Num(1), Num(2)), Num(3))" },
    },
    .{
        .name = "inspect: recursive nominal peano zero",
        .source_kind = .module,
        .source =
        \\Nat := [Zero, Succ(Nat)]
        \\
        \\main = Nat.Zero
        ,
        .expected = .{ .inspect_str = "Zero" },
    },
    .{
        .name = "inspect: recursive nominal peano one",
        .source_kind = .module,
        .source =
        \\Nat := [Zero, Succ(Nat)]
        \\
        \\main = Nat.Succ(Nat.Zero)
        ,
        .expected = .{ .inspect_str = "Succ(Zero)" },
    },
    .{
        .name = "inspect: recursive nominal peano three",
        .source_kind = .module,
        .source =
        \\Nat := [Zero, Succ(Nat)]
        \\
        \\main = Nat.Succ(Nat.Succ(Nat.Succ(Nat.Zero)))
        ,
        .expected = .{ .inspect_str = "Succ(Succ(Succ(Zero)))" },
    },
    .{
        .name = "inspect: recursive nominal json null",
        .source_kind = .module,
        .source =
        \\Json := [Null, Bool(Bool), Number(I64), Array(List(Json))]
        \\
        \\main = Json.Null
        ,
        .expected = .{ .inspect_str = "Null" },
    },
    .{
        .name = "inspect: recursive nominal json bool",
        .source_kind = .module,
        .source =
        \\Json := [Null, Bool(Bool), Number(I64), Array(List(Json))]
        \\
        \\main = Json.Bool(True)
        ,
        .expected = .{ .inspect_str = "Bool(True)" },
    },
    .{
        .name = "inspect: recursive nominal json number",
        .source_kind = .module,
        .source =
        \\Json := [Null, Bool(Bool), Number(I64), Array(List(Json))]
        \\
        \\main = Json.Number(42)
        ,
        .expected = .{ .inspect_str = "Number(42)" },
    },
    .{
        .name = "inspect: recursive nominal json empty array",
        .source_kind = .module,
        .source =
        \\Json := [Null, Bool(Bool), Number(I64), Array(List(Json))]
        \\
        \\main = Json.Array([])
        ,
        .expected = .{ .inspect_str = "Array([])" },
    },
    .{
        .name = "inspect: recursive nominal dom text",
        .source_kind = .module,
        .source =
        \\Node := [Text(Str), Element(Str, List(Node))]
        \\
        \\main = Node.Text("hello")
        ,
        .expected = .{ .inspect_str = "Text(\"hello\")" },
    },
    .{
        .name = "inspect: recursive nominal dom element empty",
        .source_kind = .module,
        .source =
        \\Node := [Text(Str), Element(Str, List(Node))]
        \\
        \\main = Node.Element("div", [])
        ,
        .expected = .{ .inspect_str = "Element(\"div\", [])" },
    },
    .{
        .name = "inspect: recursive nominal dom element with text child",
        .source_kind = .module,
        .source =
        \\Node := [Text(Str), Element(Str, List(Node))]
        \\
        \\main = Node.Element("p", [Node.Text("hello")])
        ,
        .expected = .{ .inspect_str = "Element(\"p\", [Text(\"hello\")])" },
    },
    .{
        .name = "inspect: recursive nominal dom nested elements",
        .source_kind = .module,
        .source =
        \\Node := [Text(Str), Element(Str, List(Node))]
        \\
        \\main = Node.Element("div", [
        \\    Node.Element("span", [Node.Text("Hello")]),
        \\    Node.Element("p", [Node.Text("World"), Node.Text("!")])
        \\])
        ,
        .expected = .{ .inspect_str = "Element(\"div\", [Element(\"span\", [Text(\"Hello\")]), Element(\"p\", [Text(\"World\"), Text(\"!\")])])" },
    },
    .{
        .name = "inspect: recursive nominal result ok",
        .source_kind = .module,
        .source =
        \\Result := [Ok(I64), Err(Str)]
        \\
        \\main = Result.Ok(42)
        ,
        .expected = .{ .inspect_str = "Ok(42)" },
    },
    .{
        .name = "inspect: recursive nominal result err",
        .source_kind = .module,
        .source =
        \\Result := [Ok(I64), Err(Str)]
        \\
        \\main = Result.Err("something went wrong")
        ,
        .expected = .{ .inspect_str = "Err(\"something went wrong\")" },
    },
    .{
        .name = "inspect: recursive nominal multiple lists tuple",
        .source_kind = .module,
        .source =
        \\IntList := [INil, ICons(I64, IntList)]
        \\StrList := [SNil, SCons(Str, StrList)]
        \\
        \\x = IntList.ICons(1, IntList.INil)
        \\y = StrList.SCons("hello", StrList.SNil)
        \\main = (x, y)
        ,
        .expected = .{ .inspect_str = "(ICons(1, INil), SCons(\"hello\", SNil))" },
    },
    .{
        .name = "inspect: recursive nominal rose tree",
        .source_kind = .module,
        .source =
        \\Rose := [Rose(I64, List(Rose))]
        \\
        \\main = Rose.Rose(1, [])
        ,
        .expected = .{ .inspect_str = "Rose(1, [])" },
    },
    .{
        .name = "inspect: recursive nominal rose tree with children",
        .source_kind = .module,
        .source =
        \\Rose := [Rose(I64, List(Rose))]
        \\
        \\main = Rose.Rose(1, [Rose.Rose(2, []), Rose.Rose(3, [])])
        ,
        .expected = .{ .inspect_str = "Rose(1, [Rose(2, []), Rose(3, [])])" },
    },
    .{
        .name = "inspect: recursive nominal stack empty",
        .source_kind = .module,
        .source =
        \\Stack := [Empty, Push(I64, Stack)]
        \\
        \\main = Stack.Empty
        ,
        .expected = .{ .inspect_str = "Empty" },
    },
    .{
        .name = "inspect: recursive nominal stack with items",
        .source_kind = .module,
        .source =
        \\Stack := [Empty, Push(I64, Stack)]
        \\
        \\main = Stack.Push(3, Stack.Push(2, Stack.Push(1, Stack.Empty)))
        ,
        .expected = .{ .inspect_str = "Push(3, Push(2, Push(1, Empty)))" },
    },
    .{
        .name = "inspect: recursive nominal queue",
        .source_kind = .module,
        .source =
        \\Queue := [Empty, Node(I64, Queue)]
        \\
        \\main = Queue.Node(1, Queue.Node(2, Queue.Empty))
        ,
        .expected = .{ .inspect_str = "Node(1, Node(2, Empty))" },
    },
    .{
        .name = "inspect: recursive nominal arithmetic expr",
        .source_kind = .module,
        .source =
        \\Arith := [Lit(I64), Add(Arith, Arith), Mul(Arith, Arith), Neg(Arith)]
        \\
        \\main = Arith.Mul(
        \\    Arith.Add(Arith.Lit(2), Arith.Lit(3)),
        \\    Arith.Neg(Arith.Lit(4))
        \\)
        ,
        .expected = .{ .inspect_str = "Mul(Add(Lit(2), Lit(3)), Neg(Lit(4)))" },
    },
    .{
        .name = "inspect: recursive nominal logic expr",
        .source_kind = .module,
        .source =
        \\Logic := [True, False, And(Logic, Logic), Or(Logic, Logic), Not(Logic)]
        \\
        \\main = Logic.And(Logic.Or(Logic.True, Logic.False), Logic.Not(Logic.False))
        ,
        .expected = .{ .inspect_str = "And(Or(True, False), Not(False))" },
    },
    .{
        .name = "inspect: recursive nominal singly linked",
        .source_kind = .module,
        .source =
        \\Linked := [End, Link(I64, Linked)]
        \\
        \\main = Linked.Link(1, Linked.Link(2, Linked.End))
        ,
        .expected = .{ .inspect_str = "Link(1, Link(2, End))" },
    },
    .{
        .name = "inspect: recursive nominal chain of five",
        .source_kind = .module,
        .source =
        \\Chain := [End, Link(I64, Chain)]
        \\
        \\main = Chain.Link(1, Chain.Link(2, Chain.Link(3, Chain.Link(4, Chain.Link(5, Chain.End)))))
        ,
        .expected = .{ .inspect_str = "Link(1, Link(2, Link(3, Link(4, Link(5, End)))))" },
    },
    .{
        .name = "inspect: recursive nominal three way tree",
        .source_kind = .module,
        .source =
        \\Tri := [Tip, Branch(Tri, Tri, Tri)]
        \\
        \\main = Tri.Branch(Tri.Tip, Tri.Tip, Tri.Tip)
        ,
        .expected = .{ .inspect_str = "Branch(Tip, Tip, Tip)" },
    },
    .{
        .name = "inspect: recursive nominal three way tree nested",
        .source_kind = .module,
        .source =
        \\Tri := [Tip, Branch(Tri, Tri, Tri)]
        \\
        \\main = Tri.Branch(
        \\    Tri.Branch(Tri.Tip, Tri.Tip, Tri.Tip),
        \\    Tri.Tip,
        \\    Tri.Branch(Tri.Tip, Tri.Tip, Tri.Tip)
        \\)
        ,
        .expected = .{ .inspect_str = "Branch(Branch(Tip, Tip, Tip), Tip, Branch(Tip, Tip, Tip))" },
    },
    .{
        .name = "inspect: recursive nominal stream",
        .source_kind = .module,
        .source =
        \\Stream := [Done, More(I64, Stream)]
        \\
        \\main = Stream.More(1, Stream.More(2, Stream.Done))
        ,
        .expected = .{ .inspect_str = "More(1, More(2, Done))" },
    },
    .{
        .name = "inspect: recursive nominal difference list",
        .source_kind = .module,
        .source =
        \\DList := [Empty, Single(I64), Append(DList, DList)]
        \\
        \\main = DList.Append(DList.Single(1), DList.Append(DList.Single(2), DList.Single(3)))
        ,
        .expected = .{ .inspect_str = "Append(Single(1), Append(Single(2), Single(3)))" },
    },
    .{
        .name = "inspect: recursive nominal rope",
        .source_kind = .module,
        .source =
        \\Rope := [Leaf(Str), Concat(Rope, Rope)]
        \\
        \\main = Rope.Concat(Rope.Leaf("hello"), Rope.Concat(Rope.Leaf(" "), Rope.Leaf("world")))
        ,
        .expected = .{ .inspect_str = "Concat(Leaf(\"hello\"), Concat(Leaf(\" \"), Leaf(\"world\")))" },
    },
    .{
        .name = "inspect: recursive nominal finger",
        .source_kind = .module,
        .source =
        \\Finger := [Zero, One(I64), Two(I64, I64), Deep(Finger, List(I64), Finger)]
        \\
        \\main = Finger.Deep(Finger.One(1), [2, 3], Finger.One(4))
        ,
        .expected = .{ .inspect_str = "Deep(One(1), [2, 3], One(4))" },
    },
    .{
        .name = "inspect: recursive nominal trie",
        .source_kind = .module,
        .source =
        \\Trie := [Empty, Leaf(I64), Branch(List(Trie))]
        \\
        \\main = Trie.Branch([Trie.Leaf(1), Trie.Empty, Trie.Leaf(2)])
        ,
        .expected = .{ .inspect_str = "Branch([Leaf(1), Empty, Leaf(2)])" },
    },
    .{
        .name = "inspect: recursive nominal zipper tuple",
        .source_kind = .module,
        .source =
        \\Tree := [Empty, Node(Tree, I64, Tree)]
        \\Crumb := [LeftCrumb(I64, Tree), RightCrumb(Tree, I64)]
        \\
        \\focus = Tree.Node(Tree.Empty, 5, Tree.Empty)
        \\trail = [Crumb.LeftCrumb(10, Tree.Empty)]
        \\main = (focus, trail)
        ,
        .expected = .{ .inspect_str = "(Node(Empty, 5, Empty), [LeftCrumb(10, Empty)])" },
    },
    .{
        .name = "inspect: recursive nominal menu",
        .source_kind = .module,
        .source =
        \\Menu := [Item(Str), SubMenu(Str, List(Menu))]
        \\
        \\main = Menu.SubMenu("File", [Menu.Item("New"), Menu.Item("Open"), Menu.SubMenu("Recent", [])])
        ,
        .expected = .{ .inspect_str = "SubMenu(\"File\", [Item(\"New\"), Item(\"Open\"), SubMenu(\"Recent\", [])])" },
    },
    .{
        .name = "inspect: recursive nominal filesystem",
        .source_kind = .module,
        .source =
        \\FS := [File(Str), Dir(Str, List(FS))]
        \\
        \\main = FS.Dir("root", [
        \\    FS.File("readme.txt"),
        \\    FS.Dir("src", [FS.File("main.roc")])
        \\])
        ,
        .expected = .{ .inspect_str = "Dir(\"root\", [File(\"readme.txt\"), Dir(\"src\", [File(\"main.roc\")])])" },
    },
    .{
        .name = "inspect: recursive nominal org chart",
        .source_kind = .module,
        .source =
        \\Org := [Employee(Str), Manager(Str, List(Org))]
        \\
        \\main = Org.Manager("CEO", [
        \\    Org.Manager("CTO", [Org.Employee("Dev1"), Org.Employee("Dev2")]),
        \\    Org.Employee("CFO")
        \\])
        ,
        .expected = .{ .inspect_str = "Manager(\"CEO\", [Manager(\"CTO\", [Employee(\"Dev1\"), Employee(\"Dev2\")]), Employee(\"CFO\")])" },
    },
    .{
        .name = "inspect: recursive nominal path segments",
        .source_kind = .module,
        .source =
        \\Path := [Root, Segment(Str, Path)]
        \\
        \\main = Path.Segment("home", Path.Segment("user", Path.Segment("docs", Path.Root)))
        ,
        .expected = .{ .inspect_str = "Segment(\"home\", Segment(\"user\", Segment(\"docs\", Root)))" },
    },
    .{
        .name = "inspect: recursive nominal command chain",
        .source_kind = .module,
        .source =
        \\Cmd := [Done, Step(Str, Cmd)]
        \\
        \\main = Cmd.Step("init", Cmd.Step("build", Cmd.Step("test", Cmd.Done)))
        ,
        .expected = .{ .inspect_str = "Step(\"init\", Step(\"build\", Step(\"test\", Done)))" },
    },
    .{
        .name = "inspect: recursive nominal inside Try tuple issue 8855",
        .source_kind = .module,
        .source =
        \\Statement := [ForLoop(List(Statement)), IfStatement(List(Statement))]
        \\
        \\parse_block : List(U8), U64, List(Statement) -> [Ok((List(Statement), U64)), Err(Str)]
        \\parse_block = |_file, index, acc| Ok((acc, index))
        \\
        \\main = parse_block([], 0, [])
        ,
        .expected = .{ .inspect_str = "Ok(([], 0))" },
    },
    .{
        .name = "inspect: recursive nominal recursion through tuple issue 8795",
        .source_kind = .module,
        .source =
        \\Type := [Name(Str), Array((U64, Type))]
        \\
        \\inner = Type.Name("hello")
        \\main = Type.Array((0, inner))
        ,
        .expected = .{ .inspect_str = "Array((0, Name(\"hello\")))" },
    },
    .{
        .name = "inspect: nested nominal in tuple issue 8874",
        .source_kind = .module,
        .source =
        \\main : Try((Try(Str, Str), U64), Str)
        \\main = Ok((Ok("todo"), 3))
        ,
        .expected = .{ .inspect_str = "Ok((Ok(\"todo\"), 3))" },
    },
    .{
        .name = "inspect: recursive nominal through record field",
        .source_kind = .module,
        .source =
        \\Type := [Leaf, Node({ value: Str, child: Type })]
        \\
        \\inner = Type.Leaf
        \\main = Type.Node({ value: "hello", child: inner })
        ,
        .expected = .{ .inspect_str = "Node({ child: Leaf, value: \"hello\" })" },
    },
    .{
        .name = "inspect: recursive nominal deeply nested record recursion",
        .source_kind = .module,
        .source =
        \\Type := [Leaf(Str), Node({ value: Str, child: Type })]
        \\
        \\leaf = Type.Leaf("deep")
        \\level1 = Type.Node({ value: "level1", child: leaf })
        \\level2 = Type.Node({ value: "level2", child: level1 })
        \\main = Type.Node({ value: "level3", child: level2 })
        ,
        .expected = .{ .inspect_str = "Node({ child: Node({ child: Node({ child: Leaf(\"deep\"), value: \"level1\" }), value: \"level2\" }), value: \"level3\" })" },
    },
    .{
        .name = "inspect: recursive tag payload match issue 8754",
        .source_kind = .module,
        .source =
        \\Tree := [Node(Str, List(Tree)), Text(Str), Wrapper(Tree)]
        \\
        \\inner : Tree
        \\inner = Text("hello")
        \\
        \\wrapped : Tree
        \\wrapped = Wrapper(inner)
        \\
        \\main = match wrapped {
        \\    Wrapper(inner_tree) =>
        \\        match inner_tree {
        \\            Text(_) => 1
        \\            Node(_, _) => 2
        \\            Wrapper(_) => 3
        \\        }
        \\    _ => 0
        \\}
        ,
        .expected = .{ .inspect_str = "1.0" },
    },
    .{
        .name = "inspect: recursive nominal issue 8901 zero branch",
        .source_kind = .module,
        .source =
        \\Nat := [Zero, Suc(Box(Nat))]
        \\
        \\main = Nat.Zero
        ,
        .expected = .{ .inspect_str = "Zero" },
    },
    .{
        .name = "inspect: recursive nominal issue 8901 pattern match",
        .source_kind = .module,
        .source =
        \\Nat := [Zero, Suc(Box(Nat))]
        \\
        \\to_num = |n|
        \\    match n {
        \\        Zero => 0.I64
        \\        Suc(_) => 1.I64
        \\    }
        \\
        \\main = to_num(Nat.Zero)
        ,
        .expected = .{ .inspect_str = "0" },
    },

    // New coverage requested for cross-module captures, recursion, and mutual recursion
    .{
        .name = "inspect: cross module capture factory",
        .source_kind = .module,
        .source =
        \\module []
        \\
        \\import A
        \\
        \\main = A.make_adder(40.I64)(2.I64)
        ,
        .imports = &.{
            .{
                .name = "A",
                .source =
                \\module [make_adder]
                \\
                \\make_adder = |captured| |n| captured + n
                ,
            },
        },
        .expected = .{ .inspect_str = "42" },
    },
    .{
        .name = "inspect: cross module recursive path",
        .source_kind = .module,
        .source =
        \\module []
        \\
        \\import A
        \\
        \\main = A.sum_to(5.I64)
        ,
        .imports = &.{
            .{
                .name = "A",
                .source =
                \\module [sum_to]
                \\
                \\sum_to = |n|
                \\    if n == 0.I64
                \\        0.I64
                \\    else
                \\        n + sum_to(n - 1.I64)
                ,
            },
        },
        .expected = .{ .inspect_str = "15" },
    },
    .{
        .name = "inspect: mutually recursive functions",
        .source_kind = .module,
        .source =
        \\even : I64 -> Bool
        \\even = |n| if n == 0.I64 True else odd(n - 1.I64)
        \\
        \\odd : I64 -> Bool
        \\odd = |n| if n == 0.I64 False else even(n - 1.I64)
        \\
        \\main = (even(10.I64), odd(11.I64))
        ,
        .expected = .{ .inspect_str = "(True, True)" },
    },
    .{
        .name = "inspect: very deep recursion stack behavior",
        .source_kind = .module,
        .source =
        \\depth : I64 -> I64
        \\depth = |n| if n == 0.I64 0.I64 else 1.I64 + depth(n - 1.I64)
        \\
        \\main = depth(512.I64)
        ,
        .expected = .{ .inspect_str = "512" },
    },
    .{
        .name = "inspect: mutually recursive data structures in one type module",
        .source_kind = .module,
        .source =
        \\Tree := [Leaf, Branch(Tree.Forest)].{
        \\    Forest := [Empty, More(Tree, Forest)]
        \\}
        \\
        \\main = Tree.Branch(Tree.Forest.More(Tree.Leaf, Tree.Forest.Empty))
        ,
        .expected = .{ .inspect_str = "Branch(More(Leaf, Empty))" },
    },
    .{
        .name = "inspect: deep recursive nominal runtime inspection",
        .source_kind = .module,
        .source =
        \\Node := [Leaf(Str), Branch(Str, List(Node))]
        \\
        \\main = Node.Branch("root", [
        \\    Node.Branch("left", [Node.Leaf("a"), Node.Leaf("b")]),
        \\    Node.Branch("right", [Node.Branch("inner", [Node.Leaf("c")])])
        \\])
        ,
        .expected = .{ .inspect_str = "Branch(\"root\", [Branch(\"left\", [Leaf(\"a\"), Leaf(\"b\")]), Branch(\"right\", [Branch(\"inner\", [Leaf(\"c\")])])])" },
    },
    .{
        .name = "inspect: wrapped tag union in wrapped record issue 8930",
        .source_kind = .module,
        .source =
        \\ValueCombinationMethod := [Divide, Modulo, Add, Subtract]
        \\Value := [CombinedValue({combination_method: ValueCombinationMethod})]
        \\
        \\main = Value.CombinedValue({combination_method: ValueCombinationMethod.Add})
        ,
        .expected = .{ .inspect_str = "CombinedValue({ combination_method: Add })" },
    },
    .{
        .name = "inspect: wrapper function for List.get with match issue 8944",
        .source_kind = .module,
        .source =
        \\nth = |l, i| {
        \\    match List.get(l, i) {
        \\        Ok(e) => Ok(e)
        \\        Err(OutOfBounds) => Err(OutOfBounds)
        \\    }
        \\}
        \\
        \\first = nth(["a", "b", "c", "d", "e"], 2)
        \\second = nth(["a"], 2)
        \\main = (first, second)
        ,
        .expected = .{ .inspect_str = "(Ok(\"c\"), Err(OutOfBounds))" },
    },
    .{
        .name = "inspect: tag union payload matching inside function single module",
        .source_kind = .module,
        .source =
        \\MyTag := [Foo({x: U64, y: U64}), Bar, Baz(Str)]
        \\
        \\lookup = |items, idx| {
        \\    match List.get(items, idx) {
        \\        Ok(val) =>
        \\            match val {
        \\                Foo(rec) => rec.x
        \\                Baz(_) => 99
        \\                _ => 0
        \\            }
        \\        Err(_) => 0
        \\    }
        \\}
        \\
        \\items = [MyTag.Foo({x: 42, y: 7})]
        \\inline = match List.get(items, 0) {
        \\    Ok(val) => match val { Foo(rec) => rec.x, _ => 0 }
        \\    Err(_) => 0
        \\}
        \\main = (inline, lookup(items, 0))
        ,
        .expected = .{ .inspect_str = "(42, 42)" },
    },
    .{
        .name = "inspect: tag union payload matching inside function cross module",
        .source_kind = .module,
        .source =
        \\module []
        \\
        \\import A exposing [MyTag]
        \\
        \\lookup = |items, idx| {
        \\    match List.get(items, idx) {
        \\        Ok(val) =>
        \\            match val {
        \\                Foo(rec) => rec.x
        \\                Baz(_) => 99
        \\                _ => 0
        \\            }
        \\        Err(_) => 0
        \\    }
        \\}
        \\
        \\items = [MyTag.Foo({x: 42, y: 7})]
        \\inline = match List.get(items, 0) {
        \\    Ok(val) => match val { Foo(rec) => rec.x, _ => 0 }
        \\    Err(_) => 0
        \\}
        \\main = (inline, lookup(items, 0))
        ,
        .imports = &.{
            .{
                .name = "A",
                .source =
                \\module [MyTag]
                \\
                \\MyTag := [Foo({x: U64, y: U64}), Bar, Baz(Str)]
                ,
            },
        },
        .expected = .{ .inspect_str = "(42, 42)" },
    },
};
