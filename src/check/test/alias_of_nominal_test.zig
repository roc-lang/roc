//! Tests for an alias of a nominal type used by that nominal type itself.
//!
//! An alias is a transparent name for the type it is declared as, so a nominal
//! type may name itself through an alias in its own declaration and in its
//! methods' annotations: the recursion passes through the nominal type, which
//! is what makes it legal. An alias cycle that passes through no nominal type
//! is still a recursive alias.

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");

test "a nominal declaration may name itself through an alias declared before it" {
    var env = try TestEnv.init("Repro",
        \\Repro :: [].{}
        \\
        \\T : Tree
        \\
        \\Tree := [Leaf, Node(T, T)].{
        \\    depth : T -> U64
        \\    depth = |tree| match tree {
        \\        Leaf => 0
        \\        Node(left, _) => 1 + left.depth()
        \\    }
        \\
        \\    is_eq : T, Tree -> Bool
        \\    is_eq = |a, b| a.depth() == b.depth()
        \\}
        \\
        \\one = T.Node(T.Leaf, Tree.Leaf)
        \\
        \\deep = one.depth()
        \\
        \\same = one == Tree.Leaf
    );
    defer env.deinit();

    try env.assertNoErrors();
    try env.assertNominalDeclValidity("Tree", true);
    try env.assertDefType("one", "T");
    try env.assertDefType("deep", "U64");
    try env.assertDefType("same", "Bool");
}

test "a nominal declaration may name itself through an alias declared after it" {
    var env = try TestEnv.init("Repro",
        \\Repro :: [].{}
        \\
        \\Tree := [Leaf, Node(T, T)]
        \\
        \\T : Tree
        \\
        \\one : Tree
        \\one = Tree.Node(T.Leaf, Tree.Leaf)
    );
    defer env.deinit();

    try env.assertNoErrors();
    try env.assertNominalDeclValidity("Tree", true);
}

test "a nominal declaration may name itself through a chain of aliases" {
    var env = try TestEnv.init("Repro",
        \\Repro :: [].{}
        \\
        \\T3 : T2
        \\
        \\T2 : T1
        \\
        \\T1 : Tree
        \\
        \\Tree := [Leaf, Node(T3, T2)].{
        \\    depth : T3 -> U64
        \\    depth = |tree| match tree {
        \\        Leaf => 0
        \\        Node(left, _) => 1 + left.depth()
        \\    }
        \\
        \\    mirror : T2 -> T1
        \\    mirror = |tree| match tree {
        \\        Leaf => T3.Leaf
        \\        Node(left, right) => T2.Node(right, left)
        \\    }
        \\}
        \\
        \\deep = T3.Node(T1.Leaf, Tree.Leaf).mirror().depth()
    );
    defer env.deinit();

    try env.assertNoErrors();
    try env.assertDefType("deep", "U64");
}

test "a parameterized nominal declaration may name itself through an alias" {
    var env = try TestEnv.init("Repro",
        \\Repro :: [].{}
        \\
        \\Rest(a) : Chain(a)
        \\
        \\Chain(a) := [End, Link(a, Rest(a))].{
        \\    len : Rest(a) -> U64
        \\    len = |chain| match chain {
        \\        End => 0
        \\        Link(_, rest) => 1 + rest.len()
        \\    }
        \\}
        \\
        \\count = Rest.Link("a", Chain.Link("b", Rest.End)).len()
    );
    defer env.deinit();

    try env.assertNoErrors();
    try env.assertDefType("count", "U64");
}

test "two nominal declarations may name each other through aliases" {
    var env = try TestEnv.init("Repro",
        \\Repro :: [].{}
        \\
        \\E : Even
        \\
        \\O : Odd
        \\
        \\Even := [Zero, Succ(O)]
        \\
        \\Odd := [Succ(E)]
        \\
        \\two : E
        \\two = Even.Succ(O.Succ(E.Zero))
    );
    defer env.deinit();

    try env.assertNoErrors();
}

test "a method annotation may name its type through an alias" {
    var env = try TestEnv.init("Repro",
        \\Repro :: [].{}
        \\
        \\Point := { x : U64, y : U64 }.{
        \\    flip : Pt -> P
        \\    flip = |point| Pt.{ x: point.y, y: point.x }
        \\
        \\    is_eq : P, Pt -> Bool
        \\    is_eq = |a, b| a.x == b.x and a.y == b.y
        \\}
        \\
        \\Pt : Point
        \\
        \\P : Pt
        \\
        \\flipped = P.{ x: 1, y: 2 }.flip()
        \\
        \\same = flipped == Point.{ x: 2, y: 1 }
    );
    defer env.deinit();

    try env.assertNoErrors();
    try env.assertDefType("flipped", "P");
    try env.assertDefType("same", "Bool");
}

test "an alias that names itself through no nominal type is still recursive" {
    var env = try TestEnv.init("Repro",
        \\Repro :: [].{}
        \\
        \\Tree := [Leaf, Node(T, T)]
        \\
        \\T : [Wrapped(Tree), Again(T)]
    );
    defer env.deinit();

    try env.assertFirstTypeError("Recursive Alias");
}

test "aliases that name each other through no nominal type are still recursive" {
    var env = try TestEnv.init("Repro",
        \\Repro :: [].{}
        \\
        \\Tree := [Leaf, Node(A)]
        \\
        \\A : [First(B), Tree(Tree)]
        \\
        \\B : [Second(A)]
    );
    defer env.deinit();

    try env.assertCanErrors(&.{ "Mutually Recursive Type Aliases", "Mutually Recursive Type Aliases" });
}

test "an alias in one module of a nominal type in another" {
    var shapes = try TestEnv.init("Shapes",
        \\Shapes :: [].{
        \\    Point := { x : U64, y : U64 }.{
        \\        sum : Point -> U64
        \\        sum = |point| point.x + point.y
        \\
        \\        is_eq : Point, Point -> Bool
        \\        is_eq = |a, b| a.x == b.x and a.y == b.y
        \\    }
        \\}
    );
    defer shapes.deinit();
    try shapes.assertNoErrors();

    var api = try TestEnv.initWithImport("Api",
        \\import Shapes
        \\
        \\Api :: [].{
        \\    Pt : Shapes.Point
        \\
        \\    origin : Pt
        \\    origin = Pt.{ x: 0, y: 0 }
        \\
        \\    total : Pt -> U64
        \\    total = |point| point.sum()
        \\
        \\    at_origin : Pt -> Bool
        \\    at_origin = |point| point == origin
        \\}
    , "Shapes", &shapes);
    defer api.deinit();
    try api.assertNoErrors();
    try api.assertDefType("Api.origin", "Api.Pt");
}

test "an alias chain spanning modules" {
    var shapes = try TestEnv.init("Shapes",
        \\Shapes :: [].{
        \\    Point := { x : U64, y : U64 }.{
        \\        sum : Point -> U64
        \\        sum = |point| point.x + point.y
        \\
        \\        is_eq : Point, Point -> Bool
        \\        is_eq = |a, b| a.x == b.x and a.y == b.y
        \\    }
        \\}
    );
    defer shapes.deinit();
    try shapes.assertNoErrors();

    var api = try TestEnv.initWithImport("Api",
        \\import Shapes
        \\
        \\Api :: [].{
        \\    Pt : Shapes.Point
        \\}
    , "Shapes", &shapes);
    defer api.deinit();
    try api.assertNoErrors();

    var cross = try TestEnv.initWithImport("Cross",
        \\import Api
        \\
        \\Cross :: [].{}
        \\
        \\P : Api.Pt
        \\
        \\Q : P
        \\
        \\built = Q.{ x: 1, y: 2 }
        \\
        \\total = built.sum()
        \\
        \\same = built == Api.Pt.{ x: 1, y: 2 }
    , "Api", &api);
    defer cross.deinit();

    try cross.assertNoErrors();
    try cross.assertDefType("built", "Q");
    try cross.assertDefType("total", "U64");
    try cross.assertDefType("same", "Bool");
}

test "a module whose nominal type names itself through an alias is usable from another module" {
    var forest = try TestEnv.init("Forest",
        \\Forest :: [].{
        \\    T : Tree
        \\
        \\    Tree := [Leaf, Node(T, T)].{
        \\        depth : T -> U64
        \\        depth = |tree| match tree {
        \\            Leaf => 0
        \\            Node(left, _) => 1 + left.depth()
        \\        }
        \\
        \\        is_eq : T, Tree -> Bool
        \\        is_eq = |a, b| a.depth() == b.depth()
        \\
        \\        graft : T, T -> T
        \\        graft = |left, right| Tree.Node(left, right)
        \\    }
        \\}
    );
    defer forest.deinit();
    try forest.assertNoErrors();

    var cross = try TestEnv.initWithImport("Cross",
        \\import Forest
        \\
        \\Cross :: [].{}
        \\
        \\Mine : Forest.T
        \\
        \\grown = Forest.T.Node(Forest.Tree.Leaf, Mine.Leaf).graft(Forest.Tree.Leaf)
        \\
        \\deep = grown.depth()
        \\
        \\same = grown == Mine.Leaf
        \\
        \\walk : Mine -> U64
        \\walk = |tree| match tree {
        \\    Forest.Tree.Leaf => 0
        \\    Forest.T.Node(left, _) => 1 + walk(left)
        \\}
    , "Forest", &forest);
    defer cross.deinit();

    try cross.assertNoErrors();
    try cross.assertDefType("deep", "U64");
    try cross.assertDefType("same", "Bool");
}
