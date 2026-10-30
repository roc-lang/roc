//! repro for https://github.com/roc-lang/roc/issues/12553
const TestEnv = @import("TestEnv.zig");

test "issue 12553: a caller's type that a merged method requirement rejects is reported at the call" {
    var env = try TestEnv.init("Test",
        \\f = |xs| {
        \\    a = Str.join_with(xs.map(|_| "a"), "")
        \\    b = xs.map(|x| x.len())
        \\    (a, b)
        \\}
        \\
        \\g = || f([[1.U8]])
    );
    defer env.deinit();
    try env.assertTypeErrorMsgs(&.{
        \\**Type Mismatch**
        \\The `len` method on `List` has an incompatible type.
        \\```roc
        \\g = || f([[1.U8]])
        \\```
        \\       ^^^^^^^^^^^
        \\
        \\The method `len` has the type:
        \\
        \\    List(U8) -> U64
        \\
        \\But I need it to have the type:
        \\
        \\    List(U8) -> Str
        \\
        \\This use needs that because of this code in the definition it uses:
        \\```roc
        \\    b = xs.map(|x| x.len())
        \\```
        \\                   ^^^^^^^
        \\
        \\These `map` calls share one `map` type, because the type of the value they are called on is not known where they are:
        \\```roc
        \\    a = Str.join_with(xs.map(|_| "a"), "")
        \\```
        \\                      ^^^^^^^^^^^^^^^
        \\
        \\```roc
        \\    b = xs.map(|x| x.len())
        \\```
        \\        ^^^^^^^^^^^^^^^^^^^
        \\
        \\**Hint:** Giving that value a type annotation lets each call have its own type.
        \\
        \\
        ,
    });
}

test "issue 12553: a literal that a merged method requirement converts to a caller's type is reported at the call" {
    var env = try TestEnv.init("Test",
        \\f = |xs| (xs.map(|_| "a"), xs.map(|x| x + 1))
        \\
        \\expect f([1.U64, 2]) == (["a", "a"], [2, 3])
    );
    defer env.deinit();
    try env.assertTypeErrorMsgs(&.{
        \\**Type Mismatch**
        \\This use needs a string literal in the definition it uses to have a non-string type.
        \\```roc
        \\expect f([1.U64, 2]) == (["a", "a"], [2, 3])
        \\```
        \\       ^^^^^^^^^^^^^
        \\
        \\The string literal is here:
        \\```roc
        \\f = |xs| (xs.map(|_| "a"), xs.map(|x| x + 1))
        \\```
        \\                     ^^^
        \\
        \\The type was determined to be:
        \\
        \\    U64
        \\
        \\These `map` calls share one `map` type, because the type of the value they are called on is not known where they are:
        \\```roc
        \\f = |xs| (xs.map(|_| "a"), xs.map(|x| x + 1))
        \\```
        \\          ^^^^^^^^^^^^^^^
        \\
        \\```roc
        \\f = |xs| (xs.map(|_| "a"), xs.map(|x| x + 1))
        \\```
        \\                           ^^^^^^^^^^^^^^^^^
        \\
        \\**Hint:** Giving that value a type annotation lets each call have its own type.
        \\
        \\
        ,
        \\**Type Mismatch**
        \\This string literal is being used where a non-string type is needed.
        \\```roc
        \\expect f([1.U64, 2]) == (["a", "a"], [2, 3])
        \\```
        \\                          ^^^
        \\
        \\The type was determined to be:
        \\
        \\    U64
        \\
        \\
        ,
    });
}

test "issue 12553: a copied method requirement that fails is reported at the use that copied it" {
    var env = try TestEnv.init("Test",
        \\f = |xs| Str.join_with(xs.map(|x| x.len()), "")
        \\
        \\g = || f([[1.U8]])
    );
    defer env.deinit();
    try env.assertTypeErrorMsgs(&.{
        \\**Type Mismatch**
        \\The `len` method on `List` has an incompatible type.
        \\```roc
        \\g = || f([[1.U8]])
        \\```
        \\       ^^^^^^^^^^^
        \\
        \\The method `len` has the type:
        \\
        \\    List(U8) -> U64
        \\
        \\But I need it to have the type:
        \\
        \\    List(U8) -> Str
        \\
        \\This use needs that because of this code in the definition it uses:
        \\```roc
        \\f = |xs| Str.join_with(xs.map(|x| x.len()), "")
        \\```
        \\                                  ^^^^^^^
        \\
        \\
        \\
        ,
    });
}

test "issue 12553: a copied string literal conversion that fails is reported at the use that copied it" {
    var env = try TestEnv.init("Test",
        \\f = |xs| xs.map(|_| "a")
        \\
        \\g : List(U64)
        \\g = f([1.U64])
    );
    defer env.deinit();
    try env.assertTypeErrorMsgs(&.{
        \\**Type Mismatch**
        \\This use needs a string literal in the definition it uses to have a non-string type.
        \\```roc
        \\g = f([1.U64])
        \\```
        \\    ^^^^^^^^^^
        \\
        \\The string literal is here:
        \\```roc
        \\f = |xs| xs.map(|_| "a")
        \\```
        \\                    ^^^
        \\
        \\The type was determined to be:
        \\
        \\    U64
        \\
        \\
        ,
    });
}

test "issue 12553: a method type that fails an obligation a same-method relation deferred is the method conflict" {
    var env = try TestEnv.init("Test",
        \\f = |xs| (Str.join_with(xs.map(|x| x.len()), ""), xs.map(|x| List.concat(x, [1.U8])))
    );
    defer env.deinit();
    try env.assertTypeErrorMsgs(&.{
        \\**Type Mismatch**
        \\This is used in a way that requires its type to have two different, incompatible `map` methods, but no type can have more than one `map` method.
        \\```roc
        \\f = |xs| (Str.join_with(xs.map(|x| x.len()), ""), xs.map(|x| List.concat(x, [1.U8])))
        \\```
        \\                        ^^^^^^^^^^^^^^^^^^^
        \\
        \\One part of this use needs `map` to have this type:
        \\
        \\    a, (List(U8) -> List(U8)) -> _ret
        \\
        \\Another part needs it to have this type:
        \\
        \\    a, (b -> c) -> List(Str) where [b.len : b -> c]
        \\
        \\A type has only one `map` method, so no type can satisfy both.
        \\
        \\
        ,
    });
}

test "issue 12553: a missing method a use copied is reported at that use with the code that needs it" {
    var env = try TestEnv.init("Test",
        \\f = |x| x.frobnicate()
        \\
        \\g = || f(1.U8)
    );
    defer env.deinit();
    try env.assertTypeErrorMsgs(&.{
        \\**Missing Method**
        \\This use needs the `frobnicate` method, but the type being used doesn't have that method.
        \\```roc
        \\g = || f(1.U8)
        \\```
        \\       ^^^^^^^
        \\
        \\The value's type, which does not have a method named `frobnicate`, is:
        \\
        \\    U8
        \\
        \\**Hint:** For this to work, the type would need to have a method named `frobnicate` associated with it in the type's declaration.
        \\
        \\This use needs that because of this code in the definition it uses:
        \\```roc
        \\f = |x| x.frobnicate()
        \\```
        \\        ^^^^^^^^^^^^^^
        \\
        \\
        \\
        ,
    });
}

test "issue 12553: a function stored where it is written reports its own literal" {
    var env = try TestEnv.init("Test",
        \\program : { value : U64 -> U64 }
        \\program = { value: |_| "not a U64" }
    );
    defer env.deinit();
    try env.assertTypeErrorMsgs(&.{
        \\**Type Mismatch**
        \\This string literal is being used where a non-string type is needed.
        \\```roc
        \\program = { value: |_| "not a U64" }
        \\```
        \\                       ^^^^^^^^^^^
        \\
        \\The type was determined to be:
        \\
        \\    U64
        \\
        \\
        ,
    });
}
