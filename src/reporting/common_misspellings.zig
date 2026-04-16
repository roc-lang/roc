//! Maps of common syntax mistakes and misspellings to helpful error messages.

const std = @import("std");

/// A static map of common misspellings and mistakes in Roc code.
/// Keys are the incorrect tokens/identifiers that users might write.
/// Values are helpful tip messages to guide users toward the correct Roc syntax.
pub const CommonMisspellings = struct {
    /// Token misspellings - mistakes that appear during tokenization
    pub const tokens = std.StaticStringMap([]const u8).initComptime(.{
        .{ "&&", "Roc uses the keyword `and` instead of `&&`, and the keyword `or` instead of `||`." },
        .{ "||", "Roc uses the keyword `and` instead of `&&`, and the keyword `or` instead of `||`." },
        .{ "\\", "Roc syntax does not use single backslashes. Roc lambda syntax is `|arg1, arg2| body`, and double backslash (`\\\\`) begins a line in a multiline string." },
        .{ "/=", "Roc uses `!=` for inequality, not `/=`." },
        .{ "===", "Roc uses `==` for equality comparison, not `===`." },
        .{ "!==", "Roc uses `!=` for inequality comparison, not `!==`." },
        .{ "++", "Roc doesn't have a `++` operator. Use `x + 1` for incrementing, or `List.concat` for concatenating lists." },
        .{ "--", "Roc doesn't have a `--` operator. Use `x - 1` for decrementing." },
    });

    /// Identifier misspellings - mistakes that appear during canonicalization
    pub const identifiers = std.StaticStringMap([]const u8).initComptime(.{
        // Keywords from other languages
        .{ "case", "`case` is not a keyword in Roc. Use `match` for pattern matching." },
        .{ "switch", "`switch` is not a keyword in Roc. Use `match` for pattern matching." },
        .{ "when", "`when` is not a keyword in Roc. Use `match` for pattern matching." },
        .{ "debug", "`debug` is not a keyword in Roc. Use `dbg` for debug printing." },
        .{ "then", "`then` is not a keyword in Roc. You can put the first branch of an `if` immediately after the condition, e.g. `if (condition) then_branch else else_branch`" },
        .{ "elif", "Roc uses `else if` for chaining conditions, not `elif`." },
        .{ "elseif", "Roc uses `else if` (two words) for chaining conditions." },
        .{ "const", "`const` is not a keyword in Roc. Use direct assignment like `name = value` to declare constants, and `var name = value` to declare reassignable variables." },
        .{ "let", "`let` is not a keyword in Roc. Use direct assignment like `name = value` to declare constants, and `var name = value` to declare reassignable variables." },
        .{ "def", "`def` is not a keyword in Roc. Use direct assignment like `name = value` to declare constants, and `var name = value` to declare reassignable variables." },
        .{ "mut", "`mut` is not a keyword in Roc. Use direct assignment like `name = value` to declare constants, and `var name = value` to declare reassignable variables." },
        .{ "function", "`function` is not a keyword in Roc. Use `fn_name = |arg1, arg2| body` to define functions." },
        .{ "fn", "`fn` is not a keyword in Roc. Use `fn_name = |arg1, arg2| body` to define functions." },
        .{ "fun", "`fun` is not a keyword in Roc. Use `fn_name = |arg1, arg2| body` to define functions." },
        .{ "fn", "`fn` is not a keyword in Roc. Use `fn_name = |arg1, arg2| body` to define functions." },
        .{ "is", "`is` is not a keyword in Roc." },
        .{ "lambda", "`lambda` is not a keyword in Roc. Use `fn_name = |arg1, arg2| body` to define functions." },
        .{ "class", "Roc doesn't have a `class` keyword, but it does have nominal types. Docs for nominal types can be found at <https://www.roc-lang.org/docs>" },
        .{ "trait", "Roc doesn't have a `trait` keyword, but it does have static dispatch. Docs for nominal types can be found at <https://www.roc-lang.org/docs>" },
        .{ "impl", "Roc doesn't have an `impl` keyword, but it does have static dispatch. Docs for nominal types can be found at <https://www.roc-lang.org/docs>" },
        .{ "struct", "Roc doesn't have a `struct` keyword, but it does have nominal types—including nominal record types. Docs for nominal types can be found at <https://www.roc-lang.org/docs>" },
        .{ "interface", "Roc doesn't have an `interface` keyword, but it does have static dispatch. Docs for nominal types can be found at <https://www.roc-lang.org/docs>" },
        .{ "self", "Roc doesn't have a special `self` keyword. No function argument names are special, so you can choose to name one of them \"self\" if desired, but it won't do anything special." },
        .{ "this", "Roc doesn't have a special `this` keyword. No function argument names are special, so you can choose to name one of them \"this\" if desired, but it won't do anything special." },
        .{ "null", "Roc doesn't have `null` (or `nil` or `undefined` or anything like that). You can use the `Try` type to represent operations that can fail, or tag unions like `[Some(val), None]` to represent optional values." },
        .{ "Null", "Roc doesn't have `Null` (or `nil` or `undefined` or anything like that). You can use the `Try` type to represent operations that can fail, or tag unions like `[Some(val), None]` to represent optional values." },
        .{ "nil", "Roc doesn't have `nil` (or `undefined` or anything like that). You can use the `Try` type to represent operations that can fail, or tag unions like `[Some(val), None]` to represent optional values." },
        .{ "Nil", "Roc doesn't have `Nil` (or `null` or `undefined` or anything like that). You can use the `Try` type to represent operations that can fail, or tag unions like `[Some(val), None]` to represent optional values." },
        .{ "void", "Roc doesn't have a `void` type. Functions that don't return a value can return `{}` (the empty record type)." },
        .{ "async", "Roc doesn't have `async` or `await` keywords. Any given effectful function may be run synchronously or asynchronously by the platform, but either way it's typed as an effectful function and called normally (so, no `await` either)." },
        .{ "await", "Roc doesn't have `async` or `await` keywords. Any given effectful function may be run synchronously or asynchronously by the platform, but either way it's typed as an effectful function and called normally (so, no `await` either)." },
        .{ "yield", "Roc doesn't have generators or a `yield` keyword." },

        // Common type names from other languages
        .{ "int", "Roc has sized integer types like `I64`, `U32`, etc. There's no generic `int` type." },
        .{ "Int", "Roc has sized integer types like `I64`, `U32`, etc. There's no generic `Int` type." },
        .{ "integer", "Roc has sized integer types like `I64`, `U32`, etc. There's no generic `integer` type." },
        .{ "Integer", "Roc has sized integer types like `I64`, `U32`, etc. There's no generic `Integer` type." },
        .{ "float", "Roc has `F32` and `F64` for floating-point numbers, not `float`." },
        .{ "Float", "Roc has `F32` and `F64` for floating-point numbers, not `Float`." },
        .{ "double", "Roc uses `F64` for 64-bit floating-point numbers, not `double`." },
        .{ "Double", "Roc uses `F64` for 64-bit floating-point numbers, not `Double`." },
        .{ "bool", "Roc uses `Bool` (capitalized) for boolean types." },
        .{ "boolean", "Roc uses `Bool` (capitalized) for boolean types." },
        .{ "string", "Roc uses `Str` for string types, not `string`." },
        .{ "array", "Roc uses `List` for dynamic arrays, not `array`." },
        .{ "Array", "Roc uses `List` for dynamic arrays, not `Array`." },
        .{ "vector", "Roc uses `List` for sequential collections, not `vector`." },
        .{ "Vec", "Roc uses `List` for sequential collections, not `Vec`." },
        .{ "map", "Roc uses `Dict` for key-value mappings, not `map`. Also, many Roc types have a method named `map`, but that would be called using `my_val.map(...)` syntax, not `map` as a standalone function." },
        .{ "fold_left", "Roc uses the names `fold` and `fold_rev`, not `fold_left` and `fold_right`." },
        .{ "foldLeft", "Roc uses the names `fold` and `fold_rev`, not `foldLeft` and `foldRight`." },
        .{ "foldl", "Roc uses the names `fold` and `fold_rev`, not `foldl` and `foldr`." },
        .{ "foldr", "Roc uses the names `fold` and `fold_rev`, not `foldl` and `foldr`." },
        .{ "reduce", "Roc uses the names `fold` and `fold_rev`, not `reduce` and `reduce_right`." },
        .{ "reduce_right", "Roc uses the names `fold` and `fold_rev`, not `reduce` and `reduce_right`." },
        .{ "reduceRight", "Roc uses the names `fold` and `fold_rev`, not `reduce` and `reduceRight`." },
        .{ "hash", "Roc uses `Dict` for key-value mappings." },
        .{ "hashmap", "Roc uses `Dict` for key-value mappings." },
        .{ "HashMap", "Roc uses `Dict` for key-value mappings." },
        .{ "dict", "Roc uses `Dict` (capitalized) for key-value mappings." },
        .{ "set", "Roc uses `Set` (capitalized) for unique collections." },
        .{ "tuple", "Roc doesn't have a `tuple` type name. You can define tuples using parentheses, e.g. `my_tuple = (val1, val2)`" },
    });

    /// Look up a token misspelling and return the associated tip, if any.
    pub fn getTokenTip(token: []const u8) ?[]const u8 {
        return tokens.get(token);
    }

    /// Look up an identifier misspelling and return the associated tip, if any.
    pub fn getIdentifierTip(identifier: []const u8) ?[]const u8 {
        return identifiers.get(identifier);
    }
};

test "token misspellings lookup" {
    const tip = CommonMisspellings.getTokenTip("&&");
    try std.testing.expect(tip != null);
    try std.testing.expectEqualStrings(
        "Roc uses the keyword `and` instead of `&&`, and the keyword `or` instead of `||`.",
        tip.?,
    );
}

test "identifier misspellings lookup" {
    const tip = CommonMisspellings.getIdentifierTip("case");
    try std.testing.expect(tip != null);
    try std.testing.expectEqualStrings(
        "`case` is not a keyword in Roc. Use `match` for pattern matching.",
        tip.?,
    );
}

test "non-existent misspelling returns null" {
    try std.testing.expect(CommonMisspellings.getTokenTip("validtoken") == null);
    try std.testing.expect(CommonMisspellings.getIdentifierTip("validident") == null);
}
