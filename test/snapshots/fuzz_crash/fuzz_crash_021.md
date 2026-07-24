# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
Fli/main.roc" }

Pair(a, b+ : (
~~~
# EXPECTED
UNCLOSED STRING - fuzz_crash_021.md:1:13:1:16
TYPE APPLICATION NEEDS PARENTHESES - fuzz_crash_021.md:1:4:1:5
UNEXPECTED STATEMENT - fuzz_crash_021.md:1:5:1:9
UNEXPECTED STATEMENT - fuzz_crash_021.md:1:9:1:13
UNEXPECTED STATEMENT - fuzz_crash_021.md:1:13:1:14
UNEXPECTED STATEMENT - fuzz_crash_021.md:1:14:1:16
UNEXPECTED STATEMENT - fuzz_crash_021.md:1:16:1:16
EXPECTED TYPE SEPARATOR - fuzz_crash_021.md:3:1:3:5
# PROBLEMS

┌─────────────────┐
│ UNCLOSED STRING ├─ This string is missing a closing quote. ─────────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  Fli/main.roc" }                                                           │
 │              ‾‾‾                                                           │
 └──────────────────────────────────────────────────── fuzz_crash_021.md:1:13 ┘



┌────────────────────────────────────┐
│ TYPE APPLICATION NEEDS PARENTHESES ├─ I was parsing a type annotation, ─────┐
└┬───────────────────────────────────┘  and I found a type argument without   │
 │                                      parentheses.                          │
 │                                                                            │
 │  Fli/main.roc" }                                                           │
 │     ‾                                                                      │
 └───────────────────────────────────────────────────── fuzz_crash_021.md:1:4 ┘

    Roc type applications use parentheses around their arguments. Write
    `List(U8)`, not `List U8`.

    For example:
        List(U8)

    I found `/` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  Fli/main.roc" }                                                           │
 │      ‾‾‾‾                                                                  │
 └───────────────────────────────────────────────────── fuzz_crash_021.md:1:5 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `main` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  Fli/main.roc" }                                                           │
 │          ‾‾‾‾                                                              │
 └───────────────────────────────────────────────────── fuzz_crash_021.md:1:9 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `.roc` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  Fli/main.roc" }                                                           │
 │              ‾                                                             │
 └──────────────────────────────────────────────────── fuzz_crash_021.md:1:13 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `"` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  Fli/main.roc" }                                                           │
 │               ‾‾                                                           │
 └──────────────────────────────────────────────────── fuzz_crash_021.md:1:14 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found ` }` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  Fli/main.roc" }                                                           │
 │                 ‾                                                          │
 └──────────────────────────────────────────────────── fuzz_crash_021.md:1:16 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I reached the end of the file before this construct was complete.


┌─────────────────────────┐
│ EXPECTED TYPE SEPARATOR ├─ I was parsing type parameters, and I expected ───┐
└┬────────────────────────┘  `,` or `)`.                                      │
 │                                                                            │
 │  Pair(a, b+ : (                                                            │
 │  ‾‾‾‾                                                                      │
 └───────────────────────────────────────────────────── fuzz_crash_021.md:3:1 ┘

    Separate type parameters with commas and close the parameter list with `)`.

    For example:
        Result(ok, err)

    I found `Pair` here.
    Names that start with uppercase letters are used for tags, type names, and
    mod names in Roc.

# TOKENS
~~~zig
UpperIdent,OpSlash,LowerIdent,NoSpaceDotLowerIdent,StringStart,StringPart,StringEnd,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,OpPlus,OpColon,OpenRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_ty_anno_close_round_or_comma"))))
~~~
# FORMATTED
~~~roc


~~~
# CANONICALIZE
~~~clojure
(can-ir (empty true))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
