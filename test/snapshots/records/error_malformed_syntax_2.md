# META
~~~ini
description=Malformed record syntax using equals instead of colon (error case)
type=expr
~~~
# SOURCE
~~~roc
{ age: 42, name = "Alice" }
~~~
# EXPECTED
UNEXPECTED TYPE SYNTAX - error_malformed_syntax_2.md:1:8:1:10
UNEXPECTED EXPRESSION SYNTAX - error_malformed_syntax_2.md:1:10:1:11
DECLARATION HAS NO VALUE - error_malformed_syntax_2.md:1:3:1:10
# PROBLEMS

┌────────────────────────┐
│ UNEXPECTED TYPE SYNTAX ├─ I was parsing a type annotation, and this token ──┐
└┬───────────────────────┘  cannot start a type here.                         │
 │                                                                            │
 │  { age: 42, name = "Alice" }                                               │
 │         ‾‾                                                                 │
 └─────────────────────────────────────────── error_malformed_syntax_2.md:1:8 ┘

    Types can be type variables, uppercase type names, function types, tuples,
    records, or tag unions.

    For example:
        List(U64)

    I found `42` here.


┌──────────────────────────────┐
│ UNEXPECTED EXPRESSION SYNTAX ├─ I was parsing an expression, and this ──────┐
└┬─────────────────────────────┘  token cannot start an expression here.      │
 │                                                                            │
 │  { age: 42, name = "Alice" }                                               │
 │           ‾                                                                │
 └────────────────────────────────────────── error_malformed_syntax_2.md:1:10 ┘

    Expressions can be names, literals, tags, records, lists, tuples, lambdas,
    blocks, conditionals, matches, or function calls.

    For example:
        add(1, 2)

    I found `,` here.
    A comma separates items, but there must be a valid item on both sides of it.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  { age: 42, name = "Alice" }                                               │
 │    ‾‾‾‾‾‾‾                                                                 │
 └─────────────────────────────────────────── error_malformed_syntax_2.md:1:3 ┘

    Add a value body here, or put hosted functions in a platform type mod so
    they are published through the host boundary.

# TOKENS
~~~zig
OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpAssign,StringStart,StringPart,StringEnd,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-type-anno (name "age")
			(ty-malformed (tag "ty_anno_unexpected_token")))
		(e-malformed (reason "expr_unexpected_token"))
		(s-decl
			(p-ident (raw "name"))
			(e-string
				(e-string-part (raw "Alice"))))))
~~~
# FORMATTED
~~~roc
{
	age : 
		name = "Alice"
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "age"))
		(e-anno-only))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-let
		(p-assign (ident "name"))
		(e-string
			(e-literal (string "Alice"))))
	(e-empty_record))
~~~
# TYPES
~~~clojure
(expr (type "{}"))
~~~
