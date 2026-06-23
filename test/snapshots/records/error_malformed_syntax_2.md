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
UNEXPECTED TOKEN IN TYPE ANNOTATION - error_malformed_syntax_2.md:1:8:1:10
UNEXPECTED TOKEN IN EXPRESSION - error_malformed_syntax_2.md:1:10:1:11
DECLARATION HAS NO VALUE - error_malformed_syntax_2.md:1:3:1:10
# PROBLEMS

┌─────────────────────────────────────┐
│ UNEXPECTED TOKEN IN TYPE ANNOTATION ├─ The token 42 is not expected in a ───┐
└┬────────────────────────────────────┘  type annotation.                     │
 │                                                                            │
 │  { age: 42, name = "Alice" }                                               │
 │         ‾‾                                                                 │
 └─────────────────────────────────────────── error_malformed_syntax_2.md:1:8 ┘

    Type annotations should contain types like Str, Num a, or List U64.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token , is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  { age: 42, name = "Alice" }                                               │
 │           ‾                                                                │
 └────────────────────────────────────────── error_malformed_syntax_2.md:1:10 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  { age: 42, name = "Alice" }                                               │
 │    ‾‾‾‾‾‾‾                                                                 │
 └─────────────────────────────────────────── error_malformed_syntax_2.md:1:3 ┘

    Add a value body here, or put hosted functions in a platform type module so
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
