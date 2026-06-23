# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app [main!] { |f: platform "c" }

UserId : U64

ser : UserId -> Str
getUser = |id| if (id > 1!) "big" else "l"

-ain! = |_| getUser(900)
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_022.md:1:1:1:4
UNEXPECTED TOKEN IN TYPE ANNOTATION - fuzz_crash_022.md:1:19:1:27
PARSE ERROR - fuzz_crash_022.md:1:28:1:29
PARSE ERROR - fuzz_crash_022.md:1:29:1:30
PARSE ERROR - fuzz_crash_022.md:1:30:1:31
PARSE ERROR - fuzz_crash_022.md:1:32:1:33
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_022.md:6:27:6:28
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_022.md:6:35:6:39
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_022.md:8:7:8:8
PARSE ERROR - fuzz_crash_022.md:9:1:9:1
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_022.md:9:1:9:1
MALFORMED TYPE - fuzz_crash_022.md:1:19:1:27
INVALID IF CONDITION - :0:0:0:0
UNUSED VARIABLE - fuzz_crash_022.md:6:12:6:14
DECLARATION HAS NO VALUE - fuzz_crash_022.md:1:16:1:27
DECLARATION HAS NO VALUE - fuzz_crash_022.md:5:1:5:20
# PROBLEMS

┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: expected_package_or_platform_name ─┐
└┬────────────┘                                                               │
 │                                                                            │
 │  app [main!] { |f: platform "c" }                                          │
 │  ‾‾‾                                                                       │
 └───────────────────────────────────────────────────── fuzz_crash_022.md:1:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────────────────────────────┐
│ UNEXPECTED TOKEN IN TYPE ANNOTATION ├─ The token platform is not expected ──┐
└┬────────────────────────────────────┘  in a type annotation.                │
 │                                                                            │
 │  app [main!] { |f: platform "c" }                                          │
 │                    ‾‾‾‾‾‾‾‾                                                │
 └──────────────────────────────────────────────────── fuzz_crash_022.md:1:19 ┘

    Type annotations should contain types like Str, Num a, or List U64.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  app [main!] { |f: platform "c" }                                          │
 │                             ‾                                              │
 └──────────────────────────────────────────────────── fuzz_crash_022.md:1:28 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  app [main!] { |f: platform "c" }                                          │
 │                              ‾                                             │
 └──────────────────────────────────────────────────── fuzz_crash_022.md:1:29 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  app [main!] { |f: platform "c" }                                          │
 │                               ‾                                            │
 └──────────────────────────────────────────────────── fuzz_crash_022.md:1:30 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  app [main!] { |f: platform "c" }                                          │
 │                                 ‾                                          │
 └──────────────────────────────────────────────────── fuzz_crash_022.md:1:32 ┘

    This is an unexpected parsing error. Please check your syntax.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token ) is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  getUser = |id| if (id > 1!) "big" else "l"                                │
 │                            ‾                                               │
 └──────────────────────────────────────────────────── fuzz_crash_022.md:6:27 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token else is not expected in an ─────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  getUser = |id| if (id > 1!) "big" else "l"                                │
 │                                    ‾‾‾‾                                    │
 └──────────────────────────────────────────────────── fuzz_crash_022.md:6:35 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token = is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  -ain! = |_| getUser(900)                                                  │
 │        ‾                                                                   │
 └───────────────────────────────────────────────────── fuzz_crash_022.md:8:7 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: ───────────────────────────────────┐
└┬────────────┘  expected_expr_close_round_or_comma                           │
 │                                                                            │
 │                                                                            │
 │  ‾                                                                         │
 └───────────────────────────────────────────────────── fuzz_crash_022.md:9:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token  is not expected in an ─────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │                                                                            │
 │  ‾                                                                         │
 └───────────────────────────────────────────────────── fuzz_crash_022.md:9:1 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────┐
│ MALFORMED TYPE ├─ This type annotation is malformed or contains invalid ────┐
└┬───────────────┘  syntax.                                                   │
 │                                                                            │
 │  app [main!] { |f: platform "c" }                                          │
 │                    ‾‾‾‾‾‾‾‾                                                │
 └──────────────────────────────────────────────────── fuzz_crash_022.md:1:19 ┘



INVALID IF CONDITION

The condition in this `if` expression could not be processed.
The condition must be a valid expression that evaluates to a `Bool` value (`Bool.true` or `Bool.false`).


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `id` is defined here and then never used. ──────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  getUser = |id| if (id > 1!) "big" else "l"                                │
 │             ‾‾                                                             │
 └──────────────────────────────────────────────────── fuzz_crash_022.md:6:12 ┘

    If you don't need this variable, prefix it with an underscore like `_id` to
    suppress this warning.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  app [main!] { |f: platform "c" }                                          │
 │                 ‾‾‾‾‾‾‾‾‾‾‾                                                │
 └──────────────────────────────────────────────────── fuzz_crash_022.md:1:16 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  ser : UserId -> Str                                                       │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                       │
 └───────────────────────────────────────────────────── fuzz_crash_022.md:5:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.

# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,OpBar,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
UpperIdent,OpColon,UpperIdent,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwIf,OpenRound,LowerIdent,OpGreaterThan,Int,OpBang,CloseRound,StringStart,StringPart,StringEnd,KwElse,StringStart,StringPart,StringEnd,
OpUnaryMinus,LowerIdent,OpAssign,OpBar,Underscore,OpBar,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(malformed-header (tag "expected_package_or_platform_name"))
	(statements
		(s-type-anno (name "f")
			(ty-malformed (tag "ty_anno_unexpected_token")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-decl
			(header (name "UserId")
				(args))
			(ty (name "U64")))
		(s-type-anno (name "ser")
			(ty-fn
				(ty (name "UserId"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "getUser"))
			(e-lambda
				(args
					(p-ident (raw "id")))
				(e-if-without-else
					(e-malformed (reason "expected_expr_close_round_or_comma"))
					(e-malformed (reason "expr_unexpected_token")))))))
~~~
# FORMATTED
~~~roc
f : 


UserId : U64

ser : UserId -> Str

getUser = |id| if
	
	~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "f"))
		(e-anno-only)
		(annotation
			(ty-malformed)))
	(d-let
		(p-assign (ident "ser"))
		(e-anno-only)
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "UserId") (local))
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "getUser"))
		(e-lambda
			(args
				(p-assign (ident "id")))
			(e-runtime-error (tag "if_condition_not_canonicalized"))))
	(s-alias-decl
		(ty-header (name "UserId"))
		(ty-lookup (name "U64") (builtin))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "UserId -> Str"))
		(patt (type "_arg -> Error")))
	(type_decls
		(alias (type "UserId")
			(ty-header (name "UserId"))))
	(expressions
		(expr (type "Error"))
		(expr (type "UserId -> Str"))
		(expr (type "_arg -> Error"))))
~~~
