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
EXPECTED DEPENDENCY NAME - fuzz_crash_022.md:1:1:1:4
UNEXPECTED TYPE SYNTAX - fuzz_crash_022.md:1:19:1:27
UNEXPECTED STATEMENT - fuzz_crash_022.md:1:28:1:29
UNEXPECTED STATEMENT - fuzz_crash_022.md:1:29:1:30
UNEXPECTED STATEMENT - fuzz_crash_022.md:1:30:1:31
UNEXPECTED STATEMENT - fuzz_crash_022.md:1:32:1:33
UNEXPECTED EXPRESSION SYNTAX - fuzz_crash_022.md:6:27:6:28
UNEXPECTED EXPRESSION SYNTAX - fuzz_crash_022.md:6:35:6:39
UNEXPECTED EXPRESSION SYNTAX - fuzz_crash_022.md:8:7:8:8
EXPECTED TUPLE SEPARATOR - fuzz_crash_022.md:9:1:9:1
UNEXPECTED EXPRESSION SYNTAX - fuzz_crash_022.md:9:1:9:1
MALFORMED TYPE - fuzz_crash_022.md:1:19:1:27
INVALID IF CONDITION - :0:0:0:0
UNUSED VARIABLE - fuzz_crash_022.md:6:12:6:14
DECLARATION HAS NO VALUE - fuzz_crash_022.md:1:16:1:27
DECLARATION HAS NO VALUE - fuzz_crash_022.md:5:1:5:20
# PROBLEMS

── ✗ expected dependency name ──────────────────────────── fuzz_crash_022.md:1:1

I was parsing an app dependency record, and I expected a lowercase field name.

app [main!] { |f: platform "c" }
^^^

Each package or platform entry starts with a lowercase field name, followed by
`:` and a string path or `platform` path.

For example:
    pf: platform "../platform/main.roc"

I found `app` here.
That word is reserved by Roc, so it cannot be used as a name in this position.
(fuzz_crash_022.md:1:1):

── ✗ unexpected type syntax ───────────────────────────── fuzz_crash_022.md:1:19

I was parsing a type annotation, and this token cannot start a type here.

app [main!] { |f: platform "c" }
                  ^^^^^^^^

Types can be type variables, uppercase type names, function types, tuples,
records, or tag unions.

For example:
    List(U64)

I found `platform` here.
That word is reserved by Roc, so it cannot be used as a name in this position.
(fuzz_crash_022.md:1:19):

── ✗ unexpected statement ─────────────────────────────── fuzz_crash_022.md:1:28

I was parsing a statement, and this token cannot start a statement here.

app [main!] { |f: platform "c" }
                           ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found `"` here. (fuzz_crash_022.md:1:28):

── ✗ unexpected statement ─────────────────────────────── fuzz_crash_022.md:1:29

I was parsing a statement, and this token cannot start a statement here.

app [main!] { |f: platform "c" }
                            ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found `c` here. (fuzz_crash_022.md:1:29):

── ✗ unexpected statement ─────────────────────────────── fuzz_crash_022.md:1:30

I was parsing a statement, and this token cannot start a statement here.

app [main!] { |f: platform "c" }
                             ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found `"` here. (fuzz_crash_022.md:1:30):

── ✗ unexpected statement ─────────────────────────────── fuzz_crash_022.md:1:32

I was parsing a statement, and this token cannot start a statement here.

app [main!] { |f: platform "c" }
                               ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found `}` here.
This closes the current construct, so the parser was looking for the missing
item before it. (fuzz_crash_022.md:1:32):

── ✗ unexpected expression syntax ─────────────────────── fuzz_crash_022.md:6:27

I was parsing an expression, and this token cannot start an expression here.

getUser = |id| if (id > 1!) "big" else "l"
                          ^

Expressions can be names, literals, tags, records, lists, tuples, lambdas,
blocks, conditionals, matches, or function calls.

For example:
    add(1, 2)

I found `)` here.
This closes the current construct, so the parser was looking for the missing
item before it. (fuzz_crash_022.md:6:27):

── ✗ unexpected expression syntax ─────────────────────── fuzz_crash_022.md:6:35

I was parsing an expression, and this token cannot start an expression here.

getUser = |id| if (id > 1!) "big" else "l"
                                  ^^^^

Expressions can be names, literals, tags, records, lists, tuples, lambdas,
blocks, conditionals, matches, or function calls.

For example:
    add(1, 2)

I found `else` here.
That word is reserved by Roc, so it cannot be used as a name in this position.
(fuzz_crash_022.md:6:35):

── ✗ unexpected expression syntax ──────────────────────── fuzz_crash_022.md:8:7

I was parsing an expression, and this token cannot start an expression here.

-ain! = |_| getUser(900)
      ^

Expressions can be names, literals, tags, records, lists, tuples, lambdas,
blocks, conditionals, matches, or function calls.

For example:
    add(1, 2)

I found `=` here. (fuzz_crash_022.md:8:7):

── ✗ expected tuple separator ──────────────────────────── fuzz_crash_022.md:9:1

I was parsing a parenthesized expression or tuple, and I expected `,` or `)`.


^

Separate tuple elements with commas and close the tuple or parenthesized
expression with `)`.

For example:
    (x, y)

I reached the end of the file before this construct was complete.
(fuzz_crash_022.md:9:1):

── ✗ unexpected expression syntax ──────────────────────── fuzz_crash_022.md:9:1

I was parsing an expression, and this token cannot start an expression here.


^

Expressions can be names, literals, tags, records, lists, tuples, lambdas,
blocks, conditionals, matches, or function calls.

For example:
    add(1, 2)

I reached the end of the file before this construct was complete.
(fuzz_crash_022.md:9:1):

── ✗ malformed type ───────────────────────────────────── fuzz_crash_022.md:1:19

This type annotation is malformed or contains invalid syntax.

app [main!] { |f: platform "c" }
                  ^^^^^^^^

(fuzz_crash_022.md:1:19):
INVALID IF CONDITION

The condition in this `if` expression could not be processed.
The condition must be a valid expression that evaluates to a `Bool` value (`Bool.true` or `Bool.false`).

── ● unused variable ──────────────────────────────────── fuzz_crash_022.md:6:12

Variable `id` is defined here and then never used:

getUser = |id| if (id > 1!) "big" else "l"
           ^^

If you don't need this variable, prefix it with an underscore like `_id` to
suppress this warning. (fuzz_crash_022.md:6:12):

── ● declaration has no value ─────────────────────────── fuzz_crash_022.md:1:16

This declaration has a type annotation but no implementation.

app [main!] { |f: platform "c" }
               ^^^^^^^^^^^

(fuzz_crash_022.md:1:16):


Add a value body here, or put hosted functions in a platform type mod so
they are published through the host boundary.

── ● declaration has no value ──────────────────────────── fuzz_crash_022.md:5:1

This declaration has a type annotation but no implementation.

ser : UserId -> Str
^^^^^^^^^^^^^^^^^^^

(fuzz_crash_022.md:5:1):


Add a value body here, or put hosted functions in a platform type mod so
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
		(e-runtime-error (tag "erroneous_value_expr")))
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
