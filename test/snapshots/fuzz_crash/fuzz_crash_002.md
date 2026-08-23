# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
modu:;::::::::::::::le[%
~~~
# EXPECTED
UNEXPECTED TYPE SYNTAX - fuzz_crash_002.md:1:6:1:7
UNEXPECTED STATEMENT - fuzz_crash_002.md:1:7:1:9
UNEXPECTED STATEMENT - fuzz_crash_002.md:1:9:1:11
UNEXPECTED STATEMENT - fuzz_crash_002.md:1:11:1:13
UNEXPECTED STATEMENT - fuzz_crash_002.md:1:13:1:15
UNEXPECTED STATEMENT - fuzz_crash_002.md:1:15:1:17
UNEXPECTED STATEMENT - fuzz_crash_002.md:1:17:1:19
UNEXPECTED STATEMENT - fuzz_crash_002.md:1:19:1:21
UNEXPECTED STATEMENT - fuzz_crash_002.md:1:21:1:23
UNEXPECTED STATEMENT - fuzz_crash_002.md:1:23:1:24
UNEXPECTED STATEMENT - fuzz_crash_002.md:1:24:1:25
MALFORMED TYPE - fuzz_crash_002.md:1:6:1:7
DECLARATION HAS NO VALUE - fuzz_crash_002.md:1:1:1:7
# PROBLEMS
── ✗ unexpected type syntax ────────────────────────────── fuzz_crash_002.md:1:6

I was parsing a type annotation, and this token cannot start a type here.

modu:;::::::::::::::le[%
     ^

Types can be type variables, uppercase type names, function types, tuples,
records, or tag unions.

For example:
    List(U64)

I found ; here.
This token is malformed, so it cannot be used as ordinary Roc syntax.

── ✗ unexpected statement ──────────────────────────────── fuzz_crash_002.md:1:7

I was parsing a statement, and this token cannot start a statement here.

modu:;::::::::::::::le[%
      ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found :: here.

── ✗ unexpected statement ──────────────────────────────── fuzz_crash_002.md:1:9

I was parsing a statement, and this token cannot start a statement here.

modu:;::::::::::::::le[%
        ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found :: here.

── ✗ unexpected statement ─────────────────────────────── fuzz_crash_002.md:1:11

I was parsing a statement, and this token cannot start a statement here.

modu:;::::::::::::::le[%
          ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found :: here.

── ✗ unexpected statement ─────────────────────────────── fuzz_crash_002.md:1:13

I was parsing a statement, and this token cannot start a statement here.

modu:;::::::::::::::le[%
            ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found :: here.

── ✗ unexpected statement ─────────────────────────────── fuzz_crash_002.md:1:15

I was parsing a statement, and this token cannot start a statement here.

modu:;::::::::::::::le[%
              ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found :: here.

── ✗ unexpected statement ─────────────────────────────── fuzz_crash_002.md:1:17

I was parsing a statement, and this token cannot start a statement here.

modu:;::::::::::::::le[%
                ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found :: here.

── ✗ unexpected statement ─────────────────────────────── fuzz_crash_002.md:1:19

I was parsing a statement, and this token cannot start a statement here.

modu:;::::::::::::::le[%
                  ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found :: here.

── ✗ unexpected statement ─────────────────────────────── fuzz_crash_002.md:1:21

I was parsing a statement, and this token cannot start a statement here.

modu:;::::::::::::::le[%
                    ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found le here.
Names that start with lowercase letters are value names or record field names,
depending on the surrounding syntax.

── ✗ unexpected statement ─────────────────────────────── fuzz_crash_002.md:1:23

I was parsing a statement, and this token cannot start a statement here.

modu:;::::::::::::::le[%
                      ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found [ here.

── ✗ unexpected statement ─────────────────────────────── fuzz_crash_002.md:1:24

I was parsing a statement, and this token cannot start a statement here.

modu:;::::::::::::::le[%
                       ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found % here.

── ✗ malformed type ────────────────────────────────────── fuzz_crash_002.md:1:6

This type annotation is malformed or contains invalid syntax.

modu:;::::::::::::::le[%
     ^

── ● declaration has no value ──────────────────────────── fuzz_crash_002.md:1:1

This declaration has a type annotation but no implementation.

modu:;::::::::::::::le[%
^^^^^^

Add a value body here, or put hosted functions in a platform type mod so
they are published through the host boundary.

# TOKENS
~~~zig
LowerIdent,OpColon,MalformedUnknownToken,OpDoubleColon,OpDoubleColon,OpDoubleColon,OpDoubleColon,OpDoubleColon,OpDoubleColon,OpDoubleColon,LowerIdent,OpenSquare,OpPercent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "modu")
			(ty-malformed (tag "ty_anno_unexpected_token")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
modu :
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "modu"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-malformed))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
