# META
~~~ini
description=fuzz crash
type=snippet
~~~
# SOURCE
~~~roc
#el
var t= ]

#el
var t= 0
~~~
# EXPECTED
VAR OUTSIDE BODY - fuzz_crash_024.md:2:1:2:4
UNEXPECTED EXPRESSION SYNTAX - fuzz_crash_024.md:2:8:2:9
VAR OUTSIDE BODY - fuzz_crash_024.md:5:1:5:4
UNRECOGNIZED SYNTAX - fuzz_crash_024.md:2:8:2:9
DUPLICATE DEFINITION - fuzz_crash_024.md:5:5:5:6
# PROBLEMS
── ✗ var outside body ──────────────────────────────────── fuzz_crash_024.md:2:1

I was parsing a statement, and `var` appeared outside a function or block body.

var t= ]
^^^

Mutable variables are local body statements. Move this var into a body, or use
an ordinary top-level declaration.

For example:
    main = {
        var count = 0
        count
    }

I found var here.
That word is reserved by Roc, so it cannot be used as a name in this position.

── ✗ unexpected expression syntax ──────────────────────── fuzz_crash_024.md:2:8

I was parsing an expression, and this token cannot start an expression here.

var t= ]
       ^

Expressions can be names, literals, tags, records, lists, tuples, lambdas,
blocks, conditionals, matches, or function calls.

For example:
    add(1, 2)

I found ] here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ var outside body ──────────────────────────────────── fuzz_crash_024.md:5:1

I was parsing a statement, and `var` appeared outside a function or block body.

var t= 0
^^^

Mutable variables are local body statements. Move this var into a body, or use
an ordinary top-level declaration.

For example:
    main = {
        var count = 0
        count
    }

I found var here.
That word is reserved by Roc, so it cannot be used as a name in this position.

── ✗ unrecognized syntax ───────────────────────────────── fuzz_crash_024.md:2:8

I don't recognize this syntax.

var t= ]
       ^

This might be a syntax error, an unsupported language feature, or a typo.

── ● duplicate definition ──────────────────────────────── fuzz_crash_024.md:5:5

The name t is being redeclared here:

var t= 0
    ^

In this scope, t was already defined in fuzz_crash_024.md:2:5:

var t= ]
    ^

# TOKENS
~~~zig
KwVar,LowerIdent,OpAssign,CloseSquare,
KwVar,LowerIdent,OpAssign,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-malformed (tag "var_only_allowed_in_a_body"))
		(s-decl
			(p-ident (raw "t"))
			(e-malformed (reason "expr_unexpected_token")))
		(s-malformed (tag "var_only_allowed_in_a_body"))
		(s-decl
			(p-ident (raw "t"))
			(e-int (raw "0")))))
~~~
# FORMATTED
~~~roc
# el
t =

# el
t = 0
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "t"))
		(e-runtime-error (tag "expr_not_canonicalized")))
	(d-let
		(p-assign (ident "t"))
		(e-num (value "0"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "Dec")))
	(expressions
		(expr (type "Error"))
		(expr (type "Dec"))))
~~~
