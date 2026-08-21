# META
~~~ini
description=Bare `_` stays rejected in expression position outside a record field value
type=snippet
~~~
# SOURCE
~~~roc
x = _

y = { a: _ + 1 }
~~~
# EXPECTED
UNEXPECTED EXPRESSION SYNTAX - record_unset_rejected_elsewhere.md:1:5:1:6
UNEXPECTED EXPRESSION SYNTAX - record_unset_rejected_elsewhere.md:3:10:3:11
UNRECOGNIZED SYNTAX - record_unset_rejected_elsewhere.md:1:5:1:6
# PROBLEMS
── ✗ unexpected expression syntax ─────── record_unset_rejected_elsewhere.md:1:5

I was parsing an expression, and this token cannot start an expression here.

x = _
    ^

Expressions can be names, literals, tags, records, lists, tuples, lambdas,
blocks, conditionals, matches, or function calls.

For example:
    add(1, 2)

I found _ here.

── ✗ unexpected expression syntax ────── record_unset_rejected_elsewhere.md:3:10

I was parsing an expression, and this token cannot start an expression here.

y = { a: _ + 1 }
         ^

Expressions can be names, literals, tags, records, lists, tuples, lambdas,
blocks, conditionals, matches, or function calls.

For example:
    add(1, 2)

I found _ here.

── ✗ unrecognized syntax ──────────────── record_unset_rejected_elsewhere.md:1:5

I don't recognize this syntax.

x = _
    ^

This might be a syntax error, an unsupported language feature, or a typo.

# TOKENS
~~~zig
LowerIdent,OpAssign,Underscore,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,Underscore,OpPlus,Int,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-malformed (reason "expr_unexpected_token")))
		(s-decl
			(p-ident (raw "y"))
			(e-record
				(field (field "a")
					(e-binop (op "+")
						(e-malformed (reason "expr_unexpected_token"))
						(e-int (raw "1"))))))))
~~~
# FORMATTED
~~~roc
x =

y = { a:  + 1 }
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-runtime-error (tag "expr_not_canonicalized")))
	(d-let
		(p-assign (ident "y"))
		(e-record
			(fields))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "{}")))
	(expressions
		(expr (type "Error"))
		(expr (type "{}"))))
~~~
