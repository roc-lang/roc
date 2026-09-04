# META
~~~ini
description=A value cycle that runs through a name bound by a top-level destructure is reported like any other circular value definition
type=file
~~~
# SOURCE
~~~roc
{a, b} = { a: c, b: 1 }
c = a
~~~
# EXPECTED
CIRCULAR VALUE DEFINITION - top_level_destructure_value_cycle_with_plain_def.md:1:2:1:3
CIRCULAR VALUE DEFINITION - top_level_destructure_value_cycle_with_plain_def.md:2:1:2:2
# PROBLEMS
── ✗ circular value definition ─ top_level_destructure_value_cycle_with_plain_def.md:1:2

The value a is part of a recursive non-function definition cycle.

{a, b} = { a: c, b: 1 }
 ^

Only functions can be recursive. Non-function top-level values must be fully
computable without depending on themselves through other values.

── ✗ circular value definition ─ top_level_destructure_value_cycle_with_plain_def.md:2:1

The value c is part of a recursive non-function definition cycle.

c = a
^

Only functions can be recursive. Non-function top-level values must be fully
computable without depending on themselves through other values.

# TOKENS
~~~zig
OpenCurly,LowerIdent,Comma,LowerIdent,CloseCurly,OpAssign,OpenCurly,LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpColon,Int,CloseCurly,
LowerIdent,OpAssign,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-record
				(field (name "a") (rest false))
				(field (name "b") (rest false)))
			(e-record
				(field (field "a")
					(e-ident (raw "c")))
				(field (field "b")
					(e-int (raw "1")))))
		(s-decl
			(p-ident (raw "c"))
			(e-ident (raw "a")))))
~~~
# FORMATTED
~~~roc
{ a, b } = { a: c, b: 1 }

c = a
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "a"))
		(e-runtime-error (tag "circular_value_definition")))
	(d-let
		(p-assign (ident "b"))
		(e-num (value "1")))
	(d-let
		(p-assign (ident "c"))
		(e-runtime-error (tag "circular_value_definition"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "Dec"))
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))
		(expr (type "Dec"))
		(expr (type "Error"))))
~~~
