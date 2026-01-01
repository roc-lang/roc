# META
~~~ini
description=Var statement at top level
type=file
~~~
# SOURCE
~~~roc
var x = 1
~~~
# EXPECTED
PARSE ERROR - parse_error_var_top_level.md:1:1:1:4
MISSING MAIN! FUNCTION - parse_error_var_top_level.md:1:1:1:10
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `var_only_allowed_in_a_body`
This is an unexpected parsing error. Please check your syntax.

**parse_error_var_top_level.md:1:1:1:4:**
```roc
var x = 1
```
^^^


**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**parse_error_var_top_level.md:1:1:1:10:**
```roc
var x = 1
```
^^^^^^^^^


# TOKENS
~~~zig
KwVar,LowerIdent,OpAssign,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-malformed (tag "var_only_allowed_in_a_body"))
		(s-decl
			(p-ident (raw "x"))
			(e-int (raw "1")))))
~~~
# FORMATTED
~~~roc
x = 1
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-num (value "1"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))))
~~~
