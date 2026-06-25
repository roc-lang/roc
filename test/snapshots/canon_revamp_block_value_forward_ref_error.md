# META
~~~ini
description=Block-local value cannot be used before declaration
type=snippet
~~~
# SOURCE
~~~roc
x = {
    y + 1
    y = 5
}
~~~
# EXPECTED
UNDEFINED VARIABLE - canon_revamp_block_value_forward_ref_error.md:2:5:2:6
UNUSED VARIABLE - canon_revamp_block_value_forward_ref_error.md:3:5:3:6
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `y` in this scope.
Is there an `import` or `exposing` missing up-top?

**canon_revamp_block_value_forward_ref_error.md:2:5:2:6:**
```roc
    y + 1
```
    ^


**UNUSED VARIABLE**
Variable `y` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_y` to suppress this warning.
The unused variable is declared here:
**canon_revamp_block_value_forward_ref_error.md:3:5:3:6:**
```roc
    y = 5
```
    ^


# TOKENS
~~~zig
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpPlus,Int,
LowerIdent,OpAssign,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-block
				(statements
					(e-binop (op "+")
						(e-ident (raw "y"))
						(e-int (raw "1")))
					(s-decl
						(p-ident (raw "y"))
						(e-int (raw "5"))))))))
~~~
# FORMATTED
~~~roc
x = {
	y + 1
	y = 5
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-block
			(s-expr
				(e-dispatch-call (method "plus") (constraint-fn-var 52)
					(receiver
						(e-runtime-error (tag "ident_not_in_scope")))
					(args
						(e-num (value "1")))))
			(s-let
				(p-assign (ident "y"))
				(e-num (value "5")))
			(e-empty_record))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{}")))
	(expressions
		(expr (type "{}"))))
~~~
