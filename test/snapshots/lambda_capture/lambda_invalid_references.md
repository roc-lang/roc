# META
~~~ini
description=Error handling for invalid variable references in lambda captures
type=expr
~~~
# SOURCE
~~~roc
|x| |y| x + z
~~~
# EXPECTED
UNDEFINED VARIABLE - lambda_invalid_references.md:1:13:1:14
UNUSED VARIABLE - lambda_invalid_references.md:1:6:1:7
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `z` in this scope.
Is there an `import` or `exposing` missing up-top?

**lambda_invalid_references.md:1:13:1:14:**
```roc
|x| |y| x + z
```
            ^


**UNUSED VARIABLE**
Variable `y` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_y` to suppress this warning.
The unused variable is declared here:
**lambda_invalid_references.md:1:6:1:7:**
```roc
|x| |y| x + z
```
     ^


# TOKENS
~~~zig
OpBar,LowerIdent,OpBar,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-lambda
	(args
		(p-ident (raw "x")))
	(e-lambda
		(args
			(p-ident (raw "y")))
		(e-binop (op "+")
			(e-ident (raw "x"))
			(e-ident (raw "z")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-lambda
	(args
		(p-assign (ident "x")))
	(e-closure
		(captures
			(capture (ident "x")))
		(e-lambda
			(args
				(p-assign (ident "y")))
			(e-binop (op "add")
				(e-lookup-local
					(p-assign (ident "x")))
				(e-runtime-error (tag "ident_not_in_scope"))))))
~~~
# TYPES
~~~clojure
(expr (type "Error -> _arg -> Error"))
~~~
