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
OpBar(1:1-1:2),LowerIdent(1:2-1:3),OpBar(1:3-1:4),OpBar(1:5-1:6),LowerIdent(1:6-1:7),OpBar(1:7-1:8),LowerIdent(1:9-1:10),OpPlus(1:11-1:12),LowerIdent(1:13-1:14),EndOfFile(1:14-1:14),
~~~
# PARSE
~~~clojure
(e-lambda @1.1-1.14
	(args
		(p-ident @1.2-1.3 (raw "x")))
	(e-lambda @1.5-1.14
		(args
			(p-ident @1.6-1.7 (raw "y")))
		(e-binop @1.9-1.14 (op "+")
			(e-ident @1.9-1.10 (raw "x"))
			(e-ident @1.13-1.14 (raw "z")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-lambda @1.1-1.14
	(args
		(p-assign @1.2-1.3 (ident "x")))
	(e-closure @1.5-1.14
		(captures
			(capture @1.2-1.3 (ident "x")))
		(e-lambda @1.5-1.14
			(args
				(p-assign @1.6-1.7 (ident "y")))
			(e-binop @1.9-1.14 (op "add")
				(e-lookup-local @1.9-1.10
					(p-assign @1.2-1.3 (ident "x")))
				(e-runtime-error (tag "ident_not_in_scope"))))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.14 (type "_arg -> _arg2 -> Num(_size)"))
~~~
