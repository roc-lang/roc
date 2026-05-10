# META
~~~ini
description=Error - main! with wrong number of parameters
type=file
~~~
# SOURCE
~~~roc
main! = |arg1, arg2| {
    arg1
}
~~~
# EXPECTED
UNUSED VARIABLE - default_app_wrong_arity.md:1:16:1:20
# PROBLEMS
**UNUSED VARIABLE**
Variable `arg2` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_arg2` to suppress this warning.
The unused variable is declared here:
**default_app_wrong_arity.md:1:16:1:20:**
```roc
main! = |arg1, arg2| {
```
               ^^^^


# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-ident (raw "arg1"))
					(p-ident (raw "arg2")))
				(e-block
					(statements
						(e-ident (raw "arg1"))))))))
~~~
# FORMATTED
~~~roc
main! = |arg1, arg2| {
	arg1
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-assign (ident "arg1"))
				(p-assign (ident "arg2")))
			(e-block
				(e-lookup-local
					(p-assign (ident "arg1")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a, _arg -> a")))
	(expressions
		(expr (type "a, _arg -> a"))))
~~~
