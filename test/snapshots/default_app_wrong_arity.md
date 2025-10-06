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
MAIN! SHOULD TAKE 1 ARGUMENT - default_app_wrong_arity.md:1:1:3:2
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


**MAIN! SHOULD TAKE 1 ARGUMENT**
`main!` is defined but has the wrong number of arguments. `main!` should take 1 argument.

Found `2` arguments.

Change it to:
`main! = |arg| { ... }`
**default_app_wrong_arity.md:1:1:3:2:**
```roc
main! = |arg1, arg2| {
    arg1
}
```


# TOKENS
~~~zig
LowerIdent(1:1-1:6),OpAssign(1:7-1:8),OpBar(1:9-1:10),LowerIdent(1:10-1:14),Comma(1:14-1:15),LowerIdent(1:16-1:20),OpBar(1:20-1:21),OpenCurly(1:22-1:23),
LowerIdent(2:5-2:9),
CloseCurly(3:1-3:2),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.2
	(type-module @1.1-1.6)
	(statements
		(s-decl @1.1-3.2
			(p-ident @1.1-1.6 (raw "main!"))
			(e-lambda @1.9-3.2
				(args
					(p-ident @1.10-1.14 (raw "arg1"))
					(p-ident @1.16-1.20 (raw "arg2")))
				(e-block @1.22-3.2
					(statements
						(e-ident @2.5-2.9 (raw "arg1"))))))))
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
		(p-assign @1.1-1.6 (ident "main!"))
		(e-lambda @1.9-3.2
			(args
				(p-assign @1.10-1.14 (ident "arg1"))
				(p-assign @1.16-1.20 (ident "arg2")))
			(e-block @1.22-3.2
				(e-lookup-local @2.5-2.9
					(p-assign @1.10-1.14 (ident "arg1")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.1-1.6 (type "a, _arg -> a")))
	(expressions
		(expr @1.9-3.2 (type "a, _arg -> a"))))
~~~
