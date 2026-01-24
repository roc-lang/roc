# META
~~~ini
description=Test closure parameter shadowing outer variable
type=expr
~~~
# SOURCE
~~~roc
{
    outer = 1
    |outer| outer + 1
}
~~~
# EXPECTED
DUPLICATE DEFINITION - can_closure_shadowing.md:3:6:3:11
UNUSED VARIABLE - can_closure_shadowing.md:2:5:2:10
# PROBLEMS
**DUPLICATE DEFINITION**
The name `outer` is being redeclared in this scope.

The redeclaration is here:
**can_closure_shadowing.md:3:6:3:11:**
```roc
    |outer| outer + 1
```
     ^^^^^

But `outer` was already defined here:
**can_closure_shadowing.md:2:5:2:10:**
```roc
    outer = 1
```
    ^^^^^


**UNUSED VARIABLE**
Variable `outer` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_outer` to suppress this warning.
The unused variable is declared here:
**can_closure_shadowing.md:2:5:2:10:**
```roc
    outer = 1
```
    ^^^^^


# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpAssign,Int,
OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-decl
			(p-ident (raw "outer"))
			(e-int (raw "1")))
		(e-lambda
			(args
				(p-ident (raw "outer")))
			(e-binop (op "+")
				(e-ident (raw "outer"))
				(e-int (raw "1"))))))
~~~
# FORMATTED
~~~roc
{
	outer = 1
	|outer| outer + 1
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "outer"))
		(e-num (value "1")))
	(e-lambda
		(args
			(p-assign (ident "outer")))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "outer")))
			(e-num (value "1")))))
~~~
# TYPES
~~~clojure
(expr (type "a -> a where [a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
~~~
