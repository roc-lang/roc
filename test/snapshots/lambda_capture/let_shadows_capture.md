# META
~~~ini
description="A `let` binding inside a lambda's body shadows a would-be captured variable."
type=expr
~~~
# SOURCE
~~~roc
{
    x = 5
    y = (|_| { 
        x = 10
        x 
    })({}) # Inner `x` should be used; outer `x` is not captured (it should be a shadowing warning)
    y
}
~~~
# EXPECTED
DUPLICATE DEFINITION - let_shadows_capture.md:4:9:4:10
UNUSED VARIABLE - let_shadows_capture.md:2:5:2:6
# PROBLEMS
**DUPLICATE DEFINITION**
The name `x` is being redeclared in this scope.

The redeclaration is here:
**let_shadows_capture.md:4:9:4:10:**
```roc
        x = 10
```
        ^

But `x` was already defined here:
**let_shadows_capture.md:2:5:2:6:**
```roc
    x = 5
```
    ^


**UNUSED VARIABLE**
Variable `x` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:
**let_shadows_capture.md:2:5:2:6:**
```roc
    x = 5
```
    ^


# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,OpenRound,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,
CloseCurly,CloseRound,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-int (raw "5")))
		(s-decl
			(p-ident (raw "y"))
			(e-apply
				(e-tuple
					(e-lambda
						(args
							(p-underscore))
						(e-block
							(statements
								(s-decl
									(p-ident (raw "x"))
									(e-int (raw "10")))
								(e-ident (raw "x"))))))
				(e-record)))
		(e-ident (raw "y"))))
~~~
# FORMATTED
~~~roc
{
	x = 5
	y = (
		|_| {
			x = 10
			x
		},
	)({}) # Inner `x` should be used; outer `x` is not captured (it should be a shadowing warning)
	y
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "x"))
		(e-num (value "5")))
	(s-let
		(p-assign (ident "y"))
		(e-call
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(s-let
						(p-assign (ident "x"))
						(e-num (value "10")))
					(e-lookup-local
						(p-assign (ident "x")))))
			(e-empty_record)))
	(e-lookup-local
		(p-assign (ident "y"))))
~~~
# TYPES
~~~clojure
(expr (type "Num(_size)"))
~~~
