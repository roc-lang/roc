# META
~~~ini
description="A lambda argument shadows a variable from an outer scope. The lambda should use the argument, not the captured variable."
type=expr
~~~
# SOURCE
~~~roc
{
    x = 5
    (|x| x)(10) # Should not capture outer `x` -- this should give a shadowing warning
}
~~~
# EXPECTED
DUPLICATE DEFINITION - argument_shadows_capture.md:3:7:3:8
UNUSED VARIABLE - argument_shadows_capture.md:2:5:2:6
# PROBLEMS
**DUPLICATE DEFINITION**
The name `x` is being redeclared in this scope.

The redeclaration is here:
**argument_shadows_capture.md:3:7:3:8:**
```roc
    (|x| x)(10) # Should not capture outer `x` -- this should give a shadowing warning
```
      ^

But `x` was already defined here:
**argument_shadows_capture.md:2:5:2:6:**
```roc
    x = 5
```
    ^


**UNUSED VARIABLE**
Variable `x` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:
**argument_shadows_capture.md:2:5:2:6:**
```roc
    x = 5
```
    ^


# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpAssign,Int,
OpenRound,OpBar,LowerIdent,OpBar,LowerIdent,CloseRound,NoSpaceOpenRound,Int,CloseRound,
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
		(e-apply
			(e-tuple
				(e-lambda
					(args
						(p-ident (raw "x")))
					(e-ident (raw "x"))))
			(e-int (raw "10")))))
~~~
# FORMATTED
~~~roc
{
	x = 5
	(|x| x)(10) # Should not capture outer `x` -- this should give a shadowing warning
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "x"))
		(e-num (value "5")))
	(e-call
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-lookup-local
				(p-assign (ident "x"))))
		(e-num (value "10"))))
~~~
# TYPES
~~~clojure
(expr (type "Num(_size)"))
~~~
