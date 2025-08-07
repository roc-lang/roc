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
OpenCurly(1:1-1:2),
LowerIdent(2:5-2:6),OpAssign(2:7-2:8),Int(2:9-2:10),
OpenRound(3:5-3:6),OpBar(3:6-3:7),LowerIdent(3:7-3:8),OpBar(3:8-3:9),LowerIdent(3:10-3:11),CloseRound(3:11-3:12),NoSpaceOpenRound(3:12-3:13),Int(3:13-3:15),CloseRound(3:15-3:16),
CloseCurly(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-block @1.1-4.2
	(statements
		(s-decl @2.5-2.10
			(p-ident @2.5-2.6 (raw "x"))
			(e-int @2.9-2.10 (raw "5")))
		(e-apply @3.5-3.16
			(e-tuple @3.5-3.12
				(e-lambda @3.6-3.11
					(args
						(p-ident @3.7-3.8 (raw "x")))
					(e-ident @3.10-3.11 (raw "x"))))
			(e-int @3.13-3.15 (raw "10")))))
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
(e-block @1.1-4.2
	(s-let @2.5-2.10
		(p-assign @2.5-2.6 (ident "x"))
		(e-int @2.9-2.10 (value "5")))
	(e-call @3.5-3.16
		(e-lambda @3.6-3.11
			(args
				(p-assign @3.7-3.8 (ident "x")))
			(e-lookup-local @3.10-3.11
				(p-assign @3.7-3.8 (ident "x"))))
		(e-int @3.13-3.15 (value "10"))))
~~~
# TYPES
~~~clojure
(expr @1.1-4.2 (type "Num(_size)"))
~~~
