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
OpenCurly(1:1-1:2),
LowerIdent(2:5-2:6),OpAssign(2:7-2:8),Int(2:9-2:10),
LowerIdent(3:5-3:6),OpAssign(3:7-3:8),OpenRound(3:9-3:10),OpBar(3:10-3:11),Underscore(3:11-3:12),OpBar(3:12-3:13),OpenCurly(3:14-3:15),
LowerIdent(4:9-4:10),OpAssign(4:11-4:12),Int(4:13-4:15),
LowerIdent(5:9-5:10),
CloseCurly(6:5-6:6),CloseRound(6:6-6:7),NoSpaceOpenRound(6:7-6:8),OpenCurly(6:8-6:9),CloseCurly(6:9-6:10),CloseRound(6:10-6:11),
LowerIdent(7:5-7:6),
CloseCurly(8:1-8:2),
EndOfFile(9:1-9:1),
~~~
# PARSE
~~~clojure
(e-block @1.1-8.2
	(statements
		(s-decl @2.5-2.10
			(p-ident @2.5-2.6 (raw "x"))
			(e-int @2.9-2.10 (raw "5")))
		(s-decl @3.5-6.11
			(p-ident @3.5-3.6 (raw "y"))
			(e-apply @3.9-6.11
				(e-tuple @3.9-6.7
					(e-lambda @3.10-6.6
						(args
							(p-underscore))
						(e-block @3.14-6.6
							(statements
								(s-decl @4.9-4.15
									(p-ident @4.9-4.10 (raw "x"))
									(e-int @4.13-4.15 (raw "10")))
								(e-ident @5.9-5.10 (raw "x"))))))
				(e-record @6.8-6.10)))
		(e-ident @7.5-7.6 (raw "y"))))
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
(e-block @1.1-8.2
	(s-let @2.5-2.10
		(p-assign @2.5-2.6 (ident "x"))
		(e-num @2.9-2.10 (value "5")))
	(s-let @3.5-6.11
		(p-assign @3.5-3.6 (ident "y"))
		(e-call @3.9-6.11
			(e-lambda @3.10-6.6
				(args
					(p-underscore @3.11-3.12))
				(e-block @3.14-6.6
					(s-let @4.9-4.15
						(p-assign @4.9-4.10 (ident "x"))
						(e-num @4.13-4.15 (value "10")))
					(e-lookup-local @5.9-5.10
						(p-assign @4.9-4.10 (ident "x")))))
			(e-empty_record @6.8-6.10)))
	(e-lookup-local @7.5-7.6
		(p-assign @3.5-3.6 (ident "y"))))
~~~
# TYPES
~~~clojure
(expr @1.1-8.2 (type "Num(_size)"))
~~~
