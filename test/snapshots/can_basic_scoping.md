# META
~~~ini
description=Basic variable scoping behavior
type=file
~~~
# SOURCE
~~~roc
module []

# Top-level variables
x = 5
y = 10

# Function that shadows outer variable
outerFunc = |_| {
    x = 20  # Should shadow top-level x
    innerResult = {
        # Block scope
        z = x + y  # x should resolve to 20, y to 10
        z + 1
    }
    innerResult
}
~~~
# EXPECTED
DUPLICATE DEFINITION - can_basic_scoping.md:9:5:9:6
# PROBLEMS
**DUPLICATE DEFINITION**
The name `x` is being redeclared in this scope.

The redeclaration is here:
**can_basic_scoping.md:9:5:9:6:**
```roc
    x = 20  # Should shadow top-level x
```
    ^

But `x` was already defined here:
**can_basic_scoping.md:4:1:4:2:**
```roc
x = 5
```
^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
LowerIdent(4:1-4:2),OpAssign(4:3-4:4),Int(4:5-4:6),
LowerIdent(5:1-5:2),OpAssign(5:3-5:4),Int(5:5-5:7),
LowerIdent(8:1-8:10),OpAssign(8:11-8:12),OpBar(8:13-8:14),Underscore(8:14-8:15),OpBar(8:15-8:16),OpenCurly(8:17-8:18),
LowerIdent(9:5-9:6),OpAssign(9:7-9:8),Int(9:9-9:11),
LowerIdent(10:5-10:16),OpAssign(10:17-10:18),OpenCurly(10:19-10:20),
LowerIdent(12:9-12:10),OpAssign(12:11-12:12),LowerIdent(12:13-12:14),OpPlus(12:15-12:16),LowerIdent(12:17-12:18),
LowerIdent(13:9-13:10),OpPlus(13:11-13:12),Int(13:13-13:14),
CloseCurly(14:5-14:6),
LowerIdent(15:5-15:16),
CloseCurly(16:1-16:2),
EndOfFile(17:1-17:1),
~~~
# PARSE
~~~clojure
(file @1.1-16.2
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-decl @4.1-4.6
			(p-ident @4.1-4.2 (raw "x"))
			(e-int @4.5-4.6 (raw "5")))
		(s-decl @5.1-5.7
			(p-ident @5.1-5.2 (raw "y"))
			(e-int @5.5-5.7 (raw "10")))
		(s-decl @8.1-16.2
			(p-ident @8.1-8.10 (raw "outerFunc"))
			(e-lambda @8.13-16.2
				(args
					(p-underscore))
				(e-block @8.17-16.2
					(statements
						(s-decl @9.5-9.11
							(p-ident @9.5-9.6 (raw "x"))
							(e-int @9.9-9.11 (raw "20")))
						(s-decl @10.5-14.6
							(p-ident @10.5-10.16 (raw "innerResult"))
							(e-block @10.19-14.6
								(statements
									(s-decl @12.9-12.18
										(p-ident @12.9-12.10 (raw "z"))
										(e-binop @12.13-12.18 (op "+")
											(e-ident @12.13-12.14 (raw "x"))
											(e-ident @12.17-12.18 (raw "y"))))
									(e-binop @13.9-13.14 (op "+")
										(e-ident @13.9-13.10 (raw "z"))
										(e-int @13.13-13.14 (raw "1"))))))
						(e-ident @15.5-15.16 (raw "innerResult"))))))))
~~~
# FORMATTED
~~~roc
module []

# Top-level variables
x = 5
y = 10

# Function that shadows outer variable
outerFunc = |_| {
	x = 20 # Should shadow top-level x
	innerResult = {
		# Block scope
		z = x + y # x should resolve to 20, y to 10
		z + 1
	}
	innerResult
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.2 (ident "x"))
		(e-int @4.5-4.6 (value "5")))
	(d-let
		(p-assign @5.1-5.2 (ident "y"))
		(e-int @5.5-5.7 (value "10")))
	(d-let
		(p-assign @8.1-8.10 (ident "outerFunc"))
		(e-closure @8.13-16.2
			(captures
				(capture @5.1-5.2 (ident "y")))
			(e-lambda @8.13-16.2
				(args
					(p-underscore @8.14-8.15))
				(e-block @8.17-16.2
					(s-let @9.5-9.11
						(p-assign @9.5-9.6 (ident "x"))
						(e-int @9.9-9.11 (value "20")))
					(s-let @10.5-14.6
						(p-assign @10.5-10.16 (ident "innerResult"))
						(e-block @10.19-14.6
							(s-let @12.9-12.18
								(p-assign @12.9-12.10 (ident "z"))
								(e-binop @12.13-12.18 (op "add")
									(e-lookup-local @12.13-12.14
										(p-assign @9.5-9.6 (ident "x")))
									(e-lookup-local @12.17-12.18
										(p-assign @5.1-5.2 (ident "y")))))
							(e-binop @13.9-13.14 (op "add")
								(e-lookup-local @13.9-13.10
									(p-assign @12.9-12.10 (ident "z")))
								(e-int @13.13-13.14 (value "1")))))
					(e-lookup-local @15.5-15.16
						(p-assign @10.5-10.16 (ident "innerResult"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.2 (type "Num(_size)"))
		(patt @5.1-5.2 (type "Num(_size)"))
		(patt @8.1-8.10 (type "_arg -> Num(_size)")))
	(expressions
		(expr @4.5-4.6 (type "Num(_size)"))
		(expr @5.5-5.7 (type "Num(_size)"))
		(expr @8.13-16.2 (type "_arg -> Num(_size)"))))
~~~
