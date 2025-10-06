# META
~~~ini
description=Basic variable scoping behavior
type=snippet
~~~
# SOURCE
~~~roc
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
DUPLICATE DEFINITION - can_basic_scoping.md:7:5:7:6
# PROBLEMS
**DUPLICATE DEFINITION**
The name `x` is being redeclared in this scope.

The redeclaration is here:
**can_basic_scoping.md:7:5:7:6:**
```roc
    x = 20  # Should shadow top-level x
```
    ^

But `x` was already defined here:
**can_basic_scoping.md:2:1:2:2:**
```roc
x = 5
```
^


# TOKENS
~~~zig
LowerIdent(2:1-2:2),OpAssign(2:3-2:4),Int(2:5-2:6),
LowerIdent(3:1-3:2),OpAssign(3:3-3:4),Int(3:5-3:7),
LowerIdent(6:1-6:10),OpAssign(6:11-6:12),OpBar(6:13-6:14),Underscore(6:14-6:15),OpBar(6:15-6:16),OpenCurly(6:17-6:18),
LowerIdent(7:5-7:6),OpAssign(7:7-7:8),Int(7:9-7:11),
LowerIdent(8:5-8:16),OpAssign(8:17-8:18),OpenCurly(8:19-8:20),
LowerIdent(10:9-10:10),OpAssign(10:11-10:12),LowerIdent(10:13-10:14),OpPlus(10:15-10:16),LowerIdent(10:17-10:18),
LowerIdent(11:9-11:10),OpPlus(11:11-11:12),Int(11:13-11:14),
CloseCurly(12:5-12:6),
LowerIdent(13:5-13:16),
CloseCurly(14:1-14:2),
EndOfFile(15:1-15:1),
~~~
# PARSE
~~~clojure
(file @2.1-14.2
	(type-module @2.1-2.2)
	(statements
		(s-decl @2.1-2.6
			(p-ident @2.1-2.2 (raw "x"))
			(e-int @2.5-2.6 (raw "5")))
		(s-decl @3.1-3.7
			(p-ident @3.1-3.2 (raw "y"))
			(e-int @3.5-3.7 (raw "10")))
		(s-decl @6.1-14.2
			(p-ident @6.1-6.10 (raw "outerFunc"))
			(e-lambda @6.13-14.2
				(args
					(p-underscore))
				(e-block @6.17-14.2
					(statements
						(s-decl @7.5-7.11
							(p-ident @7.5-7.6 (raw "x"))
							(e-int @7.9-7.11 (raw "20")))
						(s-decl @8.5-12.6
							(p-ident @8.5-8.16 (raw "innerResult"))
							(e-block @8.19-12.6
								(statements
									(s-decl @10.9-10.18
										(p-ident @10.9-10.10 (raw "z"))
										(e-binop @10.13-10.18 (op "+")
											(e-ident @10.13-10.14 (raw "x"))
											(e-ident @10.17-10.18 (raw "y"))))
									(e-binop @11.9-11.14 (op "+")
										(e-ident @11.9-11.10 (raw "z"))
										(e-int @11.13-11.14 (raw "1"))))))
						(e-ident @13.5-13.16 (raw "innerResult"))))))))
~~~
# FORMATTED
~~~roc
# Top-level variables
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
		(p-assign @2.1-2.2 (ident "x"))
		(e-num @2.5-2.6 (value "5")))
	(d-let
		(p-assign @3.1-3.2 (ident "y"))
		(e-num @3.5-3.7 (value "10")))
	(d-let
		(p-assign @6.1-6.10 (ident "outerFunc"))
		(e-closure @6.13-14.2
			(captures
				(capture @3.1-3.2 (ident "y")))
			(e-lambda @6.13-14.2
				(args
					(p-underscore @6.14-6.15))
				(e-block @6.17-14.2
					(s-let @7.5-7.11
						(p-assign @7.5-7.6 (ident "x"))
						(e-num @7.9-7.11 (value "20")))
					(s-let @8.5-12.6
						(p-assign @8.5-8.16 (ident "innerResult"))
						(e-block @8.19-12.6
							(s-let @10.9-10.18
								(p-assign @10.9-10.10 (ident "z"))
								(e-binop @10.13-10.18 (op "add")
									(e-lookup-local @10.13-10.14
										(p-assign @7.5-7.6 (ident "x")))
									(e-lookup-local @10.17-10.18
										(p-assign @3.1-3.2 (ident "y")))))
							(e-binop @11.9-11.14 (op "add")
								(e-lookup-local @11.9-11.10
									(p-assign @10.9-10.10 (ident "z")))
								(e-num @11.13-11.14 (value "1")))))
					(e-lookup-local @13.5-13.16
						(p-assign @8.5-8.16 (ident "innerResult"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @2.1-2.2 (type "Num(_size)"))
		(patt @3.1-3.2 (type "Num(_size)"))
		(patt @6.1-6.10 (type "_arg -> Num(_size)")))
	(expressions
		(expr @2.5-2.6 (type "Num(_size)"))
		(expr @3.5-3.7 (type "Num(_size)"))
		(expr @6.13-14.2 (type "_arg -> Num(_size)"))))
~~~
