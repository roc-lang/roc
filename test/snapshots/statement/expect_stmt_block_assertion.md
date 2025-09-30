# META
~~~ini
description=Debug expression stmt
type=file
~~~
# SOURCE
~~~roc
foo : Bool -> Bool
foo = |a| {
    expect a == Bool.True
    a
}
~~~
# EXPECTED
MISSING MAIN! FUNCTION - expect_stmt_block_assertion.md:1:1:5:2
# PROBLEMS
**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**expect_stmt_block_assertion.md:1:1:5:2:**
```roc
foo : Bool -> Bool
foo = |a| {
    expect a == Bool.True
    a
}
```


# TOKENS
~~~zig
LowerIdent(1:1-1:4),OpColon(1:5-1:6),UpperIdent(1:7-1:11),OpArrow(1:12-1:14),UpperIdent(1:15-1:19),
LowerIdent(2:1-2:4),OpAssign(2:5-2:6),OpBar(2:7-2:8),LowerIdent(2:8-2:9),OpBar(2:9-2:10),OpenCurly(2:11-2:12),
KwExpect(3:5-3:11),LowerIdent(3:12-3:13),OpEquals(3:14-3:16),UpperIdent(3:17-3:21),NoSpaceDotUpperIdent(3:21-3:26),
LowerIdent(4:5-4:6),
CloseCurly(5:1-5:2),
EndOfFile(6:1-6:1),
~~~
# PARSE
~~~clojure
(file @1.1-5.2
	(type-module @1.1-1.4)
	(statements
		(s-type-anno @1.1-1.19 (name "foo")
			(ty-fn @1.7-1.19
				(ty @1.7-1.11 (name "Bool"))
				(ty @1.15-1.19 (name "Bool"))))
		(s-decl @2.1-5.2
			(p-ident @2.1-2.4 (raw "foo"))
			(e-lambda @2.7-5.2
				(args
					(p-ident @2.8-2.9 (raw "a")))
				(e-block @2.11-5.2
					(statements
						(s-expect @3.5-3.26
							(e-binop @3.12-3.26 (op "==")
								(e-ident @3.12-3.13 (raw "a"))
								(e-tag @3.17-3.26 (raw "Bool.True"))))
						(e-ident @4.5-4.6 (raw "a"))))))))
~~~
# FORMATTED
~~~roc
foo : Bool -> Bool
foo = |a| {
	expect a == Bool.True
	a
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @2.1-2.4 (ident "foo"))
		(e-lambda @2.7-5.2
			(args
				(p-assign @2.8-2.9 (ident "a")))
			(e-block @2.11-5.2
				(s-expect @3.5-3.26
					(e-binop @3.12-3.26 (op "eq")
						(e-lookup-local @3.12-3.13
							(p-assign @2.8-2.9 (ident "a")))
						(e-nominal @3.17-3.26 (nominal "Bool")
							(e-tag @3.17-3.26 (name "True")))))
				(e-lookup-local @4.5-4.6
					(p-assign @2.8-2.9 (ident "a")))))
		(annotation @2.1-2.4
			(declared-type
				(ty-fn @1.7-1.19 (effectful false)
					(ty @1.7-1.11 (name "Bool"))
					(ty @1.15-1.19 (name "Bool")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @2.1-2.4 (type "Bool -> Bool")))
	(expressions
		(expr @2.7-5.2 (type "Bool -> Bool"))))
~~~
