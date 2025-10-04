# META
~~~ini
description=Debug expression stmt
type=file
~~~
# SOURCE
~~~roc
module [foo]

foo : Bool -> Bool
foo = |a| {
    expect a == Bool.True
    a
}
~~~
# EXPECTED
MODULE HEADER DEPRECATED - expect_stmt_block_assertion.md:1:1:1:13
# PROBLEMS
**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**expect_stmt_block_assertion.md:1:1:1:13:**
```roc
module [foo]
```
^^^^^^^^^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:12),CloseSquare(1:12-1:13),
LowerIdent(3:1-3:4),OpColon(3:5-3:6),UpperIdent(3:7-3:11),OpArrow(3:12-3:14),UpperIdent(3:15-3:19),
LowerIdent(4:1-4:4),OpAssign(4:5-4:6),OpBar(4:7-4:8),LowerIdent(4:8-4:9),OpBar(4:9-4:10),OpenCurly(4:11-4:12),
KwExpect(5:5-5:11),LowerIdent(5:12-5:13),OpEquals(5:14-5:16),UpperIdent(5:17-5:21),NoSpaceDotUpperIdent(5:21-5:26),
LowerIdent(6:5-6:6),
CloseCurly(7:1-7:2),
EndOfFile(8:1-8:1),
~~~
# PARSE
~~~clojure
(file @1.1-7.2
	(module @1.1-1.13
		(exposes @1.8-1.13
			(exposed-lower-ident @1.9-1.12
				(text "foo"))))
	(statements
		(s-type-anno @3.1-3.19 (name "foo")
			(ty-fn @3.7-3.19
				(ty @3.7-3.11 (name "Bool"))
				(ty @3.15-3.19 (name "Bool"))))
		(s-decl @4.1-7.2
			(p-ident @4.1-4.4 (raw "foo"))
			(e-lambda @4.7-7.2
				(args
					(p-ident @4.8-4.9 (raw "a")))
				(e-block @4.11-7.2
					(statements
						(s-expect @5.5-5.26
							(e-binop @5.12-5.26 (op "==")
								(e-ident @5.12-5.13 (raw "a"))
								(e-tag @5.17-5.26 (raw "Bool.True"))))
						(e-ident @6.5-6.6 (raw "a"))))))))
~~~
# FORMATTED
~~~roc
module [foo]

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
		(p-assign @4.1-4.4 (ident "foo"))
		(e-lambda @4.7-7.2
			(args
				(p-assign @4.8-4.9 (ident "a")))
			(e-block @4.11-7.2
				(s-expect @5.5-5.26
					(e-binop @5.12-5.26 (op "eq")
						(e-lookup-local @5.12-5.13
							(p-assign @4.8-4.9 (ident "a")))
						(e-nominal @5.17-5.26 (nominal "Bool")
							(e-tag @5.17-5.26 (name "True")))))
				(e-lookup-local @6.5-6.6
					(p-assign @4.8-4.9 (ident "a")))))
		(annotation @4.1-4.4
			(declared-type
				(ty-fn @3.7-3.19 (effectful false)
					(ty-lookup @3.7-3.11 (name "Bool") (local))
					(ty-lookup @3.15-3.19 (name "Bool") (local)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.4 (type "Bool -> Bool")))
	(expressions
		(expr @4.7-7.2 (type "Bool -> Bool"))))
~~~
