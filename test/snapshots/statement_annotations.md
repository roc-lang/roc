# META
~~~ini
description=Inline annotation for statements
type=file
~~~
# SOURCE
~~~roc
module []

addOneU64 = |x| {
  y : U64
  y = x + 1

  y
}

func : val -> val
func = |x| {
  y : val
  y = x

  y
}
~~~
# EXPECTED
TYPE MISMATCH - statement_annotations.md:13:7:13:8
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**statement_annotations.md:13:7:13:8:**
```roc
  y = x
```
      ^

The type annotation says it should have the type:
    _val_

But here it's being used as:
    _val_

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
LowerIdent(3:1-3:10),OpAssign(3:11-3:12),OpBar(3:13-3:14),LowerIdent(3:14-3:15),OpBar(3:15-3:16),OpenCurly(3:17-3:18),
LowerIdent(4:3-4:4),OpColon(4:5-4:6),UpperIdent(4:7-4:10),
LowerIdent(5:3-5:4),OpAssign(5:5-5:6),LowerIdent(5:7-5:8),OpPlus(5:9-5:10),Int(5:11-5:12),
LowerIdent(7:3-7:4),
CloseCurly(8:1-8:2),
LowerIdent(10:1-10:5),OpColon(10:6-10:7),LowerIdent(10:8-10:11),OpArrow(10:12-10:14),LowerIdent(10:15-10:18),
LowerIdent(11:1-11:5),OpAssign(11:6-11:7),OpBar(11:8-11:9),LowerIdent(11:9-11:10),OpBar(11:10-11:11),OpenCurly(11:12-11:13),
LowerIdent(12:3-12:4),OpColon(12:5-12:6),LowerIdent(12:7-12:10),
LowerIdent(13:3-13:4),OpAssign(13:5-13:6),LowerIdent(13:7-13:8),
LowerIdent(15:3-15:4),
CloseCurly(16:1-16:2),EndOfFile(16:2-16:2),
~~~
# PARSE
~~~clojure
(file @1.1-16.2
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-decl @3.1-8.2
			(p-ident @3.1-3.10 (raw "addOneU64"))
			(e-lambda @3.13-8.2
				(args
					(p-ident @3.14-3.15 (raw "x")))
				(e-block @3.17-8.2
					(statements
						(s-type-anno @4.3-4.10 (name "y")
							(ty @4.7-4.10 (name "U64")))
						(s-decl @5.3-5.12
							(p-ident @5.3-5.4 (raw "y"))
							(e-binop @5.7-5.12 (op "+")
								(e-ident @5.7-5.8 (raw "x"))
								(e-int @5.11-5.12 (raw "1"))))
						(e-ident @7.3-7.4 (raw "y"))))))
		(s-type-anno @10.1-10.18 (name "func")
			(ty-fn @10.8-10.18
				(ty-var @10.8-10.11 (raw "val"))
				(ty-var @10.15-10.18 (raw "val"))))
		(s-decl @11.1-16.2
			(p-ident @11.1-11.5 (raw "func"))
			(e-lambda @11.8-16.2
				(args
					(p-ident @11.9-11.10 (raw "x")))
				(e-block @11.12-16.2
					(statements
						(s-type-anno @12.3-12.10 (name "y")
							(ty-var @12.7-12.10 (raw "val")))
						(s-decl @13.3-13.8
							(p-ident @13.3-13.4 (raw "y"))
							(e-ident @13.7-13.8 (raw "x")))
						(e-ident @15.3-15.4 (raw "y"))))))))
~~~
# FORMATTED
~~~roc
module []

addOneU64 = |x| {
	y : U64
	y = x + 1

	y
}

func : val -> val
func = |x| {
	y : val
	y = x

	y
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.10 (ident "addOneU64"))
		(e-lambda @3.13-8.2
			(args
				(p-assign @3.14-3.15 (ident "x")))
			(e-block @3.17-8.2
				(s-type-anno @4.3-4.10 (name "y")
					(ty @4.7-4.10 (name "U64")))
				(s-let @5.3-5.12
					(p-assign @5.3-5.4 (ident "y"))
					(e-binop @5.7-5.12 (op "add")
						(e-lookup-local @5.7-5.8
							(p-assign @3.14-3.15 (ident "x")))
						(e-int @5.11-5.12 (value "1"))))
				(e-lookup-local @7.3-7.4
					(p-assign @5.3-5.4 (ident "y"))))))
	(d-let
		(p-assign @11.1-11.5 (ident "func"))
		(e-lambda @11.8-16.2
			(args
				(p-assign @11.9-11.10 (ident "x")))
			(e-block @11.12-16.2
				(s-type-anno @12.3-12.10 (name "y")
					(ty-var @12.7-12.10 (name "val")))
				(s-let @13.3-13.8
					(p-assign @13.3-13.4 (ident "y"))
					(e-lookup-local @13.7-13.8
						(p-assign @11.9-11.10 (ident "x"))))
				(e-lookup-local @15.3-15.4
					(p-assign @13.3-13.4 (ident "y")))))
		(annotation @11.1-11.5
			(declared-type
				(ty-fn @10.8-10.18 (effectful false)
					(ty-var @10.8-10.11 (name "val"))
					(ty-var @10.15-10.18 (name "val")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.10 (type "U64 -> U64"))
		(patt @11.1-11.5 (type "Error -> Error")))
	(expressions
		(expr @3.13-8.2 (type "U64 -> U64"))
		(expr @11.8-16.2 (type "Error -> Error"))))
~~~
