# META
~~~ini
description=Inline annotation for statements
type=file
~~~
# SOURCE
~~~roc
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
MISSING MAIN! FUNCTION - statement_annotations.md:1:1:14:2
TYPE MISMATCH - statement_annotations.md:11:7:11:8
# PROBLEMS
**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**statement_annotations.md:1:1:14:2:**
```roc
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
```


**TYPE MISMATCH**
This expression is used in an unexpected way:
**statement_annotations.md:11:7:11:8:**
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
LowerIdent(1:1-1:10),OpAssign(1:11-1:12),OpBar(1:13-1:14),LowerIdent(1:14-1:15),OpBar(1:15-1:16),OpenCurly(1:17-1:18),
LowerIdent(2:3-2:4),OpColon(2:5-2:6),UpperIdent(2:7-2:10),
LowerIdent(3:3-3:4),OpAssign(3:5-3:6),LowerIdent(3:7-3:8),OpPlus(3:9-3:10),Int(3:11-3:12),
LowerIdent(5:3-5:4),
CloseCurly(6:1-6:2),
LowerIdent(8:1-8:5),OpColon(8:6-8:7),LowerIdent(8:8-8:11),OpArrow(8:12-8:14),LowerIdent(8:15-8:18),
LowerIdent(9:1-9:5),OpAssign(9:6-9:7),OpBar(9:8-9:9),LowerIdent(9:9-9:10),OpBar(9:10-9:11),OpenCurly(9:12-9:13),
LowerIdent(10:3-10:4),OpColon(10:5-10:6),LowerIdent(10:7-10:10),
LowerIdent(11:3-11:4),OpAssign(11:5-11:6),LowerIdent(11:7-11:8),
LowerIdent(13:3-13:4),
CloseCurly(14:1-14:2),
EndOfFile(15:1-15:1),
~~~
# PARSE
~~~clojure
(file @1.1-14.2
	(type-module @1.1-1.10)
	(statements
		(s-decl @1.1-6.2
			(p-ident @1.1-1.10 (raw "addOneU64"))
			(e-lambda @1.13-6.2
				(args
					(p-ident @1.14-1.15 (raw "x")))
				(e-block @1.17-6.2
					(statements
						(s-type-anno @2.3-2.10 (name "y")
							(ty @2.7-2.10 (name "U64")))
						(s-decl @3.3-3.12
							(p-ident @3.3-3.4 (raw "y"))
							(e-binop @3.7-3.12 (op "+")
								(e-ident @3.7-3.8 (raw "x"))
								(e-int @3.11-3.12 (raw "1"))))
						(e-ident @5.3-5.4 (raw "y"))))))
		(s-type-anno @8.1-8.18 (name "func")
			(ty-fn @8.8-8.18
				(ty-var @8.8-8.11 (raw "val"))
				(ty-var @8.15-8.18 (raw "val"))))
		(s-decl @9.1-14.2
			(p-ident @9.1-9.5 (raw "func"))
			(e-lambda @9.8-14.2
				(args
					(p-ident @9.9-9.10 (raw "x")))
				(e-block @9.12-14.2
					(statements
						(s-type-anno @10.3-10.10 (name "y")
							(ty-var @10.7-10.10 (raw "val")))
						(s-decl @11.3-11.8
							(p-ident @11.3-11.4 (raw "y"))
							(e-ident @11.7-11.8 (raw "x")))
						(e-ident @13.3-13.4 (raw "y"))))))))
~~~
# FORMATTED
~~~roc
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
		(p-assign @1.1-1.10 (ident "addOneU64"))
		(e-lambda @1.13-6.2
			(args
				(p-assign @1.14-1.15 (ident "x")))
			(e-block @1.17-6.2
				(s-type-anno @2.3-2.10 (name "y")
					(ty @2.7-2.10 (name "U64")))
				(s-let @3.3-3.12
					(p-assign @3.3-3.4 (ident "y"))
					(e-binop @3.7-3.12 (op "add")
						(e-lookup-local @3.7-3.8
							(p-assign @1.14-1.15 (ident "x")))
						(e-int @3.11-3.12 (value "1"))))
				(e-lookup-local @5.3-5.4
					(p-assign @3.3-3.4 (ident "y"))))))
	(d-let
		(p-assign @9.1-9.5 (ident "func"))
		(e-lambda @9.8-14.2
			(args
				(p-assign @9.9-9.10 (ident "x")))
			(e-block @9.12-14.2
				(s-type-anno @10.3-10.10 (name "y")
					(ty-var @10.7-10.10 (name "val")))
				(s-let @11.3-11.8
					(p-assign @11.3-11.4 (ident "y"))
					(e-lookup-local @11.7-11.8
						(p-assign @9.9-9.10 (ident "x"))))
				(e-lookup-local @13.3-13.4
					(p-assign @11.3-11.4 (ident "y")))))
		(annotation @9.1-9.5
			(declared-type
				(ty-fn @8.8-8.18 (effectful false)
					(ty-var @8.8-8.11 (name "val"))
					(ty-var @8.15-8.18 (name "val")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.1-1.10 (type "U64 -> U64"))
		(patt @9.1-9.5 (type "Error -> Error")))
	(expressions
		(expr @1.13-6.2 (type "U64 -> U64"))
		(expr @9.8-14.2 (type "Error -> Error"))))
~~~
