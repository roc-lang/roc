# META
~~~ini
description=Lambda annotation mismatch error message test - verifies error messages assume annotation is correct and implementation is wrong
type=file
~~~
# SOURCE
~~~roc
module [stringFunction, wrongTypeFunction]

# Annotation says it takes and returns strings, but implementation uses number addition
stringFunction : Str -> Str
stringFunction = |x| x + 42

# Annotation says function returns I64, but implementation returns F64
wrongTypeFunction : I64 -> I64
wrongTypeFunction = |x| x * 3.14
~~~
# EXPECTED
TYPE MISMATCH - lambda_annotation_mismatch_error.md:4:25:4:28
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**lambda_annotation_mismatch_error.md:4:25:4:28:**
```roc
stringFunction : Str -> Str
```
                        ^^^

It is of type:
    _Str_

But you are trying to use it as:
    _Num(_size)_

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:23),Comma(1:23-1:24),LowerIdent(1:25-1:42),CloseSquare(1:42-1:43),
LowerIdent(4:1-4:15),OpColon(4:16-4:17),UpperIdent(4:18-4:21),OpArrow(4:22-4:24),UpperIdent(4:25-4:28),
LowerIdent(5:1-5:15),OpAssign(5:16-5:17),OpBar(5:18-5:19),LowerIdent(5:19-5:20),OpBar(5:20-5:21),LowerIdent(5:22-5:23),OpPlus(5:24-5:25),Int(5:26-5:28),
LowerIdent(8:1-8:18),OpColon(8:19-8:20),UpperIdent(8:21-8:24),OpArrow(8:25-8:27),UpperIdent(8:28-8:31),
LowerIdent(9:1-9:18),OpAssign(9:19-9:20),OpBar(9:21-9:22),LowerIdent(9:22-9:23),OpBar(9:23-9:24),LowerIdent(9:25-9:26),OpStar(9:27-9:28),Float(9:29-9:33),EndOfFile(9:33-9:33),
~~~
# PARSE
~~~clojure
(file @1.1-9.33
	(module @1.1-1.43
		(exposes @1.8-1.43
			(exposed-lower-ident @1.9-1.23
				(text "stringFunction"))
			(exposed-lower-ident @1.25-1.42
				(text "wrongTypeFunction"))))
	(statements
		(s-type-anno @4.1-4.28 (name "stringFunction")
			(ty-fn @4.18-4.28
				(ty @4.18-4.21 (name "Str"))
				(ty @4.25-4.28 (name "Str"))))
		(s-decl @5.1-5.28
			(p-ident @5.1-5.15 (raw "stringFunction"))
			(e-lambda @5.18-5.28
				(args
					(p-ident @5.19-5.20 (raw "x")))
				(e-binop @5.22-5.28 (op "+")
					(e-ident @5.22-5.23 (raw "x"))
					(e-int @5.26-5.28 (raw "42")))))
		(s-type-anno @8.1-8.31 (name "wrongTypeFunction")
			(ty-fn @8.21-8.31
				(ty @8.21-8.24 (name "I64"))
				(ty @8.28-8.31 (name "I64"))))
		(s-decl @9.1-9.33
			(p-ident @9.1-9.18 (raw "wrongTypeFunction"))
			(e-lambda @9.21-9.33
				(args
					(p-ident @9.22-9.23 (raw "x")))
				(e-binop @9.25-9.33 (op "*")
					(e-ident @9.25-9.26 (raw "x"))
					(e-frac @9.29-9.33 (raw "3.14")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @5.1-5.15 (ident "stringFunction"))
		(e-lambda @5.18-5.28
			(args
				(p-assign @5.19-5.20 (ident "x")))
			(e-binop @5.22-5.28 (op "add")
				(e-lookup-local @5.22-5.23
					(p-assign @5.19-5.20 (ident "x")))
				(e-int @5.26-5.28 (value "42"))))
		(annotation @5.1-5.15
			(declared-type
				(ty-fn @4.18-4.28 (effectful false)
					(ty @4.18-4.21 (name "Str"))
					(ty @4.25-4.28 (name "Str"))))))
	(d-let
		(p-assign @9.1-9.18 (ident "wrongTypeFunction"))
		(e-lambda @9.21-9.33
			(args
				(p-assign @9.22-9.23 (ident "x")))
			(e-binop @9.25-9.33 (op "mul")
				(e-lookup-local @9.25-9.26
					(p-assign @9.22-9.23 (ident "x")))
				(e-dec-small @9.29-9.33 (numerator "314") (denominator-power-of-ten "2") (value "3.14"))))
		(annotation @9.1-9.18
			(declared-type
				(ty-fn @8.21-8.31 (effectful false)
					(ty @8.21-8.24 (name "I64"))
					(ty @8.28-8.31 (name "I64")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.15 (type "Error -> Error"))
		(patt @9.1-9.18 (type "I64 -> I64")))
	(expressions
		(expr @5.18-5.28 (type "Error -> Error"))
		(expr @9.21-9.33 (type "I64 -> I64"))))
~~~
