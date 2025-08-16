# META
~~~ini
description=Lambda annotation mismatch error message test - verifies error messages assume annotation is correct and implementation is wrong
type=file
~~~
# SOURCE
~~~roc
module [string_function, wrong_type_function]

# Annotation says it takes and returns strings, but implementation uses number addition
string_function : Str -> Str
string_function = |x| x + 42

# Annotation says function returns I64, but implementation returns Frac(_prec)
wrong_type_function : I64 -> I64
wrong_type_function = |x| x * 3.14
~~~
# EXPECTED
TYPE MISMATCH - lambda_annotation_mismatch_error.md:4:26:4:29
TYPE MISMATCH - lambda_annotation_mismatch_error.md:8:30:8:33
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**lambda_annotation_mismatch_error.md:4:26:4:29:**
```roc
string_function : Str -> Str
```
                         ^^^

It is of type:
    _Str_

But here it's being used as:
    _Num(_size)_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**lambda_annotation_mismatch_error.md:8:30:8:33:**
```roc
wrong_type_function : I64 -> I64
```
                             ^^^

It is of type:
    _I64_

But here it's being used as:
    _Frac(_size)_

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:24),Comma(1:24-1:25),LowerIdent(1:26-1:45),CloseSquare(1:45-1:46),
LowerIdent(4:1-4:16),OpColon(4:17-4:18),UpperIdent(4:19-4:22),OpArrow(4:23-4:25),UpperIdent(4:26-4:29),
LowerIdent(5:1-5:16),OpAssign(5:17-5:18),OpBar(5:19-5:20),LowerIdent(5:20-5:21),OpBar(5:21-5:22),LowerIdent(5:23-5:24),OpPlus(5:25-5:26),Int(5:27-5:29),
LowerIdent(8:1-8:20),OpColon(8:21-8:22),UpperIdent(8:23-8:26),OpArrow(8:27-8:29),UpperIdent(8:30-8:33),
LowerIdent(9:1-9:20),OpAssign(9:21-9:22),OpBar(9:23-9:24),LowerIdent(9:24-9:25),OpBar(9:25-9:26),LowerIdent(9:27-9:28),OpStar(9:29-9:30),Float(9:31-9:35),EndOfFile(9:35-9:35),
~~~
# PARSE
~~~clojure
(file @1.1-9.35
	(module @1.1-1.46
		(exposes @1.8-1.46
			(exposed-lower-ident @1.9-1.24
				(text "string_function"))
			(exposed-lower-ident @1.26-1.45
				(text "wrong_type_function"))))
	(statements
		(s-type-anno @4.1-4.29 (name "string_function")
			(ty-fn @4.19-4.29
				(ty @4.19-4.22 (name "Str"))
				(ty @4.26-4.29 (name "Str"))))
		(s-decl @5.1-5.29
			(p-ident @5.1-5.16 (raw "string_function"))
			(e-lambda @5.19-5.29
				(args
					(p-ident @5.20-5.21 (raw "x")))
				(e-binop @5.23-5.29 (op "+")
					(e-ident @5.23-5.24 (raw "x"))
					(e-int @5.27-5.29 (raw "42")))))
		(s-type-anno @8.1-8.33 (name "wrong_type_function")
			(ty-fn @8.23-8.33
				(ty @8.23-8.26 (name "I64"))
				(ty @8.30-8.33 (name "I64"))))
		(s-decl @9.1-9.35
			(p-ident @9.1-9.20 (raw "wrong_type_function"))
			(e-lambda @9.23-9.35
				(args
					(p-ident @9.24-9.25 (raw "x")))
				(e-binop @9.27-9.35 (op "*")
					(e-ident @9.27-9.28 (raw "x"))
					(e-frac @9.31-9.35 (raw "3.14")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @5.1-5.16 (ident "string_function"))
		(e-lambda @5.19-5.29
			(args
				(p-assign @5.20-5.21 (ident "x")))
			(e-binop @5.23-5.29 (op "add")
				(e-lookup-local @5.23-5.24
					(p-assign @5.20-5.21 (ident "x")))
				(e-int @5.27-5.29 (value "42"))))
		(annotation @5.1-5.16
			(declared-type
				(ty-fn @4.19-4.29 (effectful false)
					(ty @4.19-4.22 (name "Str"))
					(ty @4.26-4.29 (name "Str"))))))
	(d-let
		(p-assign @9.1-9.20 (ident "wrong_type_function"))
		(e-lambda @9.23-9.35
			(args
				(p-assign @9.24-9.25 (ident "x")))
			(e-binop @9.27-9.35 (op "mul")
				(e-lookup-local @9.27-9.28
					(p-assign @9.24-9.25 (ident "x")))
				(e-dec-small @9.31-9.35 (numerator "314") (denominator-power-of-ten "2") (value "3.14"))))
		(annotation @9.1-9.20
			(declared-type
				(ty-fn @8.23-8.33 (effectful false)
					(ty @8.23-8.26 (name "I64"))
					(ty @8.30-8.33 (name "I64")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.16 (type "Error -> Error"))
		(patt @9.1-9.20 (type "Error -> Error")))
	(expressions
		(expr @5.19-5.29 (type "Error -> Error"))
		(expr @9.23-9.35 (type "Error -> Error"))))
~~~
