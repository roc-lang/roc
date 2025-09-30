# META
~~~ini
description=Lambda annotation mismatch error message test - verifies error messages assume annotation is correct and implementation is wrong
type=file
~~~
# SOURCE
~~~roc
# Annotation says it takes and returns strings, but implementation uses number addition
string_function : Str -> Str
string_function = |x| x + 42

# Annotation says function returns I64, but implementation returns Frac(_prec)
wrong_type_function : I64 -> I64
wrong_type_function = |x| x * 3.14
~~~
# EXPECTED
MISSING MAIN! FUNCTION - lambda_annotation_mismatch_error.md:2:1:7:35
TYPE MISMATCH - lambda_annotation_mismatch_error.md:3:27:3:29
TYPE MISMATCH - lambda_annotation_mismatch_error.md:7:31:7:35
# PROBLEMS
**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**lambda_annotation_mismatch_error.md:2:1:7:35:**
```roc
string_function : Str -> Str
string_function = |x| x + 42

# Annotation says function returns I64, but implementation returns Frac(_prec)
wrong_type_function : I64 -> I64
wrong_type_function = |x| x * 3.14
```


**TYPE MISMATCH**
This expression is used in an unexpected way:
**lambda_annotation_mismatch_error.md:3:27:3:29:**
```roc
string_function = |x| x + 42
```
                          ^^

The type annotation says it should have the type:
    _Str_

But here it's being used as:
    _Num(_size)_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**lambda_annotation_mismatch_error.md:7:31:7:35:**
```roc
wrong_type_function = |x| x * 3.14
```
                              ^^^^

The type annotation says it should have the type:
    _I64_

But here it's being used as:
    _Frac(_size)_

# TOKENS
~~~zig
LowerIdent(2:1-2:16),OpColon(2:17-2:18),UpperIdent(2:19-2:22),OpArrow(2:23-2:25),UpperIdent(2:26-2:29),
LowerIdent(3:1-3:16),OpAssign(3:17-3:18),OpBar(3:19-3:20),LowerIdent(3:20-3:21),OpBar(3:21-3:22),LowerIdent(3:23-3:24),OpPlus(3:25-3:26),Int(3:27-3:29),
LowerIdent(6:1-6:20),OpColon(6:21-6:22),UpperIdent(6:23-6:26),OpArrow(6:27-6:29),UpperIdent(6:30-6:33),
LowerIdent(7:1-7:20),OpAssign(7:21-7:22),OpBar(7:23-7:24),LowerIdent(7:24-7:25),OpBar(7:25-7:26),LowerIdent(7:27-7:28),OpStar(7:29-7:30),Float(7:31-7:35),
EndOfFile(8:1-8:1),
~~~
# PARSE
~~~clojure
(file @2.1-7.35
	(type-module @2.1-2.16)
	(statements
		(s-type-anno @2.1-2.29 (name "string_function")
			(ty-fn @2.19-2.29
				(ty @2.19-2.22 (name "Str"))
				(ty @2.26-2.29 (name "Str"))))
		(s-decl @3.1-3.29
			(p-ident @3.1-3.16 (raw "string_function"))
			(e-lambda @3.19-3.29
				(args
					(p-ident @3.20-3.21 (raw "x")))
				(e-binop @3.23-3.29 (op "+")
					(e-ident @3.23-3.24 (raw "x"))
					(e-int @3.27-3.29 (raw "42")))))
		(s-type-anno @6.1-6.33 (name "wrong_type_function")
			(ty-fn @6.23-6.33
				(ty @6.23-6.26 (name "I64"))
				(ty @6.30-6.33 (name "I64"))))
		(s-decl @7.1-7.35
			(p-ident @7.1-7.20 (raw "wrong_type_function"))
			(e-lambda @7.23-7.35
				(args
					(p-ident @7.24-7.25 (raw "x")))
				(e-binop @7.27-7.35 (op "*")
					(e-ident @7.27-7.28 (raw "x"))
					(e-frac @7.31-7.35 (raw "3.14")))))))
~~~
# FORMATTED
~~~roc
# Annotation says it takes and returns strings, but implementation uses number addition
# Annotation says it takes and returns strings, but implementation uses number addition
string_function : Str -> Str
string_function = |x| x + 42

# Annotation says function returns I64, but implementation returns Frac(_prec)
wrong_type_function : I64 -> I64
wrong_type_function = |x| x * 3.14
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.16 (ident "string_function"))
		(e-lambda @3.19-3.29
			(args
				(p-assign @3.20-3.21 (ident "x")))
			(e-binop @3.23-3.29 (op "add")
				(e-lookup-local @3.23-3.24
					(p-assign @3.20-3.21 (ident "x")))
				(e-int @3.27-3.29 (value "42"))))
		(annotation @3.1-3.16
			(declared-type
				(ty-fn @2.19-2.29 (effectful false)
					(ty @2.19-2.22 (name "Str"))
					(ty @2.26-2.29 (name "Str"))))))
	(d-let
		(p-assign @7.1-7.20 (ident "wrong_type_function"))
		(e-lambda @7.23-7.35
			(args
				(p-assign @7.24-7.25 (ident "x")))
			(e-binop @7.27-7.35 (op "mul")
				(e-lookup-local @7.27-7.28
					(p-assign @7.24-7.25 (ident "x")))
				(e-dec-small @7.31-7.35 (numerator "314") (denominator-power-of-ten "2") (value "3.14"))))
		(annotation @7.1-7.20
			(declared-type
				(ty-fn @6.23-6.33 (effectful false)
					(ty @6.23-6.26 (name "I64"))
					(ty @6.30-6.33 (name "I64")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.16 (type "Error -> Error"))
		(patt @7.1-7.20 (type "Error -> Error")))
	(expressions
		(expr @3.19-3.29 (type "Error -> Error"))
		(expr @7.23-7.35 (type "Error -> Error"))))
~~~
