# META
~~~ini
description=Lambda annotation mismatch error message test - verifies error messages assume annotation is correct and implementation is wrong
type=snippet
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
TYPE MISMATCH - lambda_annotation_mismatch_error.md:7:31:7:35
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**lambda_annotation_mismatch_error.md:7:31:7:35:**
```roc
wrong_type_function = |x| x * 3.14
```
                              ^^^^

It has the type:
    _Num(Frac(_size))_

But the type annotation says it should have the type:
    _Num(Int(Signed64))_

**Hint:** This might be because the numeric literal is too large to fit in the target type.

# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Int,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpStar,Float,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "string_function")
			(ty-fn
				(ty (name "Str"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "string_function"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-binop (op "+")
					(e-ident (raw "x"))
					(e-int (raw "42")))))
		(s-type-anno (name "wrong_type_function")
			(ty-fn
				(ty (name "I64"))
				(ty (name "I64"))))
		(s-decl
			(p-ident (raw "wrong_type_function"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-binop (op "*")
					(e-ident (raw "x"))
					(e-frac (raw "3.14")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "string_function"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-binop (op "add")
				(e-lookup-local
					(p-assign (ident "x")))
				(e-num (value "42"))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (external-module "Str"))
				(ty-lookup (name "Str") (external-module "Str")))))
	(d-let
		(p-assign (ident "wrong_type_function"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-binop (op "mul")
				(e-lookup-local
					(p-assign (ident "x")))
				(e-dec-small (numerator "314") (denominator-power-of-ten "2") (value "3.14"))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "I64") (builtin))
				(ty-lookup (name "I64") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error -> Error"))
		(patt (type "Num(Int(Signed64)) -> Error")))
	(expressions
		(expr (type "Error -> Error"))
		(expr (type "Num(Int(Signed64)) -> Error"))))
~~~
