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
MISSING METHOD - lambda_annotation_mismatch_error.md:3:27:3:29
MISSING METHOD - lambda_annotation_mismatch_error.md:3:23:3:29
+ - :0:0:0:0
# PROBLEMS
**MISSING METHOD**
This **from_numeral** method is being called on a value whose type doesn't have that method:
**lambda_annotation_mismatch_error.md:3:27:3:29:**
```roc
string_function = |x| x + 42
```
                          ^^

The value's type, which does not have a method named **from_numeral**, is:

    Str

**Hint:** For this to work, the type would need to have a method named **from_numeral** associated with it in the type's declaration.

**MISSING METHOD**
The value before this **+** operator has a type that doesn't have a **plus** method:
**lambda_annotation_mismatch_error.md:3:23:3:29:**
```roc
string_function = |x| x + 42
```
                      ^^^^^^

The value's type, which does not have a method named **plus**, is:

    Str

**Hint:**The **+** operator calls a method named **plus** on the value preceding it, passing the value after the operator as the one argument.

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
				(ty-lookup (name "Str") (builtin))
				(ty-lookup (name "Str") (builtin)))))
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
		(patt (type "Str -> Error"))
		(patt (type "I64 -> I64")))
	(expressions
		(expr (type "Str -> Error"))
		(expr (type "I64 -> I64"))))
~~~
