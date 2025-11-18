# META
~~~ini
description=Simple lambda constraint success test - verifies bidirectional type checking works correctly
type=snippet
~~~
# SOURCE
~~~roc
# Should successfully constrain literal 2 to I64
addTwo : I64 -> I64
addTwo = |x| x + 2

# Should successfully constrain literal 2.0 to F64
addTwoF64 : F64 -> F64
addTwoF64 = |x| x + 2.0
~~~
# EXPECTED
MISSING METHOD - simple_lambda_constraint_success.md:3:14:3:19
+ - :0:0:0:0
MISSING METHOD - simple_lambda_constraint_success.md:7:17:7:24
+ - :0:0:0:0
# PROBLEMS
**MISSING METHOD**
The value before this **+** operator has the type **I64**, which has no **plus** method:
**simple_lambda_constraint_success.md:3:14:3:19:**
```roc
addTwo = |x| x + 2
```
             ^^^^^


**Hint: **The **+** operator calls a method named **plus** on the value preceding it, passing the value after the operator as the one argument.

**MISSING METHOD**
The value before this **+** operator has the type **F64**, which has no **plus** method:
**simple_lambda_constraint_success.md:7:17:7:24:**
```roc
addTwoF64 = |x| x + 2.0
```
                ^^^^^^^


**Hint: **The **+** operator calls a method named **plus** on the value preceding it, passing the value after the operator as the one argument.

# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Int,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Float,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "addTwo")
			(ty-fn
				(ty (name "I64"))
				(ty (name "I64"))))
		(s-decl
			(p-ident (raw "addTwo"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-binop (op "+")
					(e-ident (raw "x"))
					(e-int (raw "2")))))
		(s-type-anno (name "addTwoF64")
			(ty-fn
				(ty (name "F64"))
				(ty (name "F64"))))
		(s-decl
			(p-ident (raw "addTwoF64"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-binop (op "+")
					(e-ident (raw "x"))
					(e-frac (raw "2.0")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "addTwo"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-binop (op "add")
				(e-lookup-local
					(p-assign (ident "x")))
				(e-num (value "2"))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "I64") (builtin))
				(ty-lookup (name "I64") (builtin)))))
	(d-let
		(p-assign (ident "addTwoF64"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-binop (op "add")
				(e-lookup-local
					(p-assign (ident "x")))
				(e-dec-small (numerator "20") (denominator-power-of-ten "1") (value "2"))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "F64") (builtin))
				(ty-lookup (name "F64") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "I64 -> Error"))
		(patt (type "F64 -> Error")))
	(expressions
		(expr (type "I64 -> Error"))
		(expr (type "F64 -> Error"))))
~~~
