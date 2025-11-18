# META
~~~ini
description=Simple addition function with expect statement
type=snippet
~~~
# SOURCE
~~~roc
addU8 : U8, U8 -> U8
addU8 = |a, b| a + b

expect addU8(1, 2) == 3
expect addU8(0, 10) == 10
~~~
# EXPECTED
MISSING METHOD - simple_add.md:2:16:2:21
+ - :0:0:0:0
# PROBLEMS
**MISSING METHOD**
The value before this **+** operator has the type **U8**, which has no **plus** method:
**simple_add.md:2:16:2:21:**
```roc
addU8 = |a, b| a + b
```
               ^^^^^


**Hint: **The **+** operator calls a method named **plus** on the value preceding it, passing the value after the operator as the one argument.

# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,
KwExpect,LowerIdent,NoSpaceOpenRound,Int,Comma,Int,CloseRound,OpEquals,Int,
KwExpect,LowerIdent,NoSpaceOpenRound,Int,Comma,Int,CloseRound,OpEquals,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "addU8")
			(ty-fn
				(ty (name "U8"))
				(ty (name "U8"))
				(ty (name "U8"))))
		(s-decl
			(p-ident (raw "addU8"))
			(e-lambda
				(args
					(p-ident (raw "a"))
					(p-ident (raw "b")))
				(e-binop (op "+")
					(e-ident (raw "a"))
					(e-ident (raw "b")))))
		(s-expect
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "addU8"))
					(e-int (raw "1"))
					(e-int (raw "2")))
				(e-int (raw "3"))))
		(s-expect
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "addU8"))
					(e-int (raw "0"))
					(e-int (raw "10")))
				(e-int (raw "10"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "addU8"))
		(e-lambda
			(args
				(p-assign (ident "a"))
				(p-assign (ident "b")))
			(e-binop (op "add")
				(e-lookup-local
					(p-assign (ident "a")))
				(e-lookup-local
					(p-assign (ident "b")))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "U8") (builtin))
				(ty-lookup (name "U8") (builtin))
				(ty-lookup (name "U8") (builtin)))))
	(s-expect
		(e-binop (op "eq")
			(e-call
				(e-lookup-local
					(p-assign (ident "addU8")))
				(e-num (value "1"))
				(e-num (value "2")))
			(e-num (value "3"))))
	(s-expect
		(e-binop (op "eq")
			(e-call
				(e-lookup-local
					(p-assign (ident "addU8")))
				(e-num (value "0"))
				(e-num (value "10")))
			(e-num (value "10")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "U8, U8 -> Error")))
	(expressions
		(expr (type "U8, U8 -> Error"))))
~~~
