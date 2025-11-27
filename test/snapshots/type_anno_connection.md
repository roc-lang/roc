# META
~~~ini
description=Type annotation connection to definitions
type=snippet
~~~
# SOURCE
~~~roc
add_one : U64 -> U64
add_one = |x| x + 1

my_number : U64
my_number = add_one(42)
~~~
# EXPECTED
NIL
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**type_anno_connection.md:2:19:2:20:**
```roc
add_one = |x| x + 1
```
                  ^

It has the type:
    _Numeral_

But I expected it to be:
    _Num.Numeral_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**type_anno_connection.md:2:19:2:20:**
```roc
add_one = |x| x + 1
```
                  ^

It has the type:
    _Try(U64, [InvalidNumeral(Str)])_

But I expected it to be:
    _Try(U64, [InvalidNumeral(Str)])_

# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Int,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "add_one")
			(ty-fn
				(ty (name "U64"))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "add_one"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-binop (op "+")
					(e-ident (raw "x"))
					(e-int (raw "1")))))
		(s-type-anno (name "my_number")
			(ty (name "U64")))
		(s-decl
			(p-ident (raw "my_number"))
			(e-apply
				(e-ident (raw "add_one"))
				(e-int (raw "42"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "add_one"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-binop (op "add")
				(e-lookup-local
					(p-assign (ident "x")))
				(e-num (value "1"))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "U64") (builtin))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "my_number"))
		(e-call
			(e-lookup-local
				(p-assign (ident "add_one")))
			(e-num (value "42")))
		(annotation
			(ty-lookup (name "U64") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error -> Error"))
		(patt (type "Error")))
	(expressions
		(expr (type "Error -> Error"))
		(expr (type "Error"))))
~~~
