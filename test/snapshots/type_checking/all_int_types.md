# META
~~~ini
description=All integer type annotations
type=snippet
~~~
# SOURCE
~~~roc
a : U8
a = 255

b : U16
b = 65535

c : U32
c = 4294967295

d : U64
d = 18446744073709551615

e : U128
e = 340282366920938463463374607431768211455

f : I8
f = -128

g : I16
g = -32768

h : I32
h = -2147483648

i : I64
i = -9223372036854775808

j : I128
j = -170141183460469231731687303715884105728
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "a")
			(ty (name "U8")))
		(s-decl
			(p-ident (raw "a"))
			(e-int (raw "255")))
		(s-type-anno (name "b")
			(ty (name "U16")))
		(s-decl
			(p-ident (raw "b"))
			(e-int (raw "65535")))
		(s-type-anno (name "c")
			(ty (name "U32")))
		(s-decl
			(p-ident (raw "c"))
			(e-int (raw "4294967295")))
		(s-type-anno (name "d")
			(ty (name "U64")))
		(s-decl
			(p-ident (raw "d"))
			(e-int (raw "18446744073709551615")))
		(s-type-anno (name "e")
			(ty (name "U128")))
		(s-decl
			(p-ident (raw "e"))
			(e-int (raw "340282366920938463463374607431768211455")))
		(s-type-anno (name "f")
			(ty (name "I8")))
		(s-decl
			(p-ident (raw "f"))
			(e-int (raw "-128")))
		(s-type-anno (name "g")
			(ty (name "I16")))
		(s-decl
			(p-ident (raw "g"))
			(e-int (raw "-32768")))
		(s-type-anno (name "h")
			(ty (name "I32")))
		(s-decl
			(p-ident (raw "h"))
			(e-int (raw "-2147483648")))
		(s-type-anno (name "i")
			(ty (name "I64")))
		(s-decl
			(p-ident (raw "i"))
			(e-int (raw "-9223372036854775808")))
		(s-type-anno (name "j")
			(ty (name "I128")))
		(s-decl
			(p-ident (raw "j"))
			(e-int (raw "-170141183460469231731687303715884105728")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "a"))
		(e-num (value "255"))
		(annotation
			(ty-lookup (name "U8") (builtin))))
	(d-let
		(p-assign (ident "b"))
		(e-num (value "65535"))
		(annotation
			(ty-lookup (name "U16") (builtin))))
	(d-let
		(p-assign (ident "c"))
		(e-num (value "4294967295"))
		(annotation
			(ty-lookup (name "U32") (builtin))))
	(d-let
		(p-assign (ident "d"))
		(e-num (value "18446744073709551615"))
		(annotation
			(ty-lookup (name "U64") (builtin))))
	(d-let
		(p-assign (ident "e"))
		(e-num (value "340282366920938463463374607431768211455"))
		(annotation
			(ty-lookup (name "U128") (builtin))))
	(d-let
		(p-assign (ident "f"))
		(e-num (value "-128"))
		(annotation
			(ty-lookup (name "I8") (builtin))))
	(d-let
		(p-assign (ident "g"))
		(e-num (value "-32768"))
		(annotation
			(ty-lookup (name "I16") (builtin))))
	(d-let
		(p-assign (ident "h"))
		(e-num (value "-2147483648"))
		(annotation
			(ty-lookup (name "I32") (builtin))))
	(d-let
		(p-assign (ident "i"))
		(e-num (value "-9223372036854775808"))
		(annotation
			(ty-lookup (name "I64") (builtin))))
	(d-let
		(p-assign (ident "j"))
		(e-num (value "-170141183460469231731687303715884105728"))
		(annotation
			(ty-lookup (name "I128") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "U8"))
		(patt (type "U16"))
		(patt (type "U32"))
		(patt (type "U64"))
		(patt (type "U128"))
		(patt (type "I8"))
		(patt (type "I16"))
		(patt (type "I32"))
		(patt (type "I64"))
		(patt (type "I128")))
	(expressions
		(expr (type "U8"))
		(expr (type "U16"))
		(expr (type "U32"))
		(expr (type "U64"))
		(expr (type "U128"))
		(expr (type "I8"))
		(expr (type "I16"))
		(expr (type "I32"))
		(expr (type "I64"))
		(expr (type "I128"))))
~~~
