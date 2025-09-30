# META
~~~ini
description=All integer type annotations
type=file
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
MISSING MAIN! FUNCTION - all_int_types.md:1:1:29:45
# PROBLEMS
**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**all_int_types.md:1:1:29:45:**
```roc
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
```


# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:3-1:4),UpperIdent(1:5-1:7),
LowerIdent(2:1-2:2),OpAssign(2:3-2:4),Int(2:5-2:8),
LowerIdent(4:1-4:2),OpColon(4:3-4:4),UpperIdent(4:5-4:8),
LowerIdent(5:1-5:2),OpAssign(5:3-5:4),Int(5:5-5:10),
LowerIdent(7:1-7:2),OpColon(7:3-7:4),UpperIdent(7:5-7:8),
LowerIdent(8:1-8:2),OpAssign(8:3-8:4),Int(8:5-8:15),
LowerIdent(10:1-10:2),OpColon(10:3-10:4),UpperIdent(10:5-10:8),
LowerIdent(11:1-11:2),OpAssign(11:3-11:4),Int(11:5-11:25),
LowerIdent(13:1-13:2),OpColon(13:3-13:4),UpperIdent(13:5-13:9),
LowerIdent(14:1-14:2),OpAssign(14:3-14:4),Int(14:5-14:44),
LowerIdent(16:1-16:2),OpColon(16:3-16:4),UpperIdent(16:5-16:7),
LowerIdent(17:1-17:2),OpAssign(17:3-17:4),Int(17:5-17:9),
LowerIdent(19:1-19:2),OpColon(19:3-19:4),UpperIdent(19:5-19:8),
LowerIdent(20:1-20:2),OpAssign(20:3-20:4),Int(20:5-20:11),
LowerIdent(22:1-22:2),OpColon(22:3-22:4),UpperIdent(22:5-22:8),
LowerIdent(23:1-23:2),OpAssign(23:3-23:4),Int(23:5-23:16),
LowerIdent(25:1-25:2),OpColon(25:3-25:4),UpperIdent(25:5-25:8),
LowerIdent(26:1-26:2),OpAssign(26:3-26:4),Int(26:5-26:25),
LowerIdent(28:1-28:2),OpColon(28:3-28:4),UpperIdent(28:5-28:9),
LowerIdent(29:1-29:2),OpAssign(29:3-29:4),Int(29:5-29:45),
EndOfFile(30:1-30:1),
~~~
# PARSE
~~~clojure
(file @1.1-29.45
	(type-module @1.1-1.2)
	(statements
		(s-type-anno @1.1-1.7 (name "a")
			(ty @1.5-1.7 (name "U8")))
		(s-decl @2.1-2.8
			(p-ident @2.1-2.2 (raw "a"))
			(e-int @2.5-2.8 (raw "255")))
		(s-type-anno @4.1-4.8 (name "b")
			(ty @4.5-4.8 (name "U16")))
		(s-decl @5.1-5.10
			(p-ident @5.1-5.2 (raw "b"))
			(e-int @5.5-5.10 (raw "65535")))
		(s-type-anno @7.1-7.8 (name "c")
			(ty @7.5-7.8 (name "U32")))
		(s-decl @8.1-8.15
			(p-ident @8.1-8.2 (raw "c"))
			(e-int @8.5-8.15 (raw "4294967295")))
		(s-type-anno @10.1-10.8 (name "d")
			(ty @10.5-10.8 (name "U64")))
		(s-decl @11.1-11.25
			(p-ident @11.1-11.2 (raw "d"))
			(e-int @11.5-11.25 (raw "18446744073709551615")))
		(s-type-anno @13.1-13.9 (name "e")
			(ty @13.5-13.9 (name "U128")))
		(s-decl @14.1-14.44
			(p-ident @14.1-14.2 (raw "e"))
			(e-int @14.5-14.44 (raw "340282366920938463463374607431768211455")))
		(s-type-anno @16.1-16.7 (name "f")
			(ty @16.5-16.7 (name "I8")))
		(s-decl @17.1-17.9
			(p-ident @17.1-17.2 (raw "f"))
			(e-int @17.5-17.9 (raw "-128")))
		(s-type-anno @19.1-19.8 (name "g")
			(ty @19.5-19.8 (name "I16")))
		(s-decl @20.1-20.11
			(p-ident @20.1-20.2 (raw "g"))
			(e-int @20.5-20.11 (raw "-32768")))
		(s-type-anno @22.1-22.8 (name "h")
			(ty @22.5-22.8 (name "I32")))
		(s-decl @23.1-23.16
			(p-ident @23.1-23.2 (raw "h"))
			(e-int @23.5-23.16 (raw "-2147483648")))
		(s-type-anno @25.1-25.8 (name "i")
			(ty @25.5-25.8 (name "I64")))
		(s-decl @26.1-26.25
			(p-ident @26.1-26.2 (raw "i"))
			(e-int @26.5-26.25 (raw "-9223372036854775808")))
		(s-type-anno @28.1-28.9 (name "j")
			(ty @28.5-28.9 (name "I128")))
		(s-decl @29.1-29.45
			(p-ident @29.1-29.2 (raw "j"))
			(e-int @29.5-29.45 (raw "-170141183460469231731687303715884105728")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @2.1-2.2 (ident "a"))
		(e-int @2.5-2.8 (value "255"))
		(annotation @2.1-2.2
			(declared-type
				(ty @1.5-1.7 (name "U8")))))
	(d-let
		(p-assign @5.1-5.2 (ident "b"))
		(e-int @5.5-5.10 (value "65535"))
		(annotation @5.1-5.2
			(declared-type
				(ty @4.5-4.8 (name "U16")))))
	(d-let
		(p-assign @8.1-8.2 (ident "c"))
		(e-int @8.5-8.15 (value "4294967295"))
		(annotation @8.1-8.2
			(declared-type
				(ty @7.5-7.8 (name "U32")))))
	(d-let
		(p-assign @11.1-11.2 (ident "d"))
		(e-int @11.5-11.25 (value "18446744073709551615"))
		(annotation @11.1-11.2
			(declared-type
				(ty @10.5-10.8 (name "U64")))))
	(d-let
		(p-assign @14.1-14.2 (ident "e"))
		(e-int @14.5-14.44 (value "-1"))
		(annotation @14.1-14.2
			(declared-type
				(ty @13.5-13.9 (name "U128")))))
	(d-let
		(p-assign @17.1-17.2 (ident "f"))
		(e-int @17.5-17.9 (value "-128"))
		(annotation @17.1-17.2
			(declared-type
				(ty @16.5-16.7 (name "I8")))))
	(d-let
		(p-assign @20.1-20.2 (ident "g"))
		(e-int @20.5-20.11 (value "-32768"))
		(annotation @20.1-20.2
			(declared-type
				(ty @19.5-19.8 (name "I16")))))
	(d-let
		(p-assign @23.1-23.2 (ident "h"))
		(e-int @23.5-23.16 (value "-2147483648"))
		(annotation @23.1-23.2
			(declared-type
				(ty @22.5-22.8 (name "I32")))))
	(d-let
		(p-assign @26.1-26.2 (ident "i"))
		(e-int @26.5-26.25 (value "-9223372036854775808"))
		(annotation @26.1-26.2
			(declared-type
				(ty @25.5-25.8 (name "I64")))))
	(d-let
		(p-assign @29.1-29.2 (ident "j"))
		(e-int @29.5-29.45 (value "-170141183460469231731687303715884105728"))
		(annotation @29.1-29.2
			(declared-type
				(ty @28.5-28.9 (name "I128"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @2.1-2.2 (type "U8"))
		(patt @5.1-5.2 (type "U16"))
		(patt @8.1-8.2 (type "U32"))
		(patt @11.1-11.2 (type "U64"))
		(patt @14.1-14.2 (type "U128"))
		(patt @17.1-17.2 (type "I8"))
		(patt @20.1-20.2 (type "I16"))
		(patt @23.1-23.2 (type "I32"))
		(patt @26.1-26.2 (type "I64"))
		(patt @29.1-29.2 (type "I128")))
	(expressions
		(expr @2.5-2.8 (type "U8"))
		(expr @5.5-5.10 (type "U16"))
		(expr @8.5-8.15 (type "U32"))
		(expr @11.5-11.25 (type "U64"))
		(expr @14.5-14.44 (type "U128"))
		(expr @17.5-17.9 (type "I8"))
		(expr @20.5-20.11 (type "I16"))
		(expr @23.5-23.16 (type "I32"))
		(expr @26.5-26.25 (type "I64"))
		(expr @29.5-29.45 (type "I128"))))
~~~
