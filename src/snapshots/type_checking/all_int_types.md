# META
~~~ini
description=All integer type annotations
type=file
~~~
# SOURCE
~~~roc
module []

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
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:2),OpColon(3:3-3:4),UpperIdent(3:5-3:7),Newline(1:1-1:1),
LowerIdent(4:1-4:2),OpAssign(4:3-4:4),Int(4:5-4:8),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(6:1-6:2),OpColon(6:3-6:4),UpperIdent(6:5-6:8),Newline(1:1-1:1),
LowerIdent(7:1-7:2),OpAssign(7:3-7:4),Int(7:5-7:10),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(9:1-9:2),OpColon(9:3-9:4),UpperIdent(9:5-9:8),Newline(1:1-1:1),
LowerIdent(10:1-10:2),OpAssign(10:3-10:4),Int(10:5-10:15),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(12:1-12:2),OpColon(12:3-12:4),UpperIdent(12:5-12:8),Newline(1:1-1:1),
LowerIdent(13:1-13:2),OpAssign(13:3-13:4),Int(13:5-13:25),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(15:1-15:2),OpColon(15:3-15:4),UpperIdent(15:5-15:9),Newline(1:1-1:1),
LowerIdent(16:1-16:2),OpAssign(16:3-16:4),Int(16:5-16:44),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(18:1-18:2),OpColon(18:3-18:4),UpperIdent(18:5-18:7),Newline(1:1-1:1),
LowerIdent(19:1-19:2),OpAssign(19:3-19:4),Int(19:5-19:9),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(21:1-21:2),OpColon(21:3-21:4),UpperIdent(21:5-21:8),Newline(1:1-1:1),
LowerIdent(22:1-22:2),OpAssign(22:3-22:4),Int(22:5-22:11),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(24:1-24:2),OpColon(24:3-24:4),UpperIdent(24:5-24:8),Newline(1:1-1:1),
LowerIdent(25:1-25:2),OpAssign(25:3-25:4),Int(25:5-25:16),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(27:1-27:2),OpColon(27:3-27:4),UpperIdent(27:5-27:8),Newline(1:1-1:1),
LowerIdent(28:1-28:2),OpAssign(28:3-28:4),Int(28:5-28:25),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(30:1-30:2),OpColon(30:3-30:4),UpperIdent(30:5-30:9),Newline(1:1-1:1),
LowerIdent(31:1-31:2),OpAssign(31:3-31:4),Int(31:5-31:45),EndOfFile(31:45-31:45),
~~~
# PARSE
~~~clojure
(file @1-1-31-45
	(module @1-1-1-10
		(exposes @1-8-1-10))
	(statements
		(s-type-anno @3-1-4-2 (name "a")
			(ty (name "U8")))
		(s-decl @4-1-4-8
			(p-ident @4-1-4-2 (raw "a"))
			(e-int @4-5-4-8 (raw "255")))
		(s-type-anno @6-1-7-2 (name "b")
			(ty (name "U16")))
		(s-decl @7-1-7-10
			(p-ident @7-1-7-2 (raw "b"))
			(e-int @7-5-7-10 (raw "65535")))
		(s-type-anno @9-1-10-2 (name "c")
			(ty (name "U32")))
		(s-decl @10-1-10-15
			(p-ident @10-1-10-2 (raw "c"))
			(e-int @10-5-10-15 (raw "4294967295")))
		(s-type-anno @12-1-13-2 (name "d")
			(ty (name "U64")))
		(s-decl @13-1-13-25
			(p-ident @13-1-13-2 (raw "d"))
			(e-int @13-5-13-25 (raw "18446744073709551615")))
		(s-type-anno @15-1-16-2 (name "e")
			(ty (name "U128")))
		(s-decl @16-1-16-44
			(p-ident @16-1-16-2 (raw "e"))
			(e-int @16-5-16-44 (raw "340282366920938463463374607431768211455")))
		(s-type-anno @18-1-19-2 (name "f")
			(ty (name "I8")))
		(s-decl @19-1-19-9
			(p-ident @19-1-19-2 (raw "f"))
			(e-int @19-5-19-9 (raw "-128")))
		(s-type-anno @21-1-22-2 (name "g")
			(ty (name "I16")))
		(s-decl @22-1-22-11
			(p-ident @22-1-22-2 (raw "g"))
			(e-int @22-5-22-11 (raw "-32768")))
		(s-type-anno @24-1-25-2 (name "h")
			(ty (name "I32")))
		(s-decl @25-1-25-16
			(p-ident @25-1-25-2 (raw "h"))
			(e-int @25-5-25-16 (raw "-2147483648")))
		(s-type-anno @27-1-28-2 (name "i")
			(ty (name "I64")))
		(s-decl @28-1-28-25
			(p-ident @28-1-28-2 (raw "i"))
			(e-int @28-5-28-25 (raw "-9223372036854775808")))
		(s-type-anno @30-1-31-2 (name "j")
			(ty (name "I128")))
		(s-decl @31-1-31-45
			(p-ident @31-1-31-2 (raw "j"))
			(e-int @31-5-31-45 (raw "-170141183460469231731687303715884105728")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let (id 79)
		(p-assign @4-1-4-2 (ident "a") (id 73))
		(e-int @4-5-4-8 (num-var 75) (value "255") (id 75))
		(annotation @4-1-4-2 (signature 77) (id 78)
			(declared-type
				(ty @3-5-3-7 (name "U8")))))
	(d-let (id 87)
		(p-assign @7-1-7-2 (ident "b") (id 81))
		(e-int @7-5-7-10 (num-var 83) (value "65535") (id 83))
		(annotation @7-1-7-2 (signature 85) (id 86)
			(declared-type
				(ty @6-5-6-8 (name "U16")))))
	(d-let (id 95)
		(p-assign @10-1-10-2 (ident "c") (id 89))
		(e-int @10-5-10-15 (num-var 91) (value "4294967295") (id 91))
		(annotation @10-1-10-2 (signature 93) (id 94)
			(declared-type
				(ty @9-5-9-8 (name "U32")))))
	(d-let (id 103)
		(p-assign @13-1-13-2 (ident "d") (id 97))
		(e-int @13-5-13-25 (num-var 99) (value "18446744073709551615") (id 99))
		(annotation @13-1-13-2 (signature 101) (id 102)
			(declared-type
				(ty @12-5-12-8 (name "U64")))))
	(d-let (id 111)
		(p-assign @16-1-16-2 (ident "e") (id 105))
		(e-int @16-5-16-44 (num-var 107) (value "-1") (id 107))
		(annotation @16-1-16-2 (signature 109) (id 110)
			(declared-type
				(ty @15-5-15-9 (name "U128")))))
	(d-let (id 119)
		(p-assign @19-1-19-2 (ident "f") (id 113))
		(e-int @19-5-19-9 (num-var 115) (value "-128") (id 115))
		(annotation @19-1-19-2 (signature 117) (id 118)
			(declared-type
				(ty @18-5-18-7 (name "I8")))))
	(d-let (id 127)
		(p-assign @22-1-22-2 (ident "g") (id 121))
		(e-int @22-5-22-11 (num-var 123) (value "-32768") (id 123))
		(annotation @22-1-22-2 (signature 125) (id 126)
			(declared-type
				(ty @21-5-21-8 (name "I16")))))
	(d-let (id 135)
		(p-assign @25-1-25-2 (ident "h") (id 129))
		(e-int @25-5-25-16 (num-var 131) (value "-2147483648") (id 131))
		(annotation @25-1-25-2 (signature 133) (id 134)
			(declared-type
				(ty @24-5-24-8 (name "I32")))))
	(d-let (id 143)
		(p-assign @28-1-28-2 (ident "i") (id 137))
		(e-int @28-5-28-25 (num-var 139) (value "-9223372036854775808") (id 139))
		(annotation @28-1-28-2 (signature 141) (id 142)
			(declared-type
				(ty @27-5-27-8 (name "I64")))))
	(d-let (id 151)
		(p-assign @31-1-31-2 (ident "j") (id 145))
		(e-int @31-5-31-45 (num-var 147) (value "-170141183460469231731687303715884105728") (id 147))
		(annotation @31-1-31-2 (signature 149) (id 150)
			(declared-type
				(ty @30-5-30-9 (name "I128"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(def (name "a") (type "U8"))
		(def (name "b") (type "U16"))
		(def (name "c") (type "U32"))
		(def (name "d") (type "U64"))
		(def (name "e") (type "U128"))
		(def (name "f") (type "I8"))
		(def (name "g") (type "I16"))
		(def (name "h") (type "I32"))
		(def (name "i") (type "I64"))
		(def (name "j") (type "I128")))
	(expressions
		(expr @4-5-4-8 (type "U8"))
		(expr @7-5-7-10 (type "U16"))
		(expr @10-5-10-15 (type "U32"))
		(expr @13-5-13-25 (type "U64"))
		(expr @16-5-16-44 (type "U128"))
		(expr @19-5-19-9 (type "I8"))
		(expr @22-5-22-11 (type "I16"))
		(expr @25-5-25-16 (type "I32"))
		(expr @28-5-28-25 (type "I64"))
		(expr @31-5-31-45 (type "I128"))))
~~~