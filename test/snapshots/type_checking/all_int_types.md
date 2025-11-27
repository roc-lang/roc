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
TYPE MISMATCH - all_int_types.md:2:5:2:8
TYPE MISMATCH - all_int_types.md:2:5:2:8
TYPE MISMATCH - all_int_types.md:5:5:5:10
TYPE MISMATCH - all_int_types.md:5:5:5:10
TYPE MISMATCH - all_int_types.md:8:5:8:15
TYPE MISMATCH - all_int_types.md:8:5:8:15
TYPE MISMATCH - all_int_types.md:11:5:11:25
TYPE MISMATCH - all_int_types.md:11:5:11:25
TYPE MISMATCH - all_int_types.md:14:5:14:44
TYPE MISMATCH - all_int_types.md:14:5:14:44
TYPE MISMATCH - all_int_types.md:17:5:17:9
TYPE MISMATCH - all_int_types.md:17:5:17:9
TYPE MISMATCH - all_int_types.md:20:5:20:11
TYPE MISMATCH - all_int_types.md:20:5:20:11
TYPE MISMATCH - all_int_types.md:23:5:23:16
TYPE MISMATCH - all_int_types.md:23:5:23:16
TYPE MISMATCH - all_int_types.md:26:5:26:25
TYPE MISMATCH - all_int_types.md:26:5:26:25
TYPE MISMATCH - all_int_types.md:29:5:29:45
TYPE MISMATCH - all_int_types.md:29:5:29:45
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**all_int_types.md:2:5:2:8:**
```roc
a = 255
```
    ^^^

It has the type:
    _Numeral_

But I expected it to be:
    _Num.Numeral_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**all_int_types.md:2:5:2:8:**
```roc
a = 255
```
    ^^^

It has the type:
    _Try(U8, [InvalidNumeral(Str)])_

But I expected it to be:
    _Try(U8, [InvalidNumeral(Str)])_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**all_int_types.md:5:5:5:10:**
```roc
b = 65535
```
    ^^^^^

It has the type:
    _Numeral_

But I expected it to be:
    _Num.Numeral_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**all_int_types.md:5:5:5:10:**
```roc
b = 65535
```
    ^^^^^

It has the type:
    _Try(U16, [InvalidNumeral(Str)])_

But I expected it to be:
    _Try(U16, [InvalidNumeral(Str)])_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**all_int_types.md:8:5:8:15:**
```roc
c = 4294967295
```
    ^^^^^^^^^^

It has the type:
    _Numeral_

But I expected it to be:
    _Num.Numeral_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**all_int_types.md:8:5:8:15:**
```roc
c = 4294967295
```
    ^^^^^^^^^^

It has the type:
    _Try(U32, [InvalidNumeral(Str)])_

But I expected it to be:
    _Try(U32, [InvalidNumeral(Str)])_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**all_int_types.md:11:5:11:25:**
```roc
d = 18446744073709551615
```
    ^^^^^^^^^^^^^^^^^^^^

It has the type:
    _Numeral_

But I expected it to be:
    _Num.Numeral_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**all_int_types.md:11:5:11:25:**
```roc
d = 18446744073709551615
```
    ^^^^^^^^^^^^^^^^^^^^

It has the type:
    _Try(U64, [InvalidNumeral(Str)])_

But I expected it to be:
    _Try(U64, [InvalidNumeral(Str)])_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**all_int_types.md:14:5:14:44:**
```roc
e = 340282366920938463463374607431768211455
```
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

It has the type:
    _Numeral_

But I expected it to be:
    _Num.Numeral_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**all_int_types.md:14:5:14:44:**
```roc
e = 340282366920938463463374607431768211455
```
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

It has the type:
    _Try(U128, [InvalidNumeral(Str)])_

But I expected it to be:
    _Try(U128, [InvalidNumeral(Str)])_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**all_int_types.md:17:5:17:9:**
```roc
f = -128
```
    ^^^^

It has the type:
    _Numeral_

But I expected it to be:
    _Num.Numeral_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**all_int_types.md:17:5:17:9:**
```roc
f = -128
```
    ^^^^

It has the type:
    _Try(I8, [InvalidNumeral(Str)])_

But I expected it to be:
    _Try(I8, [InvalidNumeral(Str)])_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**all_int_types.md:20:5:20:11:**
```roc
g = -32768
```
    ^^^^^^

It has the type:
    _Numeral_

But I expected it to be:
    _Num.Numeral_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**all_int_types.md:20:5:20:11:**
```roc
g = -32768
```
    ^^^^^^

It has the type:
    _Try(I16, [InvalidNumeral(Str)])_

But I expected it to be:
    _Try(I16, [InvalidNumeral(Str)])_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**all_int_types.md:23:5:23:16:**
```roc
h = -2147483648
```
    ^^^^^^^^^^^

It has the type:
    _Numeral_

But I expected it to be:
    _Num.Numeral_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**all_int_types.md:23:5:23:16:**
```roc
h = -2147483648
```
    ^^^^^^^^^^^

It has the type:
    _Try(I32, [InvalidNumeral(Str)])_

But I expected it to be:
    _Try(I32, [InvalidNumeral(Str)])_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**all_int_types.md:26:5:26:25:**
```roc
i = -9223372036854775808
```
    ^^^^^^^^^^^^^^^^^^^^

It has the type:
    _Numeral_

But I expected it to be:
    _Num.Numeral_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**all_int_types.md:26:5:26:25:**
```roc
i = -9223372036854775808
```
    ^^^^^^^^^^^^^^^^^^^^

It has the type:
    _Try(I64, [InvalidNumeral(Str)])_

But I expected it to be:
    _Try(I64, [InvalidNumeral(Str)])_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**all_int_types.md:29:5:29:45:**
```roc
j = -170141183460469231731687303715884105728
```
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

It has the type:
    _Numeral_

But I expected it to be:
    _Num.Numeral_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**all_int_types.md:29:5:29:45:**
```roc
j = -170141183460469231731687303715884105728
```
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

It has the type:
    _Try(I128, [InvalidNumeral(Str)])_

But I expected it to be:
    _Try(I128, [InvalidNumeral(Str)])_

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
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))))
~~~
