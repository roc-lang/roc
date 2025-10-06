# META
~~~ini
description=fuzz crash
type=file:FuzzCrash025.roc
~~~
# SOURCE
~~~roc
FuzzCrash025 := {}

a : U8
a = 255

b : U16
b = 65535

c : U32
c = 429496729 U64
d = 18446744073709551615

e : U128
e = 3402823669209384634633746074317682114553.14: I8
f =8

g : I16
g = -32768

h : I32
h = -483648

i : I64
i = -92233725808

j : I128
j = -17011687303715884105728
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_025.md:11:1:11:2
PARSE ERROR - fuzz_crash_025.md:11:3:11:4
PARSE ERROR - fuzz_crash_025.md:11:5:11:25
PARSE ERROR - fuzz_crash_025.md:14:48:14:49
PARSE ERROR - fuzz_crash_025.md:15:1:15:2
PARSE ERROR - fuzz_crash_025.md:15:3:15:4
PARSE ERROR - fuzz_crash_025.md:15:4:15:5
TYPE MISMATCH - fuzz_crash_025.md:14:5:14:48
# PROBLEMS
**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Result(a, Str)`
    `Maybe(List(U64))`

**fuzz_crash_025.md:11:1:11:2:**
```roc
d = 18446744073709551615
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_025.md:11:3:11:4:**
```roc
d = 18446744073709551615
```
  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_025.md:11:5:11:25:**
```roc
d = 18446744073709551615
```
    ^^^^^^^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_025.md:14:48:14:49:**
```roc
e = 3402823669209384634633746074317682114553.14: I8
```
                                               ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Result(a, Str)`
    `Maybe(List(U64))`

**fuzz_crash_025.md:15:1:15:2:**
```roc
f =8
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_025.md:15:3:15:4:**
```roc
f =8
```
  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_025.md:15:4:15:5:**
```roc
f =8
```
   ^


**TYPE MISMATCH**
This expression is used in an unexpected way:
**fuzz_crash_025.md:14:5:14:48:**
```roc
e = 3402823669209384634633746074317682114553.14: I8
```
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

It has the type:
    _Num(Frac(_size))_

But the type annotation says it should have the type:
    _Num(Int(Unsigned128))_

**Hint:** This might be because the numeric literal is either negative or too large to fit in the unsigned type.

# TOKENS
~~~zig
UpperIdent(1:1-1:13),OpColonEqual(1:14-1:16),OpenCurly(1:17-1:18),CloseCurly(1:18-1:19),
LowerIdent(3:1-3:2),OpColon(3:3-3:4),UpperIdent(3:5-3:7),
LowerIdent(4:1-4:2),OpAssign(4:3-4:4),Int(4:5-4:8),
LowerIdent(6:1-6:2),OpColon(6:3-6:4),UpperIdent(6:5-6:8),
LowerIdent(7:1-7:2),OpAssign(7:3-7:4),Int(7:5-7:10),
LowerIdent(9:1-9:2),OpColon(9:3-9:4),UpperIdent(9:5-9:8),
LowerIdent(10:1-10:2),OpAssign(10:3-10:4),Int(10:5-10:14),UpperIdent(10:15-10:18),
LowerIdent(11:1-11:2),OpAssign(11:3-11:4),Int(11:5-11:25),
LowerIdent(13:1-13:2),OpColon(13:3-13:4),UpperIdent(13:5-13:9),
LowerIdent(14:1-14:2),OpAssign(14:3-14:4),Float(14:5-14:48),OpColon(14:48-14:49),UpperIdent(14:50-14:52),
LowerIdent(15:1-15:2),OpAssign(15:3-15:4),Int(15:4-15:5),
LowerIdent(17:1-17:2),OpColon(17:3-17:4),UpperIdent(17:5-17:8),
LowerIdent(18:1-18:2),OpAssign(18:3-18:4),Int(18:5-18:11),
LowerIdent(20:1-20:2),OpColon(20:3-20:4),UpperIdent(20:5-20:8),
LowerIdent(21:1-21:2),OpAssign(21:3-21:4),Int(21:5-21:12),
LowerIdent(23:1-23:2),OpColon(23:3-23:4),UpperIdent(23:5-23:8),
LowerIdent(24:1-24:2),OpAssign(24:3-24:4),Int(24:5-24:17),
LowerIdent(26:1-26:2),OpColon(26:3-26:4),UpperIdent(26:5-26:9),
LowerIdent(27:1-27:2),OpAssign(27:3-27:4),Int(27:5-27:29),
EndOfFile(28:1-28:1),
~~~
# PARSE
~~~clojure
(file @1.1-27.29
	(type-module @1.1-1.13)
	(statements
		(s-type-decl @1.1-1.19
			(header @1.1-1.13 (name "FuzzCrash025")
				(args))
			(ty-record @1.17-1.19))
		(s-type-anno @3.1-3.7 (name "a")
			(ty @3.5-3.7 (name "U8")))
		(s-decl @4.1-4.8
			(p-ident @4.1-4.2 (raw "a"))
			(e-int @4.5-4.8 (raw "255")))
		(s-type-anno @6.1-6.8 (name "b")
			(ty @6.5-6.8 (name "U16")))
		(s-decl @7.1-7.10
			(p-ident @7.1-7.2 (raw "b"))
			(e-int @7.5-7.10 (raw "65535")))
		(s-type-anno @9.1-9.8 (name "c")
			(ty @9.5-9.8 (name "U32")))
		(s-decl @10.1-10.14
			(p-ident @10.1-10.2 (raw "c"))
			(e-int @10.5-10.14 (raw "429496729")))
		(s-malformed @11.1-11.2 (tag "expected_colon_after_type_annotation"))
		(s-malformed @11.3-11.4 (tag "statement_unexpected_token"))
		(s-malformed @11.5-11.25 (tag "statement_unexpected_token"))
		(s-type-anno @13.1-13.9 (name "e")
			(ty @13.5-13.9 (name "U128")))
		(s-decl @14.1-14.48
			(p-ident @14.1-14.2 (raw "e"))
			(e-frac @14.5-14.48 (raw "3402823669209384634633746074317682114553.14")))
		(s-malformed @14.48-14.49 (tag "statement_unexpected_token"))
		(s-malformed @15.1-15.2 (tag "expected_colon_after_type_annotation"))
		(s-malformed @15.3-15.4 (tag "statement_unexpected_token"))
		(s-malformed @15.4-15.5 (tag "statement_unexpected_token"))
		(s-type-anno @17.1-17.8 (name "g")
			(ty @17.5-17.8 (name "I16")))
		(s-decl @18.1-18.11
			(p-ident @18.1-18.2 (raw "g"))
			(e-int @18.5-18.11 (raw "-32768")))
		(s-type-anno @20.1-20.8 (name "h")
			(ty @20.5-20.8 (name "I32")))
		(s-decl @21.1-21.12
			(p-ident @21.1-21.2 (raw "h"))
			(e-int @21.5-21.12 (raw "-483648")))
		(s-type-anno @23.1-23.8 (name "i")
			(ty @23.5-23.8 (name "I64")))
		(s-decl @24.1-24.17
			(p-ident @24.1-24.2 (raw "i"))
			(e-int @24.5-24.17 (raw "-92233725808")))
		(s-type-anno @26.1-26.9 (name "j")
			(ty @26.5-26.9 (name "I128")))
		(s-decl @27.1-27.29
			(p-ident @27.1-27.2 (raw "j"))
			(e-int @27.5-27.29 (raw "-17011687303715884105728")))))
~~~
# FORMATTED
~~~roc
FuzzCrash025 := {}

a : U8
a = 255

b : U16
b = 65535

c : U32
c = 429496729


e : U128
e = 3402823669209384634633746074317682114553.14



g : I16
g = -32768

h : I32
h = -483648

i : I64
i = -92233725808

j : I128
j = -17011687303715884105728
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.2 (ident "a"))
		(e-num @4.5-4.8 (value "255"))
		(annotation @4.1-4.2
			(declared-type
				(ty-lookup @3.5-3.7 (name "U8") (builtin)))))
	(d-let
		(p-assign @7.1-7.2 (ident "b"))
		(e-num @7.5-7.10 (value "65535"))
		(annotation @7.1-7.2
			(declared-type
				(ty-lookup @6.5-6.8 (name "U16") (builtin)))))
	(d-let
		(p-assign @10.1-10.2 (ident "c"))
		(e-num @10.5-10.14 (value "429496729"))
		(annotation @10.1-10.2
			(declared-type
				(ty-lookup @9.5-9.8 (name "U32") (builtin)))))
	(d-let
		(p-assign @14.1-14.2 (ident "e"))
		(e-frac-f64 @14.5-14.48 (value "3.4028236692093846e39"))
		(annotation @14.1-14.2
			(declared-type
				(ty-lookup @13.5-13.9 (name "U128") (builtin)))))
	(d-let
		(p-assign @18.1-18.2 (ident "g"))
		(e-num @18.5-18.11 (value "-32768"))
		(annotation @18.1-18.2
			(declared-type
				(ty-lookup @17.5-17.8 (name "I16") (builtin)))))
	(d-let
		(p-assign @21.1-21.2 (ident "h"))
		(e-num @21.5-21.12 (value "-483648"))
		(annotation @21.1-21.2
			(declared-type
				(ty-lookup @20.5-20.8 (name "I32") (builtin)))))
	(d-let
		(p-assign @24.1-24.2 (ident "i"))
		(e-num @24.5-24.17 (value "-92233725808"))
		(annotation @24.1-24.2
			(declared-type
				(ty-lookup @23.5-23.8 (name "I64") (builtin)))))
	(d-let
		(p-assign @27.1-27.2 (ident "j"))
		(e-num @27.5-27.29 (value "-17011687303715884105728"))
		(annotation @27.1-27.2
			(declared-type
				(ty-lookup @26.5-26.9 (name "I128") (builtin)))))
	(s-nominal-decl @1.1-1.19
		(ty-header @1.1-1.13 (name "FuzzCrash025"))
		(ty-record @1.17-1.19)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.2 (type "Num(Int(Unsigned8))"))
		(patt @7.1-7.2 (type "Num(Int(Unsigned16))"))
		(patt @10.1-10.2 (type "Num(Int(Unsigned32))"))
		(patt @14.1-14.2 (type "Error"))
		(patt @18.1-18.2 (type "Num(Int(Signed16))"))
		(patt @21.1-21.2 (type "Num(Int(Signed32))"))
		(patt @24.1-24.2 (type "Num(Int(Signed64))"))
		(patt @27.1-27.2 (type "Num(Int(Signed128))")))
	(type_decls
		(nominal @1.1-1.19 (type "FuzzCrash025")
			(ty-header @1.1-1.13 (name "FuzzCrash025"))))
	(expressions
		(expr @4.5-4.8 (type "Num(Int(Unsigned8))"))
		(expr @7.5-7.10 (type "Num(Int(Unsigned16))"))
		(expr @10.5-10.14 (type "Num(Int(Unsigned32))"))
		(expr @14.5-14.48 (type "Error"))
		(expr @18.5-18.11 (type "Num(Int(Signed16))"))
		(expr @21.5-21.12 (type "Num(Int(Signed32))"))
		(expr @24.5-24.17 (type "Num(Int(Signed64))"))
		(expr @27.5-27.29 (type "Num(Int(Signed128))"))))
~~~
