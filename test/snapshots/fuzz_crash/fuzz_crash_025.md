# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
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
PARSE ERROR - fuzz_crash_025.md:9:1:9:2
PARSE ERROR - fuzz_crash_025.md:9:3:9:4
PARSE ERROR - fuzz_crash_025.md:9:5:9:25
PARSE ERROR - fuzz_crash_025.md:12:48:12:49
PARSE ERROR - fuzz_crash_025.md:13:1:13:2
PARSE ERROR - fuzz_crash_025.md:13:3:13:4
PARSE ERROR - fuzz_crash_025.md:13:4:13:5
MISSING MAIN! FUNCTION - fuzz_crash_025.md:1:1:25:29
TYPE MISMATCH - fuzz_crash_025.md:12:5:12:48
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

**fuzz_crash_025.md:9:1:9:2:**
```roc
d = 18446744073709551615
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_025.md:9:3:9:4:**
```roc
d = 18446744073709551615
```
  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_025.md:9:5:9:25:**
```roc
d = 18446744073709551615
```
    ^^^^^^^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_025.md:12:48:12:49:**
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

**fuzz_crash_025.md:13:1:13:2:**
```roc
f =8
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_025.md:13:3:13:4:**
```roc
f =8
```
  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_025.md:13:4:13:5:**
```roc
f =8
```
   ^


**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**fuzz_crash_025.md:1:1:25:29:**
```roc
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
```


**TYPE MISMATCH**
This expression is used in an unexpected way:
**fuzz_crash_025.md:12:5:12:48:**
```roc
e = 3402823669209384634633746074317682114553.14: I8
```
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The type annotation says it should have the type:
    _U128_

But here it's being used as:
    _Frac(_size)_

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:3-1:4),UpperIdent(1:5-1:7),
LowerIdent(2:1-2:2),OpAssign(2:3-2:4),Int(2:5-2:8),
LowerIdent(4:1-4:2),OpColon(4:3-4:4),UpperIdent(4:5-4:8),
LowerIdent(5:1-5:2),OpAssign(5:3-5:4),Int(5:5-5:10),
LowerIdent(7:1-7:2),OpColon(7:3-7:4),UpperIdent(7:5-7:8),
LowerIdent(8:1-8:2),OpAssign(8:3-8:4),Int(8:5-8:14),UpperIdent(8:15-8:18),
LowerIdent(9:1-9:2),OpAssign(9:3-9:4),Int(9:5-9:25),
LowerIdent(11:1-11:2),OpColon(11:3-11:4),UpperIdent(11:5-11:9),
LowerIdent(12:1-12:2),OpAssign(12:3-12:4),Float(12:5-12:48),OpColon(12:48-12:49),UpperIdent(12:50-12:52),
LowerIdent(13:1-13:2),OpAssign(13:3-13:4),Int(13:4-13:5),
LowerIdent(15:1-15:2),OpColon(15:3-15:4),UpperIdent(15:5-15:8),
LowerIdent(16:1-16:2),OpAssign(16:3-16:4),Int(16:5-16:11),
LowerIdent(18:1-18:2),OpColon(18:3-18:4),UpperIdent(18:5-18:8),
LowerIdent(19:1-19:2),OpAssign(19:3-19:4),Int(19:5-19:12),
LowerIdent(21:1-21:2),OpColon(21:3-21:4),UpperIdent(21:5-21:8),
LowerIdent(22:1-22:2),OpAssign(22:3-22:4),Int(22:5-22:17),
LowerIdent(24:1-24:2),OpColon(24:3-24:4),UpperIdent(24:5-24:9),
LowerIdent(25:1-25:2),OpAssign(25:3-25:4),Int(25:5-25:29),
EndOfFile(26:1-26:1),
~~~
# PARSE
~~~clojure
(file @1.1-25.29
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
		(s-decl @8.1-8.14
			(p-ident @8.1-8.2 (raw "c"))
			(e-int @8.5-8.14 (raw "429496729")))
		(s-malformed @9.1-9.2 (tag "expected_colon_after_type_annotation"))
		(s-malformed @9.3-9.4 (tag "statement_unexpected_token"))
		(s-malformed @9.5-9.25 (tag "statement_unexpected_token"))
		(s-type-anno @11.1-11.9 (name "e")
			(ty @11.5-11.9 (name "U128")))
		(s-decl @12.1-12.48
			(p-ident @12.1-12.2 (raw "e"))
			(e-frac @12.5-12.48 (raw "3402823669209384634633746074317682114553.14")))
		(s-malformed @12.48-12.49 (tag "statement_unexpected_token"))
		(s-malformed @13.1-13.2 (tag "expected_colon_after_type_annotation"))
		(s-malformed @13.3-13.4 (tag "statement_unexpected_token"))
		(s-malformed @13.4-13.5 (tag "statement_unexpected_token"))
		(s-type-anno @15.1-15.8 (name "g")
			(ty @15.5-15.8 (name "I16")))
		(s-decl @16.1-16.11
			(p-ident @16.1-16.2 (raw "g"))
			(e-int @16.5-16.11 (raw "-32768")))
		(s-type-anno @18.1-18.8 (name "h")
			(ty @18.5-18.8 (name "I32")))
		(s-decl @19.1-19.12
			(p-ident @19.1-19.2 (raw "h"))
			(e-int @19.5-19.12 (raw "-483648")))
		(s-type-anno @21.1-21.8 (name "i")
			(ty @21.5-21.8 (name "I64")))
		(s-decl @22.1-22.17
			(p-ident @22.1-22.2 (raw "i"))
			(e-int @22.5-22.17 (raw "-92233725808")))
		(s-type-anno @24.1-24.9 (name "j")
			(ty @24.5-24.9 (name "I128")))
		(s-decl @25.1-25.29
			(p-ident @25.1-25.2 (raw "j"))
			(e-int @25.5-25.29 (raw "-17011687303715884105728")))))
~~~
# FORMATTED
~~~roc
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
		(e-int @8.5-8.14 (value "429496729"))
		(annotation @8.1-8.2
			(declared-type
				(ty @7.5-7.8 (name "U32")))))
	(d-let
		(p-assign @12.1-12.2 (ident "e"))
		(e-frac-f64 @12.5-12.48 (value "3.4028236692093846e39"))
		(annotation @12.1-12.2
			(declared-type
				(ty @11.5-11.9 (name "U128")))))
	(d-let
		(p-assign @16.1-16.2 (ident "g"))
		(e-int @16.5-16.11 (value "-32768"))
		(annotation @16.1-16.2
			(declared-type
				(ty @15.5-15.8 (name "I16")))))
	(d-let
		(p-assign @19.1-19.2 (ident "h"))
		(e-int @19.5-19.12 (value "-483648"))
		(annotation @19.1-19.2
			(declared-type
				(ty @18.5-18.8 (name "I32")))))
	(d-let
		(p-assign @22.1-22.2 (ident "i"))
		(e-int @22.5-22.17 (value "-92233725808"))
		(annotation @22.1-22.2
			(declared-type
				(ty @21.5-21.8 (name "I64")))))
	(d-let
		(p-assign @25.1-25.2 (ident "j"))
		(e-int @25.5-25.29 (value "-17011687303715884105728"))
		(annotation @25.1-25.2
			(declared-type
				(ty @24.5-24.9 (name "I128"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @2.1-2.2 (type "U8"))
		(patt @5.1-5.2 (type "U16"))
		(patt @8.1-8.2 (type "U32"))
		(patt @12.1-12.2 (type "Error"))
		(patt @16.1-16.2 (type "I16"))
		(patt @19.1-19.2 (type "I32"))
		(patt @22.1-22.2 (type "I64"))
		(patt @25.1-25.2 (type "I128")))
	(expressions
		(expr @2.5-2.8 (type "U8"))
		(expr @5.5-5.10 (type "U16"))
		(expr @8.5-8.14 (type "U32"))
		(expr @12.5-12.48 (type "Error"))
		(expr @16.5-16.11 (type "I16"))
		(expr @19.5-19.12 (type "I32"))
		(expr @22.5-22.17 (type "I64"))
		(expr @25.5-25.29 (type "I128"))))
~~~
