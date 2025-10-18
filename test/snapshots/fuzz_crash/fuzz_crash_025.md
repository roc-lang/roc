# META
~~~ini
description=fuzz crash
type=snippet
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


**TYPE MISMATCH**
This expression is used in an unexpected way:
**fuzz_crash_025.md:12:5:12:48:**
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
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,UpperIdent,
LowerIdent,OpAssign,Int,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Float,OpColon,UpperIdent,
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
			(e-int (raw "429496729")))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-anno (name "e")
			(ty (name "U128")))
		(s-decl
			(p-ident (raw "e"))
			(e-frac (raw "3402823669209384634633746074317682114553.14")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-anno (name "g")
			(ty (name "I16")))
		(s-decl
			(p-ident (raw "g"))
			(e-int (raw "-32768")))
		(s-type-anno (name "h")
			(ty (name "I32")))
		(s-decl
			(p-ident (raw "h"))
			(e-int (raw "-483648")))
		(s-type-anno (name "i")
			(ty (name "I64")))
		(s-decl
			(p-ident (raw "i"))
			(e-int (raw "-92233725808")))
		(s-type-anno (name "j")
			(ty (name "I128")))
		(s-decl
			(p-ident (raw "j"))
			(e-int (raw "-17011687303715884105728")))))
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
		(p-assign (ident "a"))
		(e-num (value "255"))
		(annotation
			(declared-type
				(ty-lookup (name "U8") (builtin)))))
	(d-let
		(p-assign (ident "b"))
		(e-num (value "65535"))
		(annotation
			(declared-type
				(ty-lookup (name "U16") (builtin)))))
	(d-let
		(p-assign (ident "c"))
		(e-num (value "429496729"))
		(annotation
			(declared-type
				(ty-lookup (name "U32") (builtin)))))
	(d-let
		(p-assign (ident "e"))
		(e-frac-f64 (value "3.4028236692093846e39"))
		(annotation
			(declared-type
				(ty-lookup (name "U128") (builtin)))))
	(d-let
		(p-assign (ident "g"))
		(e-num (value "-32768"))
		(annotation
			(declared-type
				(ty-lookup (name "I16") (builtin)))))
	(d-let
		(p-assign (ident "h"))
		(e-num (value "-483648"))
		(annotation
			(declared-type
				(ty-lookup (name "I32") (builtin)))))
	(d-let
		(p-assign (ident "i"))
		(e-num (value "-92233725808"))
		(annotation
			(declared-type
				(ty-lookup (name "I64") (builtin)))))
	(d-let
		(p-assign (ident "j"))
		(e-num (value "-17011687303715884105728"))
		(annotation
			(declared-type
				(ty-lookup (name "I128") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(Int(Unsigned8))"))
		(patt (type "Num(Int(Unsigned16))"))
		(patt (type "Num(Int(Unsigned32))"))
		(patt (type "Error"))
		(patt (type "Num(Int(Signed16))"))
		(patt (type "Num(Int(Signed32))"))
		(patt (type "Num(Int(Signed64))"))
		(patt (type "Num(Int(Signed128))")))
	(expressions
		(expr (type "Num(Int(Unsigned8))"))
		(expr (type "Num(Int(Unsigned16))"))
		(expr (type "Num(Int(Unsigned32))"))
		(expr (type "Error"))
		(expr (type "Num(Int(Signed16))"))
		(expr (type "Num(Int(Signed32))"))
		(expr (type "Num(Int(Signed64))"))
		(expr (type "Num(Int(Signed128))"))))
~~~
