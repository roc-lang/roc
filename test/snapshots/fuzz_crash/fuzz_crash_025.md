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
TYPE APPLICATION NEEDS PARENTHESES - fuzz_crash_025.md:9:1:9:2
UNEXPECTED STATEMENT - fuzz_crash_025.md:9:3:9:4
UNEXPECTED STATEMENT - fuzz_crash_025.md:9:5:9:25
UNEXPECTED STATEMENT - fuzz_crash_025.md:12:48:12:49
TYPE APPLICATION NEEDS PARENTHESES - fuzz_crash_025.md:13:1:13:2
UNEXPECTED STATEMENT - fuzz_crash_025.md:13:3:13:4
UNEXPECTED STATEMENT - fuzz_crash_025.md:13:4:13:5
INVALID NUMBER - fuzz_crash_025.md:12:5:12:48
# PROBLEMS

┌────────────────────────────────────┐
│ TYPE APPLICATION NEEDS PARENTHESES ├─ I was parsing a type annotation, ─────┐
└┬───────────────────────────────────┘  and I found a type argument without   │
 │                                      parentheses.                          │
 │                                                                            │
 │  d = 18446744073709551615                                                  │
 │  ‾                                                                         │
 └───────────────────────────────────────────────────── fuzz_crash_025.md:9:1 ┘

    Roc type applications use parentheses around their arguments. Write
    `List(U8)`, not `List U8`.

    For example:
        List(U8)

    I found `d` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  d = 18446744073709551615                                                  │
 │    ‾                                                                       │
 └───────────────────────────────────────────────────── fuzz_crash_025.md:9:3 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `=` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  d = 18446744073709551615                                                  │
 │      ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                  │
 └───────────────────────────────────────────────────── fuzz_crash_025.md:9:5 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `18446744073709551615` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  e = 3402823669209384634633746074317682114553.14: I8                       │
 │                                                 ‾                          │
 └─────────────────────────────────────────────────── fuzz_crash_025.md:12:48 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `:` here.


┌────────────────────────────────────┐
│ TYPE APPLICATION NEEDS PARENTHESES ├─ I was parsing a type annotation, ─────┐
└┬───────────────────────────────────┘  and I found a type argument without   │
 │                                      parentheses.                          │
 │                                                                            │
 │  f =8                                                                      │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────── fuzz_crash_025.md:13:1 ┘

    Roc type applications use parentheses around their arguments. Write
    `List(U8)`, not `List U8`.

    For example:
        List(U8)

    I found `f` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  f =8                                                                      │
 │    ‾                                                                       │
 └──────────────────────────────────────────────────── fuzz_crash_025.md:13:3 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `=` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  f =8                                                                      │
 │     ‾                                                                      │
 └──────────────────────────────────────────────────── fuzz_crash_025.md:13:4 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `8` here.


┌────────────────┐
│ INVALID NUMBER ├─ This number literal does not fit in the inferred type. ───┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  e = 3402823669209384634633746074317682114553.14: I8                       │
 │      ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                           │
 └──────────────────────────────────────────────────── fuzz_crash_025.md:12:5 ┘

    The inferred type is:

        U128

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
			(ty-lookup (name "U8") (builtin))))
	(d-let
		(p-assign (ident "b"))
		(e-num (value "65535"))
		(annotation
			(ty-lookup (name "U16") (builtin))))
	(d-let
		(p-assign (ident "c"))
		(e-num (value "429496729"))
		(annotation
			(ty-lookup (name "U32") (builtin))))
	(d-let
		(p-assign (ident "e"))
		(e-num-from-numeral)
		(annotation
			(ty-lookup (name "U128") (builtin))))
	(d-let
		(p-assign (ident "g"))
		(e-num (value "-32768"))
		(annotation
			(ty-lookup (name "I16") (builtin))))
	(d-let
		(p-assign (ident "h"))
		(e-num (value "-483648"))
		(annotation
			(ty-lookup (name "I32") (builtin))))
	(d-let
		(p-assign (ident "i"))
		(e-num (value "-92233725808"))
		(annotation
			(ty-lookup (name "I64") (builtin))))
	(d-let
		(p-assign (ident "j"))
		(e-num (value "-17011687303715884105728"))
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
		(patt (type "Error"))
		(patt (type "I16"))
		(patt (type "I32"))
		(patt (type "I64"))
		(patt (type "I128")))
	(expressions
		(expr (type "U8"))
		(expr (type "U16"))
		(expr (type "U32"))
		(expr (type "Error"))
		(expr (type "I16"))
		(expr (type "I32"))
		(expr (type "I64"))
		(expr (type "I128"))))
~~~
