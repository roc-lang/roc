# META
~~~ini
description=fuzz crash
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
# TOKENS
~~~text
KwModule OpenSquare CloseSquare LowerIdent OpColon UpperIdent LowerIdent OpAssign Int LowerIdent OpColon UpperIdent LowerIdent OpAssign Int LowerIdent OpColon UpperIdent LowerIdent OpAssign Int UpperIdent LowerIdent OpAssign Int LowerIdent OpColon UpperIdent LowerIdent OpAssign Float OpColon UpperIdent LowerIdent OpAssign Int LowerIdent OpColon UpperIdent LowerIdent OpAssign OpUnaryMinus Int LowerIdent OpColon UpperIdent LowerIdent OpAssign OpUnaryMinus Int LowerIdent OpColon UpperIdent LowerIdent OpAssign OpUnaryMinus Int LowerIdent OpColon UpperIdent LowerIdent OpAssign OpUnaryMinus Int ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "a")
    (uc "U8")
  )
  (binop_equals
    (lc "a")
    (num_literal_i32 255)
  )
  (binop_colon
    (lc "b")
    (uc "U16")
  )
  (binop_equals
    (lc "b")
    (num_literal_i32 65535)
  )
  (binop_colon
    (lc "c")
    (uc "U32")
  )
  (binop_equals
    (lc "c")
    (num_literal_i32 429496729)
  )
  (uc "U64")
  (binop_equals
    (lc "d")
    (num_literal_big big:<idx:0>)
  )
  (binop_colon
    (lc "e")
    (uc "U128")
  )
  (binop_colon
    (binop_equals
      (lc "e")
      (frac_literal_big big:<idx:21>)
    )
    (uc "I8")
  )
  (binop_equals
    (lc "f")
    (num_literal_i32 8)
  )
  (binop_colon
    (lc "g")
    (uc "I16")
  )
  (binop_equals
    (lc "g")
    (unary_neg <unary>)
  )
  (binop_colon
    (lc "h")
    (uc "I32")
  )
  (binop_equals
    (lc "h")
    (unary_neg <unary>)
  )
  (binop_colon
    (lc "i")
    (uc "I64")
  )
  (binop_equals
    (lc "i")
    (unary_neg <unary>)
  )
  (binop_colon
    (lc "j")
    (uc "I128")
  )
  (binop_equals
    (lc "j")
    (unary_neg <unary>)
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
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
**Pattern in Expression Context**
at 3:5 to 3:7

**Pattern in Expression Context**
at 6:5 to 6:8

**Pattern in Expression Context**
at 9:5 to 9:8

**Pattern in Expression Context**
at 10:15 to 10:18

**Pattern in Expression Context**
at 13:5 to 13:9

**Pattern in Expression Context**
at 14:50 to 14:52

**Pattern in Expression Context**
at 17:5 to 17:8

**Pattern in Expression Context**
at 20:5 to 20:8

**Pattern in Expression Context**
at 23:5 to 23:8

**Pattern in Expression Context**
at 26:5 to 26:9

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "a")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "b")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "c")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "e")
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.binop_equals
      (Expr.lookup "e")
      (Expr.frac_literal_big big:<idx:21>)
    )
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "g")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "h")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "i")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "j")
    (Expr.malformed)
  )
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
