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
module []

a : U8
a = 255
b : U16
b = 65535
c : U32
c = 429496729
U64
d = 18446744073709551615
e : U128
e = 3402823669209384634633746074317682114553.14 : I8
f = 8
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
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "a")
    (Expr.apply_tag)
  )
  (Expr.binop_equals
    (Expr.lookup "a")
    (Expr.num_literal_i32 255)
  )
  (Expr.binop_colon
    (Expr.lookup "b")
    (Expr.apply_tag)
  )
  (Expr.binop_equals
    (Expr.lookup "b")
    (Expr.num_literal_i32 65535)
  )
  (Expr.binop_colon
    (Expr.lookup "c")
    (Expr.apply_tag)
  )
  (Expr.binop_equals
    (Expr.lookup "c")
    (Expr.num_literal_i32 429496729)
  )
  (Expr.apply_tag)
  (Expr.binop_equals
    (Expr.lookup "d")
    (Expr.num_literal_big)
  )
  (Expr.binop_colon
    (Expr.lookup "e")
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.binop_equals
      (Expr.lookup "e")
      (Expr.frac_literal_big big:<idx:21>)
    )
    (Expr.apply_tag)
  )
  (Expr.binop_equals
    (Expr.lookup "f")
    (Expr.num_literal_i32 8)
  )
  (Expr.binop_colon
    (Expr.lookup "g")
    (Expr.apply_tag)
  )
  (Expr.binop_equals
    (Expr.lookup "g")
    (Expr.unary_neg)
  )
  (Expr.binop_colon
    (Expr.lookup "h")
    (Expr.apply_tag)
  )
  (Expr.binop_equals
    (Expr.lookup "h")
    (Expr.unary_neg)
  )
  (Expr.binop_colon
    (Expr.lookup "i")
    (Expr.apply_tag)
  )
  (Expr.binop_equals
    (Expr.lookup "i")
    (Expr.unary_neg)
  )
  (Expr.binop_colon
    (Expr.lookup "j")
    (Expr.apply_tag)
  )
  (Expr.binop_equals
    (Expr.lookup "j")
    (Expr.unary_neg)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_k")
~~~
# TYPES
~~~roc
a : Num(_size)
b : Num(_size)
c : Num(_size)
d : Num(_size)
f : Num(_size)
g : Num(_size)
h : Num(_size)
i : Num(_size)
j : Num(_size)
~~~
