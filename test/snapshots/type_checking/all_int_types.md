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
# TOKENS
~~~text
KwModule OpenSquare CloseSquare LowerIdent OpColon UpperIdent LowerIdent OpAssign Int LowerIdent OpColon UpperIdent LowerIdent OpAssign Int LowerIdent OpColon UpperIdent LowerIdent OpAssign Int LowerIdent OpColon UpperIdent LowerIdent OpAssign Int LowerIdent OpColon UpperIdent LowerIdent OpAssign Int LowerIdent OpColon UpperIdent LowerIdent OpAssign OpUnaryMinus Int LowerIdent OpColon UpperIdent LowerIdent OpAssign OpUnaryMinus Int LowerIdent OpColon UpperIdent LowerIdent OpAssign OpUnaryMinus Int LowerIdent OpColon UpperIdent LowerIdent OpAssign OpUnaryMinus Int LowerIdent OpColon UpperIdent LowerIdent OpAssign OpUnaryMinus Int ~~~
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
    (num_literal_big big:<idx:0>)
  )
  (binop_colon
    (lc "d")
    (uc "U64")
  )
  (binop_equals
    (lc "d")
    (num_literal_big big:<idx:11>)
  )
  (binop_colon
    (lc "e")
    (uc "U128")
  )
  (binop_equals
    (lc "e")
    (num_literal_big big:<idx:32>)
  )
  (binop_colon
    (lc "f")
    (uc "I8")
  )
  (binop_equals
    (lc "f")
    (unary_neg <unary>)
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


a: U8
a = 255

b: U16
b = 65535

c: U32
c = 4294967295

d: U64
d = 18446744073709551615

e: U128
e = 340282366920938463463374607431768211455

f: I8
f = -128

g: I16
g = -32768

h: I32
h = -2147483648

i: I64
i = -9223372036854775808

j: I128
j = -170141183460469231731687303715884105728
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
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "b")
    (Expr.apply_tag)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "c")
    (Expr.apply_tag)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "d")
    (Expr.apply_tag)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "e")
    (Expr.apply_tag)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "f")
    (Expr.apply_tag)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "g")
    (Expr.apply_tag)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "h")
    (Expr.apply_tag)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "i")
    (Expr.apply_tag)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "j")
    (Expr.apply_tag)
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
