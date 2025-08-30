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
(module-header)
~~~
# FORMATTED
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
    (Expr.num_literal_big)
  )
  (Expr.binop_colon
    (Expr.lookup "d")
    (Expr.apply_tag)
  )
  (Expr.binop_equals
    (Expr.lookup "d")
    (Expr.num_literal_big)
  )
  (Expr.binop_colon
    (Expr.lookup "e")
    (Expr.apply_tag)
  )
  (Expr.binop_equals
    (Expr.lookup "e")
    (Expr.num_literal_big)
  )
  (Expr.binop_colon
    (Expr.lookup "f")
    (Expr.apply_tag)
  )
  (Expr.binop_equals
    (Expr.lookup "f")
    (Expr.unary_neg)
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
e : Num(_size)
f : Num(_size)
g : Num(_size)
h : Num(_size)
i : Num(_size)
j : Num(_size)
~~~
