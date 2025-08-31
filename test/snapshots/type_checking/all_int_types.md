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
KwModule OpenSquare CloseSquare BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign Int BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign Int BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign Int BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign Int BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign Int BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign OpUnaryMinus Int BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign OpUnaryMinus Int BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign OpUnaryMinus Int BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign OpUnaryMinus Int BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign OpUnaryMinus Int ~~~
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
  (Stmt.type_anno
    (name "a")
    (type uc)
  )
  (Stmt.assign
    (pattern (Patt.ident "a"))
    (Expr.num_literal_i32 255)
  )
  (Stmt.type_anno
    (name "b")
    (type uc)
  )
  (Stmt.assign
    (pattern (Patt.ident "b"))
    (Expr.num_literal_i32 65535)
  )
  (Stmt.type_anno
    (name "c")
    (type uc)
  )
  (Stmt.assign
    (pattern (Patt.ident "c"))
    (Expr.num_literal_big)
  )
  (Stmt.type_anno
    (name "d")
    (type uc)
  )
  (Stmt.assign
    (pattern (Patt.ident "d"))
    (Expr.num_literal_big)
  )
  (Stmt.type_anno
    (name "e")
    (type uc)
  )
  (Stmt.assign
    (pattern (Patt.ident "e"))
    (Expr.num_literal_big)
  )
  (Stmt.type_anno
    (name "f")
    (type uc)
  )
  (Stmt.assign
    (pattern (Patt.ident "f"))
    (Expr.unary_neg)
  )
  (Stmt.type_anno
    (name "g")
    (type uc)
  )
  (Stmt.assign
    (pattern (Patt.ident "g"))
    (Expr.unary_neg)
  )
  (Stmt.type_anno
    (name "h")
    (type uc)
  )
  (Stmt.assign
    (pattern (Patt.ident "h"))
    (Expr.unary_neg)
  )
  (Stmt.type_anno
    (name "i")
    (type uc)
  )
  (Stmt.assign
    (pattern (Patt.ident "i"))
    (Expr.unary_neg)
  )
  (Stmt.type_anno
    (name "j")
    (type uc)
  )
  (Stmt.assign
    (pattern (Patt.ident "j"))
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
~~~
