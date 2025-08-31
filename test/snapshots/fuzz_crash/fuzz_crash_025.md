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
KwModule OpenSquare CloseSquare BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign Int BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign Int BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign Int UpperIdent LowerIdent OpAssign Int BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign Float OpColon UpperIdent LowerIdent OpAssign Int BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign OpUnaryMinus Int BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign OpUnaryMinus Int BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign OpUnaryMinus Int BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign OpUnaryMinus Int ~~~
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
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_025.md:10:15:10:18:**
```roc
c = 429496729 U64
```
              ^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_k")
~~~
# TYPES
~~~roc
~~~
