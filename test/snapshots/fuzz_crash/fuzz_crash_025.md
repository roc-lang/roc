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
    (num_literal_big big:<idx:101>)
  )
  (binop_colon
    (lc "e")
    (uc "U128")
  )
  (binop_colon
    (binop_equals
      (lc "e")
      (frac_literal_big big:<idx:122>)
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
    (unary_neg <unary_op>)
  )
  (binop_colon
    (lc "h")
    (uc "I32")
  )
  (binop_equals
    (lc "h")
    (unary_neg <unary_op>)
  )
  (binop_colon
    (lc "i")
    (uc "I64")
  )
  (binop_equals
    (lc "i")
    (unary_neg <unary_op>)
  )
  (binop_colon
    (lc "j")
    (uc "I128")
  )
  (binop_equals
    (lc "j")
    (unary_neg <unary_op>)
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
PARSE ERROR - fuzz_crash_025.md:11:1:11:2
PARSE ERROR - fuzz_crash_025.md:11:3:11:4
PARSE ERROR - fuzz_crash_025.md:11:5:11:25
PARSE ERROR - fuzz_crash_025.md:14:48:14:49
PARSE ERROR - fuzz_crash_025.md:15:1:15:2
PARSE ERROR - fuzz_crash_025.md:15:3:15:4
PARSE ERROR - fuzz_crash_025.md:15:4:15:5
TYPE MISMATCH - fuzz_crash_025.md:14:5:14:48
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_025.md:14:1:14:48:**
```roc
e = 3402823669209384634633746074317682114553.14: I8
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "a"))
    (type type_2)
  )
  (Stmt.assign
    (pattern (Patt.ident "a"))
    (Expr.num_literal_i32 255)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "b"))
    (type type_8)
  )
  (Stmt.assign
    (pattern (Patt.ident "b"))
    (Expr.num_literal_i32 65535)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "c"))
    (type type_14)
  )
  (Stmt.assign
    (pattern (Patt.ident "c"))
    (Expr.num_literal_i32 429496729)
  )
  (Expr.tag_no_args)
  (Stmt.assign
    (pattern (Patt.ident "d"))
    (Expr.num_literal_big)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "e"))
    (type type_24)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.malformed))
    (type type_29)
  )
  (Stmt.assign
    (pattern (Patt.ident "f"))
    (Expr.num_literal_i32 8)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "g"))
    (type type_35)
  )
  (Stmt.assign
    (pattern (Patt.ident "g"))
    (Expr.unary_neg)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "h"))
    (type type_42)
  )
  (Stmt.assign
    (pattern (Patt.ident "h"))
    (Expr.unary_neg)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "i"))
    (type type_49)
  )
  (Stmt.assign
    (pattern (Patt.ident "i"))
    (Expr.unary_neg)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "j"))
    (type type_56)
  )
  (Stmt.assign
    (pattern (Patt.ident "j"))
    (Expr.unary_neg)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 63
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 -> #5)
(var #5 Num *)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 -> #11)
(var #11 Num *)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 -> #17)
(var #17 Num *)
(var #18 _)
(var #19 _)
(var #20 -> #21)
(var #21 Num *)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 -> #32)
(var #32 Num *)
(var #33 _)
(var #34 _)
(var #35 _)
(var #36 _)
(var #37 -> #38)
(var #38 Num *)
(var #39 -> #38)
(var #40 _)
(var #41 _)
(var #42 _)
(var #43 _)
(var #44 -> #45)
(var #45 Num *)
(var #46 -> #45)
(var #47 _)
(var #48 _)
(var #49 _)
(var #50 _)
(var #51 -> #52)
(var #52 Num *)
(var #53 -> #52)
(var #54 _)
(var #55 _)
(var #56 _)
(var #57 _)
(var #58 -> #59)
(var #59 Num *)
(var #60 -> #59)
(var #61 _)
(var #62 _)
~~~
# TYPES
~~~roc
d : Num(_size)
e : _k
g : Num(_size)
f : Num(_size)
j : Num(_size)
b : Num(_size)
a : Num(_size)
h : Num(_size)
i : Num(_size)
c : Num(_size)
~~~
