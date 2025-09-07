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
    (num_literal_big num:<idx:143>)
  )
  (binop_colon
    (lc "d")
    (uc "U64")
  )
  (binop_equals
    (lc "d")
    (num_literal_big num:<idx:154>)
  )
  (binop_colon
    (lc "e")
    (uc "U128")
  )
  (binop_equals
    (lc "e")
    (num_literal_big num:<idx:175>)
  )
  (binop_colon
    (lc "f")
    (uc "I8")
  )
  (binop_equals
    (lc "f")
    (unary_neg <unary_op>)
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
**SHADOWING**
This definition shadows an existing one.

**all_int_types.md:3:1:3:2:**
```roc
a : U8
```
^


**SHADOWING**
This definition shadows an existing one.

**all_int_types.md:4:1:4:2:**
```roc
a = 255
```
^


**SHADOWING**
This definition shadows an existing one.

**all_int_types.md:6:1:6:2:**
```roc
b : U16
```
^


**SHADOWING**
This definition shadows an existing one.

**all_int_types.md:7:1:7:2:**
```roc
b = 65535
```
^


**SHADOWING**
This definition shadows an existing one.

**all_int_types.md:9:1:9:2:**
```roc
c : U32
```
^


**SHADOWING**
This definition shadows an existing one.

**all_int_types.md:10:1:10:2:**
```roc
c = 4294967295
```
^


**SHADOWING**
This definition shadows an existing one.

**all_int_types.md:12:1:12:2:**
```roc
d : U64
```
^


**SHADOWING**
This definition shadows an existing one.

**all_int_types.md:13:1:13:2:**
```roc
d = 18446744073709551615
```
^


**SHADOWING**
This definition shadows an existing one.

**all_int_types.md:15:1:15:2:**
```roc
e : U128
```
^


**SHADOWING**
This definition shadows an existing one.

**all_int_types.md:16:1:16:2:**
```roc
e = 340282366920938463463374607431768211455
```
^


**SHADOWING**
This definition shadows an existing one.

**all_int_types.md:18:1:18:2:**
```roc
f : I8
```
^


**SHADOWING**
This definition shadows an existing one.

**all_int_types.md:19:1:19:2:**
```roc
f = -128
```
^


**SHADOWING**
This definition shadows an existing one.

**all_int_types.md:21:1:21:2:**
```roc
g : I16
```
^


**SHADOWING**
This definition shadows an existing one.

**all_int_types.md:22:1:22:2:**
```roc
g = -32768
```
^


**SHADOWING**
This definition shadows an existing one.

**all_int_types.md:24:1:24:2:**
```roc
h : I32
```
^


**SHADOWING**
This definition shadows an existing one.

**all_int_types.md:25:1:25:2:**
```roc
h = -2147483648
```
^


**SHADOWING**
This definition shadows an existing one.

**all_int_types.md:27:1:27:2:**
```roc
i : I64
```
^


**SHADOWING**
This definition shadows an existing one.

**all_int_types.md:28:1:28:2:**
```roc
i = -9223372036854775808
```
^


**SHADOWING**
This definition shadows an existing one.

**all_int_types.md:30:1:30:2:**
```roc
j : I128
```
^


**SHADOWING**
This definition shadows an existing one.

**all_int_types.md:31:1:31:2:**
```roc
j = -170141183460469231731687303715884105728
```
^


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
    (Expr.num_literal_big)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "d"))
    (type type_20)
  )
  (Stmt.assign
    (pattern (Patt.ident "d"))
    (Expr.num_literal_big)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "e"))
    (type type_26)
  )
  (Stmt.assign
    (pattern (Patt.ident "e"))
    (Expr.num_literal_big)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "f"))
    (type type_32)
  )
  (Stmt.assign
    (pattern (Patt.ident "f"))
    (Expr.unary_neg)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "g"))
    (type type_39)
  )
  (Stmt.assign
    (pattern (Patt.ident "g"))
    (Expr.unary_neg)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "h"))
    (type type_46)
  )
  (Stmt.assign
    (pattern (Patt.ident "h"))
    (Expr.unary_neg)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "i"))
    (type type_53)
  )
  (Stmt.assign
    (pattern (Patt.ident "i"))
    (Expr.unary_neg)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "j"))
    (type type_60)
  )
  (Stmt.assign
    (pattern (Patt.ident "j"))
    (Expr.unary_neg)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 67
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
(var #20 _)
(var #21 _)
(var #22 -> #23)
(var #23 Num *)
(var #24 _)
(var #25 _)
(var #26 _)
(var #27 _)
(var #28 -> #29)
(var #29 Num *)
(var #30 _)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 -> #35)
(var #35 Num *)
(var #36 -> #35)
(var #37 _)
(var #38 _)
(var #39 _)
(var #40 _)
(var #41 -> #42)
(var #42 Num *)
(var #43 -> #42)
(var #44 _)
(var #45 _)
(var #46 _)
(var #47 _)
(var #48 -> #49)
(var #49 Num *)
(var #50 -> #49)
(var #51 _)
(var #52 _)
(var #53 _)
(var #54 _)
(var #55 -> #56)
(var #56 Num *)
(var #57 -> #56)
(var #58 _)
(var #59 _)
(var #60 _)
(var #61 _)
(var #62 -> #63)
(var #63 Num *)
(var #64 -> #63)
(var #65 _)
(var #66 _)
~~~
# TYPES
~~~roc
e : Num(_size)
g : Num(_size)
j : Num(_size)
b : Num(_size)
a : Num(_size)
d : Num(_size)
f : Num(_size)
h : Num(_size)
i : Num(_size)
c : Num(_size)
~~~
