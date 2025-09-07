# META
~~~ini
description=Example of a nominal tag union with a payload
type=file
~~~
# SOURCE
~~~roc
module []

Pair(a) := [Pair(a, a)]

pairU64 : Pair(U64)
pairU64 = Pair.Pair(1, 2)

pairStr : Pair(Str)
pairStr = Pair.Pair("hello", "world")

mkPair : a, a -> Pair(a)
mkPair = |x, y| Pair.Pair(x, y)

succeedPairSameType : Pair(U8)
succeedPairSameType = mkPair(1, 2)

failPairDiffTypes : Pair(U8)
failPairDiffTypes = mkPair("1", 2)

failPairDiffTypes2 : Pair(U64)
failPairDiffTypes2 = Pair.Pair(1, "str")

mkPairInvalid : a, b -> Pair(a)
mkPairInvalid = |x, y| Pair.Pair(x, y)

mkPairInferred = |x, y| Pair.Pair(x, y)

failWithImplicit = mkPairInferred("str", 2)
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine UpperIdent OpenRound LowerIdent CloseRound OpColonEqual OpenSquare UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound CloseSquare BlankLine LowerIdent OpColon UpperIdent OpenRound UpperIdent CloseRound LowerIdent OpAssign UpperIdent Dot UpperIdent OpenRound Int Comma Int CloseRound BlankLine LowerIdent OpColon UpperIdent OpenRound UpperIdent CloseRound LowerIdent OpAssign UpperIdent Dot UpperIdent OpenRound String Comma String CloseRound BlankLine LowerIdent OpColon LowerIdent Comma LowerIdent OpArrow UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar UpperIdent Dot UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LowerIdent OpColon UpperIdent OpenRound UpperIdent CloseRound LowerIdent OpAssign LowerIdent OpenRound Int Comma Int CloseRound BlankLine LowerIdent OpColon UpperIdent OpenRound UpperIdent CloseRound LowerIdent OpAssign LowerIdent OpenRound String Comma Int CloseRound BlankLine LowerIdent OpColon UpperIdent OpenRound UpperIdent CloseRound LowerIdent OpAssign UpperIdent Dot UpperIdent OpenRound Int Comma String CloseRound BlankLine LowerIdent OpColon LowerIdent Comma LowerIdent OpArrow UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar UpperIdent Dot UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar UpperIdent Dot UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LowerIdent OpAssign LowerIdent OpenRound String Comma Int CloseRound ~~~
# PARSE
~~~clojure
(module-header)
(block
  (binop_colon_equals
    (apply_uc
      (uc "Pair")
      (lc "a")
    )
    (list_literal
      (apply_uc
        (uc "Pair")
        (tuple_literal
          (lc "a")
          (lc "a")
        )
      )
    )
  )
  (binop_colon
    (lc "pairU64")
    (apply_uc
      (uc "Pair")
      (uc "U64")
    )
  )
  (binop_equals
    (lc "pairU64")
    (apply_anon
      (binop_dot
        (uc "Pair")
        (uc "Pair")
      )
      (tuple_literal
        (num_literal_i32 1)
        (num_literal_i32 2)
      )
    )
  )
  (binop_colon
    (lc "pairStr")
    (apply_uc
      (uc "Pair")
      (uc "Str")
    )
  )
  (binop_equals
    (lc "pairStr")
    (apply_anon
      (binop_dot
        (uc "Pair")
        (uc "Pair")
      )
      (tuple_literal
        (str_literal_big "hello")
        (str_literal_big "world")
      )
    )
  )
  (binop_colon
    (lc "mkPair")
    (binop_arrow_call
      (lc "a")
      (binop_arrow_call
        (lc "a")
        (apply_uc
          (uc "Pair")
          (lc "a")
        )
      )
    )
  )
  (binop_equals
    (lc "mkPair")
    (lambda
      (body
        (apply_anon
          (binop_dot
            (uc "Pair")
            (uc "Pair")
          )
          (tuple_literal
            (lc "x")
            (lc "y")
          )
        )
      )
      (args
        (lc "x")
        (lc "y")
      )
    )
  )
  (binop_colon
    (lc "succeedPairSameType")
    (apply_uc
      (uc "Pair")
      (uc "U8")
    )
  )
  (binop_equals
    (lc "succeedPairSameType")
    (apply_lc
      (lc "mkPair")
      (tuple_literal
        (num_literal_i32 1)
        (num_literal_i32 2)
      )
    )
  )
  (binop_colon
    (lc "failPairDiffTypes")
    (apply_uc
      (uc "Pair")
      (uc "U8")
    )
  )
  (binop_equals
    (lc "failPairDiffTypes")
    (apply_lc
      (lc "mkPair")
      (tuple_literal
        (str_literal_small "1")
        (num_literal_i32 2)
      )
    )
  )
  (binop_colon
    (lc "failPairDiffTypes2")
    (apply_uc
      (uc "Pair")
      (uc "U64")
    )
  )
  (binop_equals
    (lc "failPairDiffTypes2")
    (apply_anon
      (binop_dot
        (uc "Pair")
        (uc "Pair")
      )
      (tuple_literal
        (num_literal_i32 1)
        (str_literal_small "str")
      )
    )
  )
  (binop_colon
    (lc "mkPairInvalid")
    (binop_arrow_call
      (lc "a")
      (binop_arrow_call
        (lc "b")
        (apply_uc
          (uc "Pair")
          (lc "a")
        )
      )
    )
  )
  (binop_equals
    (lc "mkPairInvalid")
    (lambda
      (body
        (apply_anon
          (binop_dot
            (uc "Pair")
            (uc "Pair")
          )
          (tuple_literal
            (lc "x")
            (lc "y")
          )
        )
      )
      (args
        (lc "x")
        (lc "y")
      )
    )
  )
  (binop_equals
    (lc "mkPairInferred")
    (lambda
      (body
        (apply_anon
          (binop_dot
            (uc "Pair")
            (uc "Pair")
          )
          (tuple_literal
            (lc "x")
            (lc "y")
          )
        )
      )
      (args
        (lc "x")
        (lc "y")
      )
    )
  )
  (binop_equals
    (lc "failWithImplicit")
    (apply_lc
      (lc "mkPairInferred")
      (tuple_literal
        (str_literal_small "str")
        (num_literal_i32 2)
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module []

Pair(a) := [Pair((a, a))]
pairU64 : Pair U64
pairU64 = Pair.Pair((1, 2))
pairStr : Pair Str
pairStr = Pair.Pair(("hello", "world"))
mkPair : a -> a -> Pair a
mkPair = |x, y| Pair.Pair((x, y))
succeedPairSameType : Pair U8
succeedPairSameType = mkPair((1, 2))
failPairDiffTypes : Pair U8
failPairDiffTypes = mkPair(("1", 2))
failPairDiffTypes2 : Pair U64
failPairDiffTypes2 = Pair.Pair((1, "str"))
mkPairInvalid : a -> b -> Pair a
mkPairInvalid = |x, y| Pair.Pair((x, y))
mkPairInferred = |x, y| Pair.Pair((x, y))
failWithImplicit = mkPairInferred(("str", 2))
~~~
# EXPECTED
TYPE MISMATCH - annotations.md:18:28:18:28
INVALID NOMINAL TAG - annotations.md:21:22:21:41
INVALID NOMINAL TAG - annotations.md:24:24:24:39
TYPE MISMATCH - annotations.md:28:35:28:35
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**annotations.md:5:1:5:8:**
```roc
pairU64 : Pair(U64)
```
^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**annotations.md:6:1:6:8:**
```roc
pairU64 = Pair.Pair(1, 2)
```
^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**annotations.md:8:1:8:8:**
```roc
pairStr : Pair(Str)
```
^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**annotations.md:9:1:9:8:**
```roc
pairStr = Pair.Pair("hello", "world")
```
^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**annotations.md:11:1:11:7:**
```roc
mkPair : a, a -> Pair(a)
```
^^^^^^


**SHADOWING**
This definition shadows an existing one.

**annotations.md:12:1:12:7:**
```roc
mkPair = |x, y| Pair.Pair(x, y)
```
^^^^^^


**SHADOWING**
This definition shadows an existing one.

**annotations.md:14:1:14:20:**
```roc
succeedPairSameType : Pair(U8)
```
^^^^^^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**annotations.md:15:1:15:20:**
```roc
succeedPairSameType = mkPair(1, 2)
```
^^^^^^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**annotations.md:17:1:17:18:**
```roc
failPairDiffTypes : Pair(U8)
```
^^^^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**annotations.md:18:1:18:18:**
```roc
failPairDiffTypes = mkPair("1", 2)
```
^^^^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**annotations.md:20:1:20:19:**
```roc
failPairDiffTypes2 : Pair(U64)
```
^^^^^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**annotations.md:21:1:21:19:**
```roc
failPairDiffTypes2 = Pair.Pair(1, "str")
```
^^^^^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**annotations.md:23:1:23:14:**
```roc
mkPairInvalid : a, b -> Pair(a)
```
^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**annotations.md:24:18:24:19:**
```roc
mkPairInvalid = |x, y| Pair.Pair(x, y)
```
                 ^


**SHADOWING**
This definition shadows an existing one.

**annotations.md:24:21:24:22:**
```roc
mkPairInvalid = |x, y| Pair.Pair(x, y)
```
                    ^


**SHADOWING**
This definition shadows an existing one.

**annotations.md:24:1:24:14:**
```roc
mkPairInvalid = |x, y| Pair.Pair(x, y)
```
^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**annotations.md:26:19:26:20:**
```roc
mkPairInferred = |x, y| Pair.Pair(x, y)
```
                  ^


**SHADOWING**
This definition shadows an existing one.

**annotations.md:26:22:26:23:**
```roc
mkPairInferred = |x, y| Pair.Pair(x, y)
```
                     ^


**SHADOWING**
This definition shadows an existing one.

**annotations.md:26:1:26:15:**
```roc
mkPairInferred = |x, y| Pair.Pair(x, y)
```
^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**annotations.md:28:1:28:17:**
```roc
failWithImplicit = mkPairInferred("str", 2)
```
^^^^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.opaque_type)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "pairU64"))
    (type type_14)
  )
  (Stmt.assign
    (pattern (Patt.ident "pairU64"))
    (Expr.fn_call)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "pairStr"))
    (type type_28)
  )
  (Stmt.assign
    (pattern (Patt.ident "pairStr"))
    (Expr.fn_call)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "mkPair"))
    (type type_46)
  )
  (Stmt.assign
    (pattern (Patt.ident "mkPair"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "succeedPairSameType"))
    (type type_63)
  )
  (Stmt.assign
    (pattern (Patt.ident "succeedPairSameType"))
    (Expr.fn_call)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "failPairDiffTypes"))
    (type type_75)
  )
  (Stmt.assign
    (pattern (Patt.ident "failPairDiffTypes"))
    (Expr.fn_call)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "failPairDiffTypes2"))
    (type type_87)
  )
  (Stmt.assign
    (pattern (Patt.ident "failPairDiffTypes2"))
    (Expr.fn_call)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "mkPairInvalid"))
    (type type_105)
  )
  (Stmt.assign
    (pattern (Patt.ident "mkPairInvalid"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "mkPairInferred"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "failWithImplicit"))
    (Expr.fn_call)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 175
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 -> #23)
(var #17 _)
(var #18 _)
(var #19 -> #139)
(var #20 Num *)
(var #21 Num *)
(var #22 -> #140)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 -> #37)
(var #31 _)
(var #32 _)
(var #33 -> #142)
(var #34 Str)
(var #35 Str)
(var #36 -> #143)
(var #37 _)
(var #38 _)
(var #39 _)
(var #40 _)
(var #41 _)
(var #42 _)
(var #43 _)
(var #44 _)
(var #45 _)
(var #46 _)
(var #47 _)
(var #48 -> #151)
(var #49 _)
(var #50 _)
(var #51 _)
(var #52 _)
(var #53 -> #147)
(var #54 _)
(var #55 _)
(var #56 -> #148)
(var #57 _)
(var #58 -> #151)
(var #59 _)
(var #60 _)
(var #61 _)
(var #62 _)
(var #63 _)
(var #64 _)
(var #65 -> #70)
(var #66 -> #153)
(var #67 Num *)
(var #68 Num *)
(var #69 -> #152)
(var #70 _)
(var #71 _)
(var #72 _)
(var #73 _)
(var #74 _)
(var #75 _)
(var #76 _)
(var #77 -> #82)
(var #78 -> #155)
(var #79 Str)
(var #80 Num *)
(var #81 -> #154)
(var #82 _)
(var #83 _)
(var #84 _)
(var #85 _)
(var #86 _)
(var #87 _)
(var #88 _)
(var #89 -> #96)
(var #90 _)
(var #91 _)
(var #92 -> #156)
(var #93 Num *)
(var #94 Str)
(var #95 -> #157)
(var #96 _)
(var #97 _)
(var #98 _)
(var #99 _)
(var #100 _)
(var #101 _)
(var #102 _)
(var #103 _)
(var #104 _)
(var #105 _)
(var #106 _)
(var #107 -> #165)
(var #108 _)
(var #109 _)
(var #110 _)
(var #111 _)
(var #112 -> #161)
(var #113 _)
(var #114 _)
(var #115 -> #162)
(var #116 _)
(var #117 -> #165)
(var #118 _)
(var #119 -> #172)
(var #120 _)
(var #121 _)
(var #122 _)
(var #123 _)
(var #124 -> #168)
(var #125 _)
(var #126 _)
(var #127 -> #169)
(var #128 _)
(var #129 -> #172)
(var #130 _)
(var #131 -> #136)
(var #132 -> #174)
(var #133 Str)
(var #134 Num *)
(var #135 -> #173)
(var #136 _)
(var #137 _)
(var #138 _)
(var #139 -> #141)
(var #140 tuple)
(var #141 fn_pure)
(var #142 -> #144)
(var #143 tuple)
(var #144 fn_pure)
(var #145 _)
(var #146 _)
(var #147 -> #149)
(var #148 tuple)
(var #149 fn_pure)
(var #150 fn_pure)
(var #151 fn_pure)
(var #152 tuple)
(var #153 fn_pure)
(var #154 tuple)
(var #155 fn_pure)
(var #156 -> #158)
(var #157 tuple)
(var #158 fn_pure)
(var #159 _)
(var #160 _)
(var #161 -> #163)
(var #162 tuple)
(var #163 fn_pure)
(var #164 fn_pure)
(var #165 fn_pure)
(var #166 _)
(var #167 _)
(var #168 -> #170)
(var #169 tuple)
(var #170 fn_pure)
(var #171 fn_pure)
(var #172 fn_pure)
(var #173 tuple)
(var #174 fn_pure)
~~~
# TYPES
~~~roc
succeedPairSameType : _c
failWithImplicit : _c
pairStr : _c
mkPairInvalid : _arg -> _arg2 -> _ret
mkPairInferred : _arg -> _arg2 -> _ret
y : _c
failPairDiffTypes2 : _c
x : _c
failPairDiffTypes : _c
mkPair : _arg -> _arg2 -> _ret
pairU64 : _c
~~~
