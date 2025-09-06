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
NIL
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
; Total type variables: 166
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
(var #19 -> #140)
(var #20 Num *)
(var #21 Num *)
(var #22 -> #139)
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
(var #36 -> #141)
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
(var #48 -> #147)
(var #49 _)
(var #50 _)
(var #51 _)
(var #52 _)
(var #53 -> #146)
(var #54 _)
(var #55 _)
(var #56 -> #145)
(var #57 _)
(var #58 -> #147)
(var #59 _)
(var #60 _)
(var #61 _)
(var #62 _)
(var #63 _)
(var #64 _)
(var #65 -> #70)
(var #66 -> #149)
(var #67 Num *)
(var #68 Num *)
(var #69 -> #148)
(var #70 _)
(var #71 _)
(var #72 _)
(var #73 _)
(var #74 _)
(var #75 _)
(var #76 _)
(var #77 -> #82)
(var #78 -> #151)
(var #79 Str)
(var #80 Num *)
(var #81 -> #150)
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
(var #92 -> #153)
(var #93 Num *)
(var #94 Str)
(var #95 -> #152)
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
(var #107 -> #158)
(var #108 _)
(var #109 _)
(var #110 _)
(var #111 _)
(var #112 -> #157)
(var #113 _)
(var #114 _)
(var #115 -> #156)
(var #116 _)
(var #117 -> #158)
(var #118 _)
(var #119 -> #163)
(var #120 _)
(var #121 _)
(var #122 _)
(var #123 _)
(var #124 -> #162)
(var #125 _)
(var #126 _)
(var #127 -> #161)
(var #128 _)
(var #129 -> #163)
(var #130 _)
(var #131 -> #136)
(var #132 -> #165)
(var #133 Str)
(var #134 Num *)
(var #135 -> #164)
(var #136 _)
(var #137 _)
(var #138 _)
(var #139 tuple)
(var #140 fn_pure)
(var #141 tuple)
(var #142 fn_pure)
(var #143 _)
(var #144 _)
(var #145 tuple)
(var #146 fn_pure)
(var #147 fn_pure)
(var #148 tuple)
(var #149 fn_pure)
(var #150 tuple)
(var #151 fn_pure)
(var #152 tuple)
(var #153 fn_pure)
(var #154 _)
(var #155 _)
(var #156 tuple)
(var #157 fn_pure)
(var #158 fn_pure)
(var #159 _)
(var #160 _)
(var #161 tuple)
(var #162 fn_pure)
(var #163 fn_pure)
(var #164 tuple)
(var #165 fn_pure)
~~~
# TYPES
~~~roc
succeedPairSameType : _c
failWithImplicit : _c
pairStr : _c
mkPairInvalid : _arg, _arg2 -> _ret
mkPairInferred : _arg, _arg2 -> _ret
y : _c
failPairDiffTypes2 : _c
x : _c
failPairDiffTypes : _c
mkPair : _arg, _arg2 -> _ret
pairU64 : _c
~~~
