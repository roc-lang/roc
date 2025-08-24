# META
~~~ini
description=Singleline with comma formatting everything
type=file
~~~
# SOURCE
~~~roc
module []

# Import exposing
import I1 exposing [I11, I12,]
import I2 exposing [I21 as Ias1, I22 as Ias2,]

# Where constraint
A(a) : a where module(a).a1 : (a, a,) -> Str, module(a).a2 : (a, a,) -> Str,
B(b) : b where module(b).b1 : (b, b,) -> Str, module(b).b2 : (b, b,) -> Str,

C(a, b,) : (a, b,)
D(a, b,) : C(a, b,)
E : { a : Str, b : Str, }
F : [A, B,]

g : e -> e where module(e).A, module(e).B,

h = |x, y,| {
	h1 = { h11: x, h12: x, h13: { h131: x, h132: y, }, }
	h2 = h(x, y,)
	h3 = A(x, y,)
	h4 = [x, y,]
	h5 = (x, y,)

	match x {
		Z1((a, b,)) => a
		Z2(a, b,) => a
		Z3({ a, b, }) => a
		Z4([a, b,]) => a
	}
}
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare KwImport UpperIdent KwExposing OpenSquare UpperIdent Comma UpperIdent Comma CloseSquare KwImport UpperIdent KwExposing OpenSquare UpperIdent KwAs UpperIdent Comma UpperIdent KwAs UpperIdent Comma CloseSquare UpperIdent OpenRound LowerIdent CloseRound OpColon LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent Comma CloseRound OpArrow UpperIdent Comma KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent Comma CloseRound OpArrow UpperIdent Comma UpperIdent OpenRound LowerIdent CloseRound OpColon LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent Comma CloseRound OpArrow UpperIdent Comma KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent Comma CloseRound OpArrow UpperIdent Comma UpperIdent OpenRound LowerIdent Comma LowerIdent Comma CloseRound OpColon OpenRound LowerIdent Comma LowerIdent Comma CloseRound UpperIdent OpenRound LowerIdent Comma LowerIdent Comma CloseRound OpColon UpperIdent OpenRound LowerIdent Comma LowerIdent Comma CloseRound UpperIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent Comma CloseCurly UpperIdent OpColon OpenSquare UpperIdent Comma UpperIdent Comma CloseSquare LowerIdent OpColon LowerIdent OpArrow LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot UpperIdent Comma KwModule OpenRound LowerIdent CloseRound Dot UpperIdent Comma LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent Comma OpBar OpenCurly LowerIdent OpAssign OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent Comma LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent Comma CloseCurly Comma CloseCurly LowerIdent OpAssign LowerIdent OpenRound LowerIdent Comma LowerIdent Comma CloseRound LowerIdent OpAssign UpperIdent OpenRound LowerIdent Comma LowerIdent Comma CloseRound LowerIdent OpAssign OpenSquare LowerIdent Comma LowerIdent Comma CloseSquare LowerIdent OpAssign OpenRound LowerIdent Comma LowerIdent Comma CloseRound KwMatch LowerIdent OpenCurly UpperIdent OpenRound OpenRound LowerIdent Comma LowerIdent Comma CloseRound CloseRound OpFatArrow LowerIdent UpperIdent OpenRound LowerIdent Comma LowerIdent Comma CloseRound OpFatArrow LowerIdent UpperIdent OpenRound OpenCurly LowerIdent Comma LowerIdent Comma CloseCurly CloseRound OpFatArrow LowerIdent UpperIdent OpenRound OpenSquare LowerIdent Comma LowerIdent Comma CloseSquare CloseRound OpFatArrow LowerIdent CloseCurly CloseCurly ~~~
# PARSE
~~~clojure
(block
  (import
    (uc "I1")
    (uc "I11")
    (uc "I12")
  )
  (import
    (uc "I2")
    (uc "I21")
  )
  (malformed malformed:expr_unexpected_token)
  (uc "Ias1")
  (malformed malformed:expr_unexpected_token)
  (uc "I22")
  (malformed malformed:expr_unexpected_token)
  (uc "Ias2")
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (binop_colon
    (apply_uc
      (uc "A")
      (lc "a")
    )
    (binop_where
      (lc "a")
      (binop_colon
        (binop_pipe
          (apply_module
            (lc "a")
          )
          (dot_lc "a1")
        )
        (binop_thin_arrow
          (lc "a")
          (binop_thin_arrow
            (lc "a")
            (binop_thin_arrow
              (malformed malformed:expr_unexpected_token)
              (binop_colon
                (tuple_literal
                  (uc "Str")
                  (binop_pipe
                    (apply_module
                      (lc "a")
                    )
                    (dot_lc "a2")
                  )
                )
                (binop_thin_arrow
                  (lc "a")
                  (binop_thin_arrow
                    (lc "a")
                    (binop_thin_arrow
                      (malformed malformed:expr_unexpected_token)
                      (binop_where
                        (binop_colon
                          (tuple_literal
                            (uc "Str")
                            (apply_uc
                              (uc "B")
                              (lc "b")
                            )
                          )
                          (lc "b")
                        )
                        (binop_colon
                          (binop_pipe
                            (apply_module
                              (lc "b")
                            )
                            (dot_lc "b1")
                          )
                          (binop_thin_arrow
                            (lc "b")
                            (binop_thin_arrow
                              (lc "b")
                              (binop_thin_arrow
                                (malformed malformed:expr_unexpected_token)
                                (binop_colon
                                  (tuple_literal
                                    (uc "Str")
                                    (binop_pipe
                                      (apply_module
                                        (lc "b")
                                      )
                                      (dot_lc "b2")
                                    )
                                  )
                                  (binop_thin_arrow
                                    (lc "b")
                                    (binop_thin_arrow
                                      (lc "b")
                                      (binop_thin_arrow
                                        (malformed malformed:expr_unexpected_token)
                                        (tuple_literal
                                          (uc "Str")
                                          (apply_uc
                                            (uc "C")
                                            (binop_colon
                                              (tuple_literal
                                                (lc "a")
                                                (lc "b")
                                                (malformed malformed:expr_unexpected_token)
                                              )
                                              (tuple_literal
                                                (lc "a")
                                                (lc "b")
                                                (malformed malformed:expr_unexpected_token)
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  (apply_uc
    (uc "D")
    (binop_colon
      (tuple_literal
        (lc "a")
        (lc "b")
        (malformed malformed:expr_unexpected_token)
      )
      (apply_uc
        (uc "C")
        (tuple_literal
          (lc "a")
          (lc "b")
          (malformed malformed:expr_unexpected_token)
        )
      )
    )
  )
  (binop_colon
    (uc "E")
    (block
      (binop_colon
        (lc "a")
        (tuple_literal
          (binop_colon
            (tuple_literal
              (uc "Str")
              (lc "b")
            )
            (uc "Str")
          )
          (malformed malformed:expr_unexpected_token)
        )
      )
      (binop_colon
        (uc "F")
        (list_literal
          (tuple_literal
            (uc "A")
            (uc "B")
            (malformed malformed:expr_unexpected_token)
          )
        )
      )
      (binop_colon
        (lc "g")
        (binop_equals
          (tuple_literal
            (binop_where
              (binop_thin_arrow
                (lc "e")
                (lc "e")
              )
              (binop_pipe
                (apply_module
                  (lc "e")
                )
                (uc "A")
              )
            )
            (binop_pipe
              (apply_module
                (lc "e")
              )
              (uc "B")
            )
            (lc "h")
          )
          (malformed malformed:expr_unexpected_token)
        )
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - everything.md:8:1:8:77
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - everything.md:9:1:9:77
MODULE NOT FOUND - everything.md:4:1:4:31
MODULE NOT FOUND - everything.md:5:1:5:47
UNUSED VARIABLE - everything.md:26:10:26:11
UNUSED VARIABLE - everything.md:27:9:27:10
UNUSED VARIABLE - everything.md:28:11:28:12
UNUSED VARIABLE - everything.md:29:10:29:11
UNUSED VARIABLE - everything.md:19:2:19:4
UNUSED VARIABLE - everything.md:20:2:20:4
UNUSED VARIABLE - everything.md:21:2:21:4
UNUSED VARIABLE - everything.md:22:2:22:4
UNUSED VARIABLE - everything.md:23:2:23:4
# PROBLEMS
**Parse Error**
at 5:25 to 5:25

**Parse Error**
at 5:32 to 5:32

**Parse Error**
at 5:38 to 5:38

**Parse Error**
at 5:45 to 5:45

**Parse Error**
at 5:46 to 5:46

**Parse Error**
at 8:37 to 8:37

**Parse Error**
at 8:68 to 8:68

**Parse Error**
at 9:37 to 9:37

**Parse Error**
at 9:68 to 9:68

**Parse Error**
at 11:8 to 11:8

**Parse Error**
at 11:18 to 11:18

**Parse Error**
at 12:1 to 12:1

**Parse Error**
at 11:1 to 12:1

**Parse Error**
at 12:1 to 12:1

**Parse Error**
at 12:1 to 12:1

**Parse Error**
at 12:1 to 12:1

**Parse Error**
at 12:1 to 12:1

**Parse Error**
at 12:8 to 12:8

**Parse Error**
at 12:19 to 12:19

**Parse Error**
at 12:12 to 13:1

**Parse Error**
at 12:1 to 13:1

**Parse Error**
at 13:25 to 13:25

**Parse Error**
at 14:11 to 14:11

**Parse Error**
at 14:5 to 16:1

**Parse Error**
at 19:50 to 19:50

**Parse Error**
at 19:53 to 19:53

**Parse Error**
at 20:14 to 20:14

**Parse Error**
at 20:7 to 21:2

**Parse Error**
at 21:14 to 21:14

**Parse Error**
at 21:7 to 22:2

**Parse Error**
at 22:13 to 22:13

**Parse Error**
at 22:7 to 23:2

**Parse Error**
at 23:13 to 23:13

**Parse Error**
at 25:2 to 25:2

**Parse Error**
at 25:2 to 25:10

**Parse Error**
at 26:12 to 26:12

**Parse Error**
at 26:3 to 27:3

**Parse Error**
at 27:11 to 27:11

**Parse Error**
at 27:3 to 28:3

**Parse Error**
at 28:14 to 28:14

**Parse Error**
at 28:6 to 28:15

**Parse Error**
at 28:17 to 28:17

**Parse Error**
at 29:12 to 29:12

**Parse Error**
at 29:6 to 29:13

**Parse Error**
at 29:15 to 29:15

**Parse Error**
at 25:2 to 31:1

**Parse Error**
at 31:1 to 31:1

**Parse Error**
at 19:30 to 31:2

**Parse Error**
at 19:7 to 31:2

**Parse Error**
at 18:13 to 31:2

**Parse Error**
at 31:2 to 31:2

**Parse Error**
at 31:2 to 31:2

**Parse Error**
at 13:5 to 31:2

**Unsupported Node**
at 4:1 to 4:29

**Unsupported Node**
at 5:1 to 5:24

**Unsupported Node**
at 5:25 to 5:25

**Unsupported Node**
at 5:32 to 5:32

**Unsupported Node**
at 5:38 to 5:38

**Unsupported Node**
at 5:45 to 5:45

**Unsupported Node**
at 5:46 to 5:46

**Unsupported Node**
at 8:8 to 12:2

**Unsupported Node**
at 1:1 to 1:1

**Unsupported Node**
at 12:19 to 12:19

**Unsupported Node**
at 1:1 to 1:1

**Unsupported Node**
at 14:5 to 15:1

**Unsupported Node**
at 18:3 to 18:3

**Unsupported Node**
at 31:2 to 31:2

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.apply_tag)
  (Expr.malformed)
  (Expr.apply_tag)
  (Expr.malformed)
  (Expr.apply_tag)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.malformed)
  )
  (Expr.apply_tag)
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.block
      (Expr.binop_colon
        (Expr.lookup "a")
        (Expr.malformed)
      )
      (Expr.binop_colon
        (Expr.apply_tag)
        (Expr.malformed)
      )
      (Expr.binop_colon
        (Expr.lookup "g")
        (Expr.binop_equals
          (Expr.malformed)
          (Expr.malformed)
        )
      )
    )
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "[]_others")
~~~
# TYPES
~~~roc
~~~
