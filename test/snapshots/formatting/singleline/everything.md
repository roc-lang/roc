# META
~~~ini
description=Singleline formatting everything
type=file
~~~
# SOURCE
~~~roc
module []

# Import exposing
import I1 exposing [I11, I12]
import I2 exposing [I21 as Ias1, I22 as Ias2]

# Where constraint
A(a) : a where module(a).a1 : (a, a) -> Str, module(a).a2 : (a, a) -> Str
B(b) : b where module(b).b1 : (b, b) -> Str, module(b).b2 : (b, b) -> Str

C(a, b) : (a, b)
D(a, b) : C(a, b)
E : { a : Str, b : Str }
F : [A, B]

g : e -> e where module(e).A, module(e).B

h = |x, y| {
	h1 = { h11: x, h12: x, h13: { h131: x, h132: y } }
	h2 = h(x, y)
	h3 = A(x, y)
	h4 = [x, y]
	h5 = (x, y)

	match x {
		Z1((a, b)) => a
		Z2(a, b) => a
		Z3({ a, b }) => a
		Z4([a, b]) => a
	}
}
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare KwImport UpperIdent KwExposing OpenSquare UpperIdent Comma UpperIdent CloseSquare KwImport UpperIdent KwExposing OpenSquare UpperIdent KwAs UpperIdent Comma UpperIdent KwAs UpperIdent CloseSquare UpperIdent OpenRound LowerIdent CloseRound OpColon LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent CloseRound OpArrow UpperIdent Comma KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent CloseRound OpArrow UpperIdent UpperIdent OpenRound LowerIdent CloseRound OpColon LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent CloseRound OpArrow UpperIdent Comma KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent CloseRound OpArrow UpperIdent UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon OpenRound LowerIdent Comma LowerIdent CloseRound UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound UpperIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent CloseCurly UpperIdent OpColon OpenSquare UpperIdent Comma UpperIdent CloseSquare LowerIdent OpColon LowerIdent OpArrow LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot UpperIdent Comma KwModule OpenRound LowerIdent CloseRound Dot UpperIdent LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenCurly LowerIdent OpAssign OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent Comma LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent CloseCurly CloseCurly LowerIdent OpAssign LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpenSquare LowerIdent Comma LowerIdent CloseSquare LowerIdent OpAssign OpenRound LowerIdent Comma LowerIdent CloseRound KwMatch LowerIdent OpenCurly UpperIdent OpenRound OpenRound LowerIdent Comma LowerIdent CloseRound CloseRound OpFatArrow LowerIdent UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpFatArrow LowerIdent UpperIdent OpenRound OpenCurly LowerIdent Comma LowerIdent CloseCurly CloseRound OpFatArrow LowerIdent UpperIdent OpenRound OpenSquare LowerIdent Comma LowerIdent CloseSquare CloseRound OpFatArrow LowerIdent CloseCurly CloseCurly ~~~
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
  (binop_colon
    (apply_uc
      (uc "A")
      (lc "a")
    )
    (binop_thin_arrow
      (binop_colon
        (tuple_literal
          (binop_thin_arrow
            (binop_where
              (lc "a")
              (binop_colon
                (binop_pipe
                  (apply_module
                    (lc "a")
                  )
                  (dot_lc "a1")
                )
                (tuple_literal
                  (lc "a")
                  (lc "a")
                )
              )
            )
            (uc "Str")
          )
          (binop_pipe
            (apply_module
              (lc "a")
            )
            (dot_lc "a2")
          )
        )
        (tuple_literal
          (lc "a")
          (lc "a")
        )
      )
      (uc "Str")
    )
  )
  (binop_colon
    (apply_uc
      (uc "B")
      (lc "b")
    )
    (binop_thin_arrow
      (binop_colon
        (tuple_literal
          (binop_thin_arrow
            (binop_where
              (lc "b")
              (binop_colon
                (binop_pipe
                  (apply_module
                    (lc "b")
                  )
                  (dot_lc "b1")
                )
                (tuple_literal
                  (lc "b")
                  (lc "b")
                )
              )
            )
            (uc "Str")
          )
          (binop_pipe
            (apply_module
              (lc "b")
            )
            (dot_lc "b2")
          )
        )
        (tuple_literal
          (lc "b")
          (lc "b")
        )
      )
      (uc "Str")
    )
  )
  (binop_colon
    (apply_uc
      (uc "C")
      (tuple_literal
        (lc "a")
        (lc "b")
      )
    )
    (tuple_literal
      (lc "a")
      (lc "b")
    )
  )
  (binop_colon
    (apply_uc
      (uc "D")
      (tuple_literal
        (lc "a")
        (lc "b")
      )
    )
    (apply_uc
      (uc "C")
      (tuple_literal
        (lc "a")
        (lc "b")
      )
    )
  )
  (binop_colon
    (uc "E")
    (block
      (binop_colon
        (lc "a")
        (binop_colon
          (tuple_literal
            (uc "Str")
            (lc "b")
          )
          (uc "Str")
        )
      )
    )
  )
  (binop_colon
    (uc "F")
    (list_literal
      (tuple_literal
        (uc "A")
        (uc "B")
      )
    )
  )
  (binop_colon
    (lc "g")
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
    )
  )
  (binop_equals
    (lc "h")
    (lambda
      (body
        (block
          (binop_equals
            (lc "h1")
            (block
              (binop_colon
                (lc "h11")
                (binop_colon
                  (tuple_literal
                    (binop_colon
                      (tuple_literal
                        (lc "x")
                        (lc "h12")
                      )
                      (lc "x")
                    )
                    (lc "h13")
                  )
                  (block
                    (binop_colon
                      (lc "h131")
                      (binop_colon
                        (tuple_literal
                          (lc "x")
                          (lc "h132")
                        )
                        (lc "y")
                      )
                    )
                  )
                )
              )
            )
          )
          (binop_equals
            (lc "h2")
            (apply_lc
              (lc "h")
              (tuple_literal
                (lc "x")
                (lc "y")
              )
            )
          )
          (binop_equals
            (lc "h3")
            (apply_uc
              (uc "A")
              (tuple_literal
                (lc "x")
                (lc "y")
              )
            )
          )
          (binop_equals
            (lc "h4")
            (list_literal
              (tuple_literal
                (lc "x")
                (lc "y")
              )
            )
          )
          (binop_equals
            (lc "h5")
            (tuple_literal
              (lc "x")
              (lc "y")
            )
          )
          (match <175 branches>)
        )
      )
      (args
        (tuple_literal
          (lc "x")
          (lc "y")
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
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - everything.md:8:1:8:74
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - everything.md:9:1:9:74
MODULE NOT FOUND - everything.md:4:1:4:30
MODULE NOT FOUND - everything.md:5:1:5:46
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
at 25:2 to 25:10

**Parse Error**
at 26:14 to 26:14

**Parse Error**
at 27:12 to 27:12

**Parse Error**
at 28:16 to 28:16

**Parse Error**
at 29:14 to 29:14

**Parse Error**
at 25:2 to 31:1

**Parse Error**
at 31:1 to 31:1

**Parse Error**
at 18:12 to 31:2

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
at 8:59 to 8:74

**Unsupported Node**
at 9:59 to 9:74

**Unsupported Node**
at 11:16 to 11:17

**Unsupported Node**
at 13:18 to 13:18

**Unsupported Node**
at 14:5 to 15:1

**Unsupported Node**
at 1:1 to 1:1

**Unsupported Node**
at 18:5 to 18:12

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
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "a")
        (Expr.binop_colon
          (Expr.malformed)
          (Expr.apply_tag)
        )
      )
    )
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.lookup "g")
    (Expr.malformed)
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
