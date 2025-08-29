# META
~~~ini
description=Multiline formatting everything
type=file
~~~
# SOURCE
~~~roc
module []

# Import exposing
import I1 exposing [
	I11,
	I12,
]
import I2 exposing [
	I21 as Ias1,
	I22 as Ias2,
]

# Where constraint
A(a) : a
	where
		module(a).a1 : (
			a,
			a,
		) -> Str,
		module(a).a2 : (
			a,
			a,
		) -> Str
B(b) : b
	where
		module(b).b1 : (
			b,
			b,
		) -> Str,
		module(b).b2 : (
			b,
			b,
		) -> Str

C(
	a,
	b,
) : (
	a,
	b,
)
D(
	a,
	b,
) : C(
	a,
	b,
)
E : {
	a : Str,
	b : Str,
}
F : [
	A,
	B,
]

g : e -> e where module(e).A, module(e).B

h = |x, y| {
	h1 = {
		h11: x,
		h12: x,
		h13: {
			h131: x,
			h132: y,
		},
	}
	h2 = h(
		x,
		y,
	)
	h3 = A(
		x,
		y,
	)
	h4 = [
		x,
		y,
	]
	h5 = (
		x,
		y,
	)

	match x {
		Z1(
			(
				a,
				b,
			),
		) => a
		Z2(
			a,
			b,
		) => a
		Z3(
			{
				a,
				b,
			},
		) => a
		Z4(
			[
				a,
				b,
			],
		) => a
	}
}
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare KwImport UpperIdent KwExposing OpenSquare UpperIdent Comma UpperIdent Comma CloseSquare KwImport UpperIdent KwExposing OpenSquare UpperIdent KwAs UpperIdent Comma UpperIdent KwAs UpperIdent Comma CloseSquare UpperIdent OpenRound LowerIdent CloseRound OpColon LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent Comma CloseRound OpArrow UpperIdent Comma KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent Comma CloseRound OpArrow UpperIdent UpperIdent OpenRound LowerIdent CloseRound OpColon LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent Comma CloseRound OpArrow UpperIdent Comma KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent Comma CloseRound OpArrow UpperIdent UpperIdent OpenRound LowerIdent Comma LowerIdent Comma CloseRound OpColon OpenRound LowerIdent Comma LowerIdent Comma CloseRound UpperIdent OpenRound LowerIdent Comma LowerIdent Comma CloseRound OpColon UpperIdent OpenRound LowerIdent Comma LowerIdent Comma CloseRound UpperIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent Comma CloseCurly UpperIdent OpColon OpenSquare UpperIdent Comma UpperIdent Comma CloseSquare LowerIdent OpColon LowerIdent OpArrow LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot UpperIdent Comma KwModule OpenRound LowerIdent CloseRound Dot UpperIdent LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenCurly LowerIdent OpAssign OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent Comma LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent Comma CloseCurly Comma CloseCurly LowerIdent OpAssign LowerIdent OpenRound LowerIdent Comma LowerIdent Comma CloseRound LowerIdent OpAssign UpperIdent OpenRound LowerIdent Comma LowerIdent Comma CloseRound LowerIdent OpAssign OpenSquare LowerIdent Comma LowerIdent Comma CloseSquare LowerIdent OpAssign OpenRound LowerIdent Comma LowerIdent Comma CloseRound KwMatch LowerIdent OpenCurly UpperIdent OpenRound OpenRound LowerIdent Comma LowerIdent Comma CloseRound Comma CloseRound OpFatArrow LowerIdent UpperIdent OpenRound LowerIdent Comma LowerIdent Comma CloseRound OpFatArrow LowerIdent UpperIdent OpenRound OpenCurly LowerIdent Comma LowerIdent Comma CloseCurly Comma CloseRound OpFatArrow LowerIdent UpperIdent OpenRound OpenSquare LowerIdent Comma LowerIdent Comma CloseSquare Comma CloseRound OpFatArrow LowerIdent CloseCurly CloseCurly ~~~
# PARSE
~~~clojure
(block
  (import
    (binop_exposing
      (uc "I1")
      (list_literal
        (uc "I11")
        (uc "I12")
      )
    )
  )
  (import
    (binop_exposing
      (uc "I2")
      (list_literal
        (uc "I21")
      )
    )
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
                      (uc "Str")
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
  (binop_colon
    (apply_uc
      (uc "B")
      (lc "b")
    )
    (binop_where
      (lc "b")
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
                      (uc "Str")
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
    (record_literal
      (binop_colon
        (lc "a")
        (uc "Str")
      )
      (binop_colon
        (lc "b")
        (uc "Str")
      )
    )
  )
  (binop_colon
    (uc "F")
    (list_literal
      (uc "A")
      (uc "B")
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
            (record_literal
              (binop_colon
                (lc "h11")
                (lc "x")
              )
              (binop_colon
                (lc "h12")
                (lc "x")
              )
              (binop_colon
                (lc "h13")
                (record_literal
                  (binop_colon
                    (lc "h131")
                    (lc "x")
                  )
                  (binop_colon
                    (lc "h132")
                    (lc "y")
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
                (malformed malformed:expr_unexpected_token)
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
                (malformed malformed:expr_unexpected_token)
              )
            )
          )
          (binop_equals
            (lc "h4")
            (list_literal
              (lc "x")
              (lc "y")
            )
          )
          (binop_equals
            (lc "h5")
            (tuple_literal
              (lc "x")
              (lc "y")
              (malformed malformed:expr_unexpected_token)
            )
          )
          (match
            (scrutinee               (lc "x")
))
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
module []

import I1 exposing [
	I11,
	I12,
]
import I2 exposing [I21]
as Ias1,
	I22
as Ias2,
]

# Where constraint
A(a) : a where module(a).a1 : a -> a -> )  -> Str, module(a).a2 : a -> a -> )  -> Str
B(b) : b where module(b).b1 : b -> b -> )  -> Str, module(b).b2 : b -> b -> )  -> Str
C(
	(a, b) : (a, b),
)D(
	(a, b) :
		C(a, b, )
),
)
E :
	{
		a : Str,
		b : Str,
	}
F : [
	A,
	B,
]
g : e -> e where module(e) | A, module(e) | B
h = |x, y| {
	h1 = {
		h11 : x,
		h12 : x,
		h13 :
			{
				h131 : x,
				h132 : y,
			},
	}
	h2 = h(
		(x, y),
	)
	h3 = A(
		(x, y),
	)
	h4 = [
		x,
		y,
	]
	h5 = (x, y)
	match x
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 9:6 to 9:9

**Parse Error**
at 9:13 to 10:2

**Parse Error**
at 10:6 to 10:9

**Parse Error**
at 10:13 to 11:1

**Parse Error**
at 11:1 to 14:1

**Parse Error**
at 19:3 to 19:5

**Parse Error**
at 23:3 to 23:5

**Parse Error**
at 24:1 to 24:1

**Parse Error**
at 24:1 to 24:1

**Parse Error**
at 29:3 to 29:5

**Parse Error**
at 33:3 to 33:5

**Parse Error**
at 35:1 to 35:1

**Parse Error**
at 35:1 to 35:1

**Parse Error**
at 38:1 to 38:3

**Parse Error**
at 41:1 to 42:1

**Parse Error**
at 42:1 to 42:1

**Parse Error**
at 35:1 to 42:1

**Parse Error**
at 45:1 to 45:3

**Parse Error**
at 48:1 to 49:1

**Parse Error**
at 45:5 to 49:1

**Parse Error**
at 42:1 to 49:1

**Parse Error**
at 72:2 to 73:2

**Parse Error**
at 69:7 to 73:2

**Parse Error**
at 76:2 to 77:2

**Parse Error**
at 73:7 to 77:2

**Parse Error**
at 84:2 to 86:2

**Parse Error**
at 86:2 to 86:2

**Parse Error**
at 91:4 to 91:5

**Parse Error**
at 92:3 to 92:5

**Parse Error**
at 93:3 to 93:3

**Parse Error**
at 87:3 to 93:3

**Parse Error**
at 96:3 to 96:5

**Parse Error**
at 93:3 to 97:3

**Parse Error**
at 102:3 to 102:5

**Parse Error**
at 97:3 to 103:3

**Parse Error**
at 108:3 to 108:5

**Parse Error**
at 103:3 to 109:2

**Parse Error**
at 86:10 to 110:2

**Parse Error**
at 60:12 to 110:2

**Unsupported Node**
at 4:1 to 7:2

**Unsupported Node**
at 8:1 to 9:5

**Unsupported Node**
at 16:9 to 16:12

**Unsupported Node**
at 20:9 to 20:12

**Unsupported Node**
at 26:9 to 26:12

**Unsupported Node**
at 30:9 to 30:12

**Unsupported Node**
at 58:24 to 58:27

**Unsupported Node**
at 58:37 to 58:40

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
    (Expr.binop_colon
      (Expr.lookup "a")
      (Expr.binop_colon
        (Expr.lambda)
        (Expr.binop_thin_arrow
          (Expr.lookup "a")
          (Expr.binop_thin_arrow
            (Expr.lookup "a")
            (Expr.binop_thin_arrow
              (Expr.malformed)
              (Expr.binop_colon
                (Expr.tuple_literal
                  (Expr.apply_tag)
                  (Expr.lambda)
                )
                (Expr.binop_thin_arrow
                  (Expr.lookup "a")
                  (Expr.binop_thin_arrow
                    (Expr.lookup "a")
                    (Expr.binop_thin_arrow
                      (Expr.malformed)
                      (Expr.apply_tag)
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
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.binop_colon
      (Expr.lookup "b")
      (Expr.binop_colon
        (Expr.lambda)
        (Expr.binop_thin_arrow
          (Expr.lookup "b")
          (Expr.binop_thin_arrow
            (Expr.lookup "b")
            (Expr.binop_thin_arrow
              (Expr.malformed)
              (Expr.binop_colon
                (Expr.tuple_literal
                  (Expr.apply_tag)
                  (Expr.lambda)
                )
                (Expr.binop_thin_arrow
                  (Expr.lookup "b")
                  (Expr.binop_thin_arrow
                    (Expr.lookup "b")
                    (Expr.binop_thin_arrow
                      (Expr.malformed)
                      (Expr.apply_tag)
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
  (Expr.apply_tag)
  (Expr.apply_tag)
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "a")
        (Expr.apply_tag)
      )
      (Expr.binop_colon
        (Expr.lookup "b")
        (Expr.apply_tag)
      )
    )
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.list_literal)
  )
  (Expr.binop_colon
    (Expr.lookup "g")
    (Expr.tuple_literal
      (Expr.binop_colon
        (Expr.binop_thin_arrow
          (Expr.lookup "e")
          (Expr.lookup "e")
        )
        (Expr.lambda)
      )
      (Expr.lambda)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "h")
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
h : _c
~~~
