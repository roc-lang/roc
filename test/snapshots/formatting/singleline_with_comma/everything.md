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
(module-header)
~~~
# FORMATTED
~~~roc
module []

import I1 exposing [
	I11,
	I12,
]
import I2 exposing [I21]
as Ias1, I22
as Ias2,]

# Where constraint
A(a) : a where module(a).a1 : a -> a -> )  -> Str, module(a).a2 : a -> a -> )  -> (Str, B(b)) : b where module(b).b1 : b -> b -> )  -> Str, module(b).b2 : b -> b -> )  -> (
	Str,
	C(
		(
			a,
			b,
		) : (
			a,
			b,
		),
	),
)
D(
	(
		a,
		b,
	) :
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
g : e -> e where module(e) | A, module(e) | B, h = |
	x,
	y,
| {
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
		(
			x,
			y,
		),
	)
	h3 = A(
		(
			x,
			y,
		),
	)
	h4 = [
		x,
		y,
	]
	h5 = (
		x,
		y,
	)
	match x
(
		a => (b => ()  => a)),
	)
	Z3(
		{
			a : a,
			b : b,
		},
	)
	=> 
	a : a
	Z4(
		[
			a,
			b,
		],
	)
	=> 
	a : a
}
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 5:25 to 5:28

**Parse Error**
at 5:32 to 5:34

**Parse Error**
at 5:38 to 5:41

**Parse Error**
at 5:45 to 5:46

**Parse Error**
at 5:46 to 8:1

**Parse Error**
at 8:37 to 8:39

**Parse Error**
at 8:68 to 8:70

**Parse Error**
at 9:37 to 9:39

**Parse Error**
at 9:68 to 9:70

**Parse Error**
at 11:8 to 11:10

**Parse Error**
at 11:18 to 12:1

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
at 12:8 to 12:10

**Parse Error**
at 12:19 to 13:1

**Parse Error**
at 12:12 to 13:1

**Parse Error**
at 12:1 to 13:1

**Parse Error**
at 20:14 to 21:2

**Parse Error**
at 20:7 to 21:2

**Parse Error**
at 21:14 to 22:2

**Parse Error**
at 21:7 to 22:2

**Parse Error**
at 23:13 to 25:2

**Parse Error**
at 25:2 to 25:2

**Parse Error**
at 26:12 to 26:13

**Parse Error**
at 26:3 to 27:3

**Parse Error**
at 26:3 to 27:5

**Parse Error**
at 27:11 to 27:13

**Parse Error**
at 25:2 to 28:3

**Parse Error**
at 28:17 to 28:20

**Parse Error**
at 29:15 to 29:18

**Parse Error**
at 31:1 to 31:2

**Unsupported Node**
at 4:1 to 4:31

**Unsupported Node**
at 5:1 to 5:24

**Unsupported Node**
at 8:22 to 8:25

**Unsupported Node**
at 8:53 to 8:56

**Unsupported Node**
at 9:22 to 9:25

**Unsupported Node**
at 9:53 to 9:56

**Unsupported Node**
at 16:24 to 16:27

**Unsupported Node**
at 16:37 to 16:40

**Unsupported Node**
at 27:11 to 27:13

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
                      (Expr.binop_colon
                        (Expr.binop_colon
                          (Expr.tuple_literal
                            (Expr.apply_tag)
                            (Expr.apply_tag)
                          )
                          (Expr.lookup "b")
                        )
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
                                        (Expr.tuple_literal
                                          (Expr.apply_tag)
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
    (Expr.binop_equals
      (Expr.tuple_literal
        (Expr.binop_colon
          (Expr.binop_thin_arrow
            (Expr.lookup "e")
            (Expr.lookup "e")
          )
          (Expr.lambda)
        )
        (Expr.lambda)
        (Expr.lookup "h")
      )
      (Expr.lambda)
    )
  )
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
~~~
