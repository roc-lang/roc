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
KwModule OpenSquare CloseSquare BlankLine LineComment KwImport UpperIdent KwExposing OpenSquare UpperIdent Comma UpperIdent Comma CloseSquare KwImport UpperIdent KwExposing OpenSquare UpperIdent KwAs UpperIdent Comma UpperIdent KwAs UpperIdent Comma CloseSquare BlankLine LineComment UpperIdent OpenRound LowerIdent CloseRound OpColon LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent Comma CloseRound OpArrow UpperIdent Comma KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent Comma CloseRound OpArrow UpperIdent UpperIdent OpenRound LowerIdent CloseRound OpColon LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent Comma CloseRound OpArrow UpperIdent Comma KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent Comma CloseRound OpArrow UpperIdent BlankLine UpperIdent OpenRound LowerIdent Comma LowerIdent Comma CloseRound OpColon OpenRound LowerIdent Comma LowerIdent Comma CloseRound UpperIdent OpenRound LowerIdent Comma LowerIdent Comma CloseRound OpColon UpperIdent OpenRound LowerIdent Comma LowerIdent Comma CloseRound UpperIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent Comma CloseCurly UpperIdent OpColon OpenSquare UpperIdent Comma UpperIdent Comma CloseSquare BlankLine LowerIdent OpColon LowerIdent OpArrow LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot UpperIdent Comma KwModule OpenRound LowerIdent CloseRound Dot UpperIdent BlankLine LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenCurly LowerIdent OpAssign OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent Comma LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent Comma CloseCurly Comma CloseCurly LowerIdent OpAssign LowerIdent OpenRound LowerIdent Comma LowerIdent Comma CloseRound LowerIdent OpAssign UpperIdent OpenRound LowerIdent Comma LowerIdent Comma CloseRound LowerIdent OpAssign OpenSquare LowerIdent Comma LowerIdent Comma CloseSquare LowerIdent OpAssign OpenRound LowerIdent Comma LowerIdent Comma CloseRound BlankLine KwMatch LowerIdent OpenCurly UpperIdent OpenRound OpenRound LowerIdent Comma LowerIdent Comma CloseRound Comma CloseRound OpFatArrow LowerIdent UpperIdent OpenRound LowerIdent Comma LowerIdent Comma CloseRound OpFatArrow LowerIdent UpperIdent OpenRound OpenCurly LowerIdent Comma LowerIdent Comma CloseCurly Comma CloseRound OpFatArrow LowerIdent UpperIdent OpenRound OpenSquare LowerIdent Comma LowerIdent Comma CloseSquare Comma CloseRound OpFatArrow LowerIdent CloseCurly CloseCurly ~~~
# PARSE
~~~clojure
(module-header)
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
  (malformed)
  (uc "Ias1")
  (malformed)
  (uc "I22")
  (malformed)
  (uc "Ias2")
  (malformed)
  (malformed)
  (binop_colon
    (apply_uc
      (uc "A")
      (lc "a")
    )
    (binop_arrow_call
      (binop_colon
        (tuple_literal
          (binop_arrow_call
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
    (binop_arrow_call
      (binop_colon
        (tuple_literal
          (binop_arrow_call
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
        (binop_arrow_call
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
              (lc "x")
              (lc "y")
            )
          )
          (binop_equals
            (lc "h5")
            (tuple_literal
              (lc "x")
              (lc "y")
            )
          )
          (match
            (scrutinee               (lc "x")
))
          (apply_uc
            (uc "Z2")
            (tuple_literal
              (lc "a")
              (lc "b")
            )
          )
          (malformed)
          (binop_colon
            (lc "a")
            (lc "a")
          )
          (apply_uc
            (uc "Z3")
            (record_literal
              (binop_colon
                (lc "a")
                (lc "a")
              )
              (binop_colon
                (lc "b")
                (lc "b")
              )
            )
          )
          (malformed)
          (binop_colon
            (lc "a")
            (lc "a")
          )
          (apply_uc
            (uc "Z4")
            (list_literal
              (lc "a")
              (lc "b")
            )
          )
          (malformed)
          (binop_colon
            (lc "a")
            (lc "a")
          )
        )
      )
      (args
        (lc "x")
        (lc "y")
      )
    )
  )
  (malformed)
)
~~~
# FORMATTED
~~~roc
module []

# Import exposing
import I1 exposing [
	I11,
	I12,
]
import I2 exposing [I21]
as 
Ias1
,
	
I22
as 
Ias2
,
]

# Where constraint
A(a) : (a where module(a).a1 : (a, a)) -> Str, module(a).a2 : (a, a) -> Str
B(b) : (b where module(b).b1 : (b, b)) -> Str, module(b).b2 : (b, b) -> Str
C(
	(a, b),
) : (a, b)
D(
	(a, b),
) :
	C(a, b)
E :
	{
		a: Str,
		b: Str,
	}
F : [
	A,
	B,
]
g : e -> e where module(e) | A, module(e) | B
h = |
	x,
	y,
| {
	h1 = {
		h11: x,
		h12: x,
		h13: {
			h131: x,
			h132: y,
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
	Z2(
		(a, b),
	)
	=> 
	a : a
	Z3(
		{
			a: a,
			b: b,
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
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - everything.md:14:1:23:11
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - everything.md:24:1:33:11
MODULE NOT FOUND - everything.md:4:1:7:2
MODULE NOT FOUND - everything.md:8:1:11:2
UNUSED VARIABLE - everything.md:90:5:90:6
UNUSED VARIABLE - everything.md:95:4:95:5
UNUSED VARIABLE - everything.md:100:5:100:6
UNUSED VARIABLE - everything.md:106:5:106:6
UNUSED VARIABLE - everything.md:61:2:61:4
UNUSED VARIABLE - everything.md:69:2:69:4
UNUSED VARIABLE - everything.md:73:2:73:4
UNUSED VARIABLE - everything.md:77:2:77:4
UNUSED VARIABLE - everything.md:81:2:81:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**everything.md:9:6:9:9:**
```roc
	I21 as Ias1,
```
	    ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**everything.md:9:13:10:2:**
```roc
	I21 as Ias1,
	I22 as Ias2,
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**everything.md:10:6:10:9:**
```roc
	I22 as Ias2,
```
	    ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**everything.md:10:13:11:1:**
```roc
	I22 as Ias2,
]
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **]

# Where constraint
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**everything.md:11:1:14:1:**
```roc
]

# Where constraint
A(a) : a
```


**UNEXPECTED TOKEN IN PATTERN**
The token **) ** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

**everything.md:92:3:92:5:**
```roc
		) => a
```
		^^


**PARSE ERROR**
A parsing error occurred: **expected_close_round**
This is an unexpected parsing error. Please check your syntax.

**everything.md:92:5:92:8:**
```roc
		) => a
```
		  ^^^


**PARSE ERROR**
A parsing error occurred: **expected_arrow_after_pattern**
This is an unexpected parsing error. Please check your syntax.

**everything.md:92:8:93:3:**
```roc
		) => a
		Z2(
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**everything.md:96:5:96:8:**
```roc
		) => a
```
		  ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**everything.md:102:5:102:8:**
```roc
		) => a
```
		  ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**everything.md:108:5:108:8:**
```roc
		) => a
```
		  ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**everything.md:110:1:110:2:**
```roc
}
```
^


**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**everything.md:14:8:22:5:**
```roc
A(a) : a
	where
		module(a).a1 : (
			a,
			a,
		) -> Str,
		module(a).a2 : (
			a,
			a,
```


**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**everything.md:24:8:32:5:**
```roc
B(b) : b
	where
		module(b).b1 : (
			b,
			b,
		) -> Str,
		module(b).b2 : (
			b,
			b,
```


**INVALID WHERE CONSTRAINT**
Invalid where clause constraint syntax.
Where clauses should contain valid ability constraints.

**everything.md:58:24:58:27:**
```roc
g : e -> e where module(e).A, module(e).B
```
                       ^^^


**INVALID WHERE CONSTRAINT**
Invalid where clause constraint syntax.
Where clauses should contain valid ability constraints.

**everything.md:58:27:58:29:**
```roc
g : e -> e where module(e).A, module(e).B
```
                          ^^


**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**everything.md:58:37:58:42:**
```roc
g : e -> e where module(e).A, module(e).B
```
                                    ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **a** in this scope.
Is there an **import** or **exposing** missing up-top?

**everything.md:94:4:94:5:**
```roc
			a,
```
			^


**UNDEFINED VARIABLE**
Nothing is named **b** in this scope.
Is there an **import** or **exposing** missing up-top?

**everything.md:95:4:95:5:**
```roc
			b,
```
			^


**UNDEFINED VARIABLE**
Nothing is named **b** in this scope.
Is there an **import** or **exposing** missing up-top?

**everything.md:106:5:106:6:**
```roc
				b,
```
				^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.import)
  (Expr.malformed)
  (Expr.tag_no_args)
  (Expr.malformed)
  (Expr.tag_no_args)
  (Expr.malformed)
  (Expr.tag_no_args)
  (Expr.malformed)
  (Expr.malformed)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "g"))
    (type type_121)
  )
  (Stmt.assign
    (pattern (Patt.ident "h"))
    (Expr.lambda (canonicalized))
  )
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 232
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
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 _)
(var #35 _)
(var #36 _)
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
(var #48 _)
(var #49 _)
(var #50 _)
(var #51 _)
(var #52 _)
(var #53 _)
(var #54 _)
(var #55 _)
(var #56 _)
(var #57 _)
(var #58 _)
(var #59 _)
(var #60 _)
(var #61 _)
(var #62 _)
(var #63 _)
(var #64 _)
(var #65 _)
(var #66 _)
(var #67 _)
(var #68 _)
(var #69 _)
(var #70 _)
(var #71 _)
(var #72 _)
(var #73 _)
(var #74 _)
(var #75 _)
(var #76 _)
(var #77 _)
(var #78 _)
(var #79 _)
(var #80 _)
(var #81 _)
(var #82 _)
(var #83 _)
(var #84 _)
(var #85 _)
(var #86 _)
(var #87 _)
(var #88 _)
(var #89 _)
(var #90 _)
(var #91 _)
(var #92 _)
(var #93 _)
(var #94 _)
(var #95 _)
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
(var #107 _)
(var #108 _)
(var #109 _)
(var #110 _)
(var #111 _)
(var #112 _)
(var #113 _)
(var #114 _)
(var #115 _)
(var #116 _)
(var #117 _)
(var #118 _)
(var #119 _)
(var #120 _)
(var #121 _)
(var #122 _)
(var #123 -> #230)
(var #124 _)
(var #125 _)
(var #126 -> #216)
(var #127 _)
(var #128 _)
(var #129 _)
(var #130 _)
(var #131 _)
(var #132 _)
(var #133 _)
(var #134 _)
(var #135 _)
(var #136 _)
(var #137 _)
(var #138 _)
(var #139 _)
(var #140 _)
(var #141 _)
(var #142 -> #216)
(var #143 _)
(var #144 -> #149)
(var #145 -> #218)
(var #146 _)
(var #147 _)
(var #148 -> #217)
(var #149 _)
(var #150 _)
(var #151 -> #156)
(var #152 -> #220)
(var #153 _)
(var #154 _)
(var #155 -> #219)
(var #156 _)
(var #157 _)
(var #158 -> #161)
(var #159 _)
(var #160 _)
(var #161 _)
(var #162 _)
(var #163 -> #221)
(var #164 _)
(var #165 _)
(var #166 -> #221)
(var #167 _)
(var #168 _)
(var #169 _)
(var #170 _)
(var #171 _)
(var #172 _)
(var #173 _)
(var #174 _)
(var #175 _)
(var #176 _)
(var #177 _)
(var #178 -> #223)
(var #179 _)
(var #180 _)
(var #181 -> #222)
(var #182 _)
(var #183 _)
(var #184 _)
(var #185 _)
(var #186 -> #226)
(var #187 _)
(var #188 _)
(var #189 _)
(var #190 _)
(var #191 -> #225)
(var #192 _)
(var #193 _)
(var #194 _)
(var #195 _)
(var #196 -> #228)
(var #197 _)
(var #198 _)
(var #199 _)
(var #200 _)
(var #201 _)
(var #202 _)
(var #203 _)
(var #204 _)
(var #205 -> #230)
(var #206 _)
(var #207 _)
(var #208 _)
(var #209 _)
(var #210 _)
(var #211 _)
(var #212 _)
(var #213 _)
(var #214 _)
(var #215 _)
(var #216 {})
(var #217 tuple)
(var #218 fn_pure)
(var #219 tuple)
(var #220 fn_pure)
(var #221 tuple)
(var #222 tuple)
(var #223 fn_pure)
(var #224 _)
(var #225 {})
(var #226 fn_pure)
(var #227 _)
(var #228 fn_pure)
(var #229 _)
(var #230 fn_pure)
(var #231 _)
~~~
# TYPES
~~~roc
h1 : {}
h2 : _c
x : _c
h3 : _c
h5 : (_field, _field2)
y : _c
h4 : _c
g : _c
h : _arg, _arg2 -> _ret
a : _c
~~~
