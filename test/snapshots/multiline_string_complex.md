# META
~~~ini
description=multiline_string_complex
type=file
~~~
# SOURCE
~~~roc
package
	[]
	{
		x: """Multiline
		,
	}

value1 = """This is a "string" with just one line

value2 = 
	"""This is a "string" with just one line

value3 = """This is a string
	"""With multiple lines
	"""${value1}

value4 = 
	"""This is a string
	# A comment in between
	"""With multiple lines
	"""${value2}

value5 = {
	a: """Multiline
	,
	b: (
		"""Multiline
		,
		"""Multiline
		,
	),
	c: [
		"""multiline
		,
	],
	d: (
		0 - """
		,
	),
	e: !"""
	,
}

x = {
	"""
	"""
}
~~~
# TOKENS
~~~text
KwPackage OpenSquare CloseSquare OpenCurly LowerIdent OpColon MultilineString UpperIdent Comma CloseCurly BlankLine LowerIdent OpAssign MultilineString UpperIdent LowerIdent LowerIdent String KwWith LowerIdent LowerIdent LowerIdent BlankLine LowerIdent OpAssign MultilineString UpperIdent LowerIdent LowerIdent String KwWith LowerIdent LowerIdent LowerIdent BlankLine LowerIdent OpAssign MultilineString UpperIdent LowerIdent LowerIdent LowerIdent MultilineString UpperIdent LowerIdent LowerIdent MultilineString MalformedUnknownToken OpenCurly LowerIdent CloseCurly BlankLine LowerIdent OpAssign MultilineString UpperIdent LowerIdent LowerIdent LowerIdent LineComment MultilineString UpperIdent LowerIdent LowerIdent MultilineString MalformedUnknownToken OpenCurly LowerIdent CloseCurly BlankLine LowerIdent OpAssign OpenCurly LowerIdent OpColon MultilineString UpperIdent Comma LowerIdent OpColon OpenRound MultilineString UpperIdent Comma MultilineString UpperIdent Comma CloseRound Comma LowerIdent OpColon OpenSquare MultilineString LowerIdent Comma CloseSquare Comma LowerIdent OpColon OpenRound Int OpBinaryMinus MultilineString Comma CloseRound Comma LowerIdent OpColon OpBang MultilineString Comma CloseCurly BlankLine LowerIdent OpAssign OpenCurly MultilineString MultilineString CloseCurly ~~~
# PARSE
~~~clojure
(package-header
  (packages
    (lc "x")

    (str_literal_big "string")
))
(block
  (uc "Multiline")
  (malformed)
  (malformed)
  (binop_equals
    (lc "value1")
    (str_literal_big "string")
  )
  (uc "This")
  (lc "is")
  (lc "a")
  (str_literal_big "string")
  (malformed)
  (lc "just")
  (lc "one")
  (lc "line")
  (binop_equals
    (lc "value2")
    (str_literal_big "string")
  )
  (uc "This")
  (lc "is")
  (lc "a")
  (str_literal_big "string")
  (malformed)
  (lc "just")
  (lc "one")
  (lc "line")
  (binop_equals
    (lc "value3")
    (str_literal_big "string")
  )
  (uc "This")
  (lc "is")
  (lc "a")
  (lc "string")
  (str_literal_big "string")
  (uc "With")
  (lc "multiple")
  (lc "lines")
  (str_literal_big "string")
  (malformed)
  (block
    (lc "value1")
  )
  (binop_equals
    (lc "value4")
    (str_literal_big "string")
  )
  (uc "This")
  (lc "is")
  (lc "a")
  (lc "string")
  (str_literal_big "string")
  (uc "With")
  (lc "multiple")
  (lc "lines")
  (str_literal_big "string")
  (malformed)
  (block
    (lc "value2")
  )
  (binop_equals
    (lc "value5")
    (record_literal
      (binop_colon
        (lc "a")
        (str_literal_big "string")
      )
      (uc "Multiline")
      (binop_colon
        (lc "b")
        (str_literal_big "string")
      )
    )
  )
  (uc "Multiline")
  (malformed)
  (str_literal_big "string")
  (uc "Multiline")
  (malformed)
  (malformed)
  (malformed)
  (binop_colon
    (lc "c")
    (list_literal
      (str_literal_big "string")
    )
  )
  (lc "multiline")
  (malformed)
  (malformed)
  (malformed)
  (binop_colon
    (lc "d")
    (binop_colon
      (tuple_literal
        (binop_minus
          (num_literal_i32 0)
          (str_literal_big "string")
        )
        (lc "e")
      )
      (unary_not <unary_op>)
    )
  )
  (malformed)
  (binop_equals
    (lc "x")
    (block
      (str_literal_big "string")
      (str_literal_big "string")
    )
  )
)
~~~
# FORMATTED
~~~roc
package [] packages {x, """Multiline
		,
	}

value1 = """}

Multiline
,
	
}

value1 = """This is a "string" with just one line

value2 = 
	"""

This
is
a
"string"
with 
just
one
line
value2 = """This is a "string" with just one line

value3 = """

This
is
a
"string"
with 
just
one
line
value3 = """This is a string
	"""

This
is
a
string
"""With multiple lines
	"""
With
multiple
lines
"""${value1}

value4 = 
	"""
$
{
	value1
}
value4 = """This is a string
	# A comment in between
	"""

This
is
a
string
# A comment in between
"""With multiple lines
	"""
With
multiple
lines
"""${value2}

value5 = {
	a: """
$
{
	value2
}
value5 = { a: """Multiline
	,
	b: (
		""", Multiline, b: """Multiline
		,
		""" }

Multiline
,
		
"""Multiline
		,
	),
	c: [
		"""
Multiline
,
	
)
,
	
c : ["""multiline
		,
	],
	d: (
		0 - """]
multiline
,
	
]
,
	
d : (
	0 - """
		,
	),
	e: !""",
	e,
) : !"""
	,
}

x = {
	"""
}

x = {
	"""
	"""
	"""
}
~~~
# EXPECTED
TYPE MISMATCH - multiline_string_complex.md:37:7:37:10
TYPE MISMATCH - multiline_string_complex.md:40:5:40:9
# PROBLEMS
**EXPECTED CLOSE CURLY BRACE**
A parsing error occurred: **expected_packages_close_curly**
This is an unexpected parsing error. Please check your syntax.

**multiline_string_complex.md:1:1:4:9:**
```roc
package
	[]
	{
		x: """Multiline
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**multiline_string_complex.md:5:3:6:2:**
```roc
		,
	}
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}

** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**multiline_string_complex.md:6:2:8:1:**
```roc
	}

value1 = """This is a "string" with just one line
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **with ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**multiline_string_complex.md:8:32:8:37:**
```roc
value1 = """This is a "string" with just one line
```
                               ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **with ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**multiline_string_complex.md:11:24:11:29:**
```roc
	"""This is a "string" with just one line
```
	                      ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **$** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**multiline_string_complex.md:15:5:15:6:**
```roc
	"""${value1}
```
	   ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **$** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**multiline_string_complex.md:21:5:21:6:**
```roc
	"""${value2}
```
	   ^


**PARSE ERROR**
A parsing error occurred: **expected_expr_close_round_or_comma**
This is an unexpected parsing error. Please check your syntax.



**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**multiline_string_complex.md:23:10:27:6:**
```roc
value5 = {
	a: """Multiline
	,
	b: (
		"""Multiline
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,
		** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**multiline_string_complex.md:28:3:29:3:**
```roc
		,
		"""Multiline
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**multiline_string_complex.md:30:3:31:2:**
```roc
		,
	),
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **)** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**multiline_string_complex.md:31:2:31:3:**
```roc
	),
```
	^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**multiline_string_complex.md:31:3:32:2:**
```roc
	),
	c: [
```


**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**multiline_string_complex.md:32:5:33:6:**
```roc
	c: [
		"""multiline
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**multiline_string_complex.md:34:3:35:2:**
```roc
		,
	],
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **]** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**multiline_string_complex.md:35:2:35:3:**
```roc
	],
```
	^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**multiline_string_complex.md:35:3:36:2:**
```roc
	],
	d: (
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}

** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**multiline_string_complex.md:42:1:44:1:**
```roc
}

x = {
```


**UNDEFINED VARIABLE**
Nothing is named **is** in this scope.
Is there an **import** or **exposing** missing up-top?

**multiline_string_complex.md:8:18:8:20:**
```roc
value1 = """This is a "string" with just one line
```
                 ^^


**UNDEFINED VARIABLE**
Nothing is named **a** in this scope.
Is there an **import** or **exposing** missing up-top?

**multiline_string_complex.md:8:21:8:22:**
```roc
value1 = """This is a "string" with just one line
```
                    ^


**UNDEFINED VARIABLE**
Nothing is named **just** in this scope.
Is there an **import** or **exposing** missing up-top?

**multiline_string_complex.md:8:37:8:41:**
```roc
value1 = """This is a "string" with just one line
```
                                    ^^^^


**UNDEFINED VARIABLE**
Nothing is named **one** in this scope.
Is there an **import** or **exposing** missing up-top?

**multiline_string_complex.md:8:42:8:45:**
```roc
value1 = """This is a "string" with just one line
```
                                         ^^^


**UNDEFINED VARIABLE**
Nothing is named **line** in this scope.
Is there an **import** or **exposing** missing up-top?

**multiline_string_complex.md:8:46:8:50:**
```roc
value1 = """This is a "string" with just one line
```
                                             ^^^^


**UNDEFINED VARIABLE**
Nothing is named **is** in this scope.
Is there an **import** or **exposing** missing up-top?

**multiline_string_complex.md:11:10:11:12:**
```roc
	"""This is a "string" with just one line
```
	        ^^


**UNDEFINED VARIABLE**
Nothing is named **a** in this scope.
Is there an **import** or **exposing** missing up-top?

**multiline_string_complex.md:11:13:11:14:**
```roc
	"""This is a "string" with just one line
```
	           ^


**UNDEFINED VARIABLE**
Nothing is named **just** in this scope.
Is there an **import** or **exposing** missing up-top?

**multiline_string_complex.md:11:29:11:33:**
```roc
	"""This is a "string" with just one line
```
	                           ^^^^


**UNDEFINED VARIABLE**
Nothing is named **one** in this scope.
Is there an **import** or **exposing** missing up-top?

**multiline_string_complex.md:11:34:11:37:**
```roc
	"""This is a "string" with just one line
```
	                                ^^^


**UNDEFINED VARIABLE**
Nothing is named **line** in this scope.
Is there an **import** or **exposing** missing up-top?

**multiline_string_complex.md:11:38:11:42:**
```roc
	"""This is a "string" with just one line
```
	                                    ^^^^


**UNDEFINED VARIABLE**
Nothing is named **is** in this scope.
Is there an **import** or **exposing** missing up-top?

**multiline_string_complex.md:13:18:13:20:**
```roc
value3 = """This is a string
```
                 ^^


**UNDEFINED VARIABLE**
Nothing is named **a** in this scope.
Is there an **import** or **exposing** missing up-top?

**multiline_string_complex.md:13:21:13:22:**
```roc
value3 = """This is a string
```
                    ^


**UNDEFINED VARIABLE**
Nothing is named **string** in this scope.
Is there an **import** or **exposing** missing up-top?

**multiline_string_complex.md:13:23:13:29:**
```roc
value3 = """This is a string
```
                      ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **multiple** in this scope.
Is there an **import** or **exposing** missing up-top?

**multiline_string_complex.md:14:10:14:18:**
```roc
	"""With multiple lines
```
	        ^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **lines** in this scope.
Is there an **import** or **exposing** missing up-top?

**multiline_string_complex.md:14:19:14:24:**
```roc
	"""With multiple lines
```
	                 ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **is** in this scope.
Is there an **import** or **exposing** missing up-top?

**multiline_string_complex.md:18:10:18:12:**
```roc
	"""This is a string
```
	        ^^


**UNDEFINED VARIABLE**
Nothing is named **a** in this scope.
Is there an **import** or **exposing** missing up-top?

**multiline_string_complex.md:18:13:18:14:**
```roc
	"""This is a string
```
	           ^


**UNDEFINED VARIABLE**
Nothing is named **string** in this scope.
Is there an **import** or **exposing** missing up-top?

**multiline_string_complex.md:18:15:18:21:**
```roc
	"""This is a string
```
	             ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **multiple** in this scope.
Is there an **import** or **exposing** missing up-top?

**multiline_string_complex.md:20:10:20:18:**
```roc
	"""With multiple lines
```
	        ^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **lines** in this scope.
Is there an **import** or **exposing** missing up-top?

**multiline_string_complex.md:20:19:20:24:**
```roc
	"""With multiple lines
```
	                 ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **multiline** in this scope.
Is there an **import** or **exposing** missing up-top?

**multiline_string_complex.md:33:6:33:15:**
```roc
		"""multiline
```
		   ^^^^^^^^^


**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**multiline_string_complex.md:37:3:40:6:**
```roc
		0 - """
		,
	),
	e: !"""
```


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.tag_no_args)
  (Expr.malformed)
  (Expr.malformed)
  (Stmt.assign
    (pattern (Patt.ident "value1"))
    (Expr.str_literal_big)
  )
  (Expr.tag_no_args)
  (Expr.lookup "is")
  (Expr.lookup "a")
  (Expr.str_literal_big)
  (Expr.malformed)
  (Expr.lookup "just")
  (Expr.lookup "one")
  (Expr.lookup "line")
  (Stmt.assign
    (pattern (Patt.ident "value2"))
    (Expr.str_literal_big)
  )
  (Expr.tag_no_args)
  (Expr.lookup "is")
  (Expr.lookup "a")
  (Expr.str_literal_big)
  (Expr.malformed)
  (Expr.lookup "just")
  (Expr.lookup "one")
  (Expr.lookup "line")
  (Stmt.assign
    (pattern (Patt.ident "value3"))
    (Expr.str_literal_big)
  )
  (Expr.tag_no_args)
  (Expr.lookup "is")
  (Expr.lookup "a")
  (Expr.lookup "string")
  (Expr.str_literal_big)
  (Expr.tag_no_args)
  (Expr.lookup "multiple")
  (Expr.lookup "lines")
  (Expr.str_literal_big)
  (Expr.malformed)
  (Expr.block
    (Expr.lookup "value1")
  )
  (Stmt.assign
    (pattern (Patt.ident "value4"))
    (Expr.str_literal_big)
  )
  (Expr.tag_no_args)
  (Expr.lookup "is")
  (Expr.lookup "a")
  (Expr.lookup "string")
  (Expr.str_literal_big)
  (Expr.tag_no_args)
  (Expr.lookup "multiple")
  (Expr.lookup "lines")
  (Expr.str_literal_big)
  (Expr.malformed)
  (Expr.block
    (Expr.lookup "value2")
  )
  (Stmt.assign
    (pattern (Patt.ident "value5"))
    (Expr.record_literal
      (Expr.record_field
        (Expr.malformed)
        (Expr.str_literal_big)
      )
      (Expr.tag_no_args)
      (Expr.record_field
        (Expr.malformed)
        (Expr.str_literal_big)
      )
    )
  )
  (Expr.tag_no_args)
  (Expr.malformed)
  (Expr.str_literal_big)
  (Expr.tag_no_args)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "c"))
    (type type_77)
  )
  (Expr.lookup "multiline")
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "d"))
    (type type_91)
  )
  (Expr.malformed)
  (Stmt.assign
    (pattern (Patt.ident "x"))
    (Expr.block
      (Expr.str_literal_big)
      (Expr.str_literal_big)
    )
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 115
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 -> #7)
(var #7 Str)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 Str)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 -> #18)
(var #18 Str)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 Str)
(var #24 _)
(var #25 _)
(var #26 _)
(var #27 _)
(var #28 -> #29)
(var #29 Str)
(var #30 _)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 _)
(var #35 Str)
(var #36 _)
(var #37 _)
(var #38 _)
(var #39 Str)
(var #40 _)
(var #41 _)
(var #42 _)
(var #43 -> #44)
(var #44 Str)
(var #45 _)
(var #46 _)
(var #47 _)
(var #48 _)
(var #49 _)
(var #50 Str)
(var #51 _)
(var #52 _)
(var #53 _)
(var #54 Str)
(var #55 _)
(var #56 _)
(var #57 _)
(var #58 -> #106)
(var #59 _)
(var #60 Str)
(var #61 _)
(var #62 _)
(var #63 _)
(var #64 Str)
(var #65 _)
(var #66 -> #106)
(var #67 _)
(var #68 _)
(var #69 _)
(var #70 Str)
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
(var #94 -> #97)
(var #95 Str)
(var #96 Str)
(var #97 _)
(var #98 _)
(var #99 _)
(var #100 _)
(var #101 _)
(var #102 _)
(var #103 _)
(var #104 _)
(var #105 _)
(var #106 {})
(var #107 _)
(var #108 _)
(var #109 _)
(var #110 _)
(var #111 _)
(var #112 _)
(var #113 _)
(var #114 _)
~~~
# TYPES
~~~roc
value2 : Str
value1 : Str
c : _f
x : _f
d : _f
value3 : Str
value4 : Str
value5 : {}
~~~
