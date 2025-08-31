# META
~~~ini
description=multiline_string_complex
type=file
~~~
# SOURCE
~~~roc
module [value1, value2, value3, value4]

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
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent CloseSquare BlankLine LowerIdent OpAssign MultilineString UpperIdent LowerIdent LowerIdent String KwWith LowerIdent LowerIdent LowerIdent BlankLine LowerIdent OpAssign MultilineString UpperIdent LowerIdent LowerIdent String KwWith LowerIdent LowerIdent LowerIdent BlankLine LowerIdent OpAssign MultilineString UpperIdent LowerIdent LowerIdent LowerIdent MultilineString UpperIdent LowerIdent LowerIdent MultilineString MalformedUnknownToken OpenCurly LowerIdent CloseCurly BlankLine LowerIdent OpAssign MultilineString UpperIdent LowerIdent LowerIdent LowerIdent LineComment MultilineString UpperIdent LowerIdent LowerIdent MultilineString MalformedUnknownToken OpenCurly LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "value1")

    (lc "value2")

    (lc "value3")

    (lc "value4")
))
~~~
# FORMATTED
~~~roc
module [value1, value2, value3, value4]

value1 = ""
This
is
a
"string"
with 
just
one
line
value2 = ""
This
is
a
"string"
with 
just
one
line
value3 = ""
This
is
a
string
""
With
multiple
lines
""
$
{
	value1
}
value4 = ""
This
is
a
string
""
With
multiple
lines
""
$
{
	value2
}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **with ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**multiline_string_complex.md:3:32:3:37:**
```roc
value1 = """This is a "string" with just one line
```
                               ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **with ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**multiline_string_complex.md:6:24:6:29:**
```roc
	"""This is a "string" with just one line
```
	                      ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **$** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**multiline_string_complex.md:10:5:10:6:**
```roc
	"""${value1}
```
	   ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **$** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**multiline_string_complex.md:16:5:16:6:**
```roc
	"""${value2}
```
	   ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_equals
    (Expr.lookup "value1")
    (Expr.str_literal_small)
  )
  (Expr.apply_tag)
  (Expr.lookup "is")
  (Expr.lookup "a")
  (Expr.str_literal_big)
  (Expr.malformed)
  (Expr.lookup "just")
  (Expr.lookup "one")
  (Expr.lookup "line")
  (Expr.binop_equals
    (Expr.lookup "value2")
    (Expr.str_literal_big)
  )
  (Expr.apply_tag)
  (Expr.lookup "is")
  (Expr.lookup "a")
  (Expr.str_literal_big)
  (Expr.malformed)
  (Expr.lookup "just")
  (Expr.lookup "one")
  (Expr.lookup "line")
  (Expr.binop_equals
    (Expr.lookup "value3")
    (Expr.str_literal_big)
  )
  (Expr.apply_tag)
  (Expr.lookup "is")
  (Expr.lookup "a")
  (Expr.lookup "string")
  (Expr.str_literal_big)
  (Expr.apply_tag)
  (Expr.lookup "multiple")
  (Expr.lookup "lines")
  (Expr.str_literal_big)
  (Expr.malformed)
  (Expr.block
    (Expr.lookup "value1")
  )
  (Expr.binop_equals
    (Expr.lookup "value4")
    (Expr.str_literal_big)
  )
  (Expr.apply_tag)
  (Expr.lookup "is")
  (Expr.lookup "a")
  (Expr.lookup "string")
  (Expr.str_literal_big)
  (Expr.apply_tag)
  (Expr.lookup "multiple")
  (Expr.lookup "lines")
  (Expr.str_literal_big)
  (Expr.malformed)
  (Expr.block
    (Expr.lookup "value2")
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
value1 : Str
value2 : Str
value3 : Str
value4 : Str
~~~
