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
KwModule OpenSquare LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent CloseSquare LowerIdent OpAssign MultilineString UpperIdent LowerIdent LowerIdent String KwWith LowerIdent LowerIdent LowerIdent LowerIdent OpAssign MultilineString UpperIdent LowerIdent LowerIdent String KwWith LowerIdent LowerIdent LowerIdent LowerIdent OpAssign MultilineString UpperIdent LowerIdent LowerIdent LowerIdent MultilineString UpperIdent LowerIdent LowerIdent MultilineString MalformedUnknownToken OpenCurly LowerIdent CloseCurly LowerIdent OpAssign MultilineString UpperIdent LowerIdent LowerIdent LowerIdent MultilineString UpperIdent LowerIdent LowerIdent MultilineString MalformedUnknownToken OpenCurly LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "value1")
    (str_literal_small "")
  )
  (uc "This")
  (lc "is")
  (lc "a")
  (str_literal_big "string")
  (malformed malformed:expr_unexpected_token)
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
  (malformed malformed:expr_unexpected_token)
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
  (malformed malformed:expr_unexpected_token)
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
  (malformed malformed:expr_unexpected_token)
  (block
    (lc "value2")
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 3:32 to 3:32

**Parse Error**
at 6:24 to 6:24

**Parse Error**
at 10:5 to 10:5

**Parse Error**
at 16:5 to 16:5

**Unsupported Node**
at 3:32 to 3:32

**Unsupported Node**
at 6:24 to 6:24

**Unsupported Node**
at 10:5 to 10:5

**Unsupported Node**
at 16:5 to 16:5

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.apply_tag)
  (Expr.lookup "is")
  (Expr.lookup "a")
  (Expr.str_literal_big)
  (Expr.malformed)
  (Expr.lookup "just")
  (Expr.lookup "one")
  (Expr.lookup "line")
  (Expr.malformed)
  (Expr.apply_tag)
  (Expr.lookup "is")
  (Expr.lookup "a")
  (Expr.str_literal_big)
  (Expr.malformed)
  (Expr.lookup "just")
  (Expr.lookup "one")
  (Expr.lookup "line")
  (Expr.malformed)
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
  (Expr.malformed)
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
~~~
