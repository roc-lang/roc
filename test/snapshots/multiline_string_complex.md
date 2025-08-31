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
# A comment in between
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


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**multiline_string_complex.md:3:13:3:17:**
```roc
value1 = """This is a "string" with just one line
```
            ^^^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**multiline_string_complex.md:3:18:3:20:**
```roc
value1 = """This is a "string" with just one line
```
                 ^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**multiline_string_complex.md:3:21:3:22:**
```roc
value1 = """This is a "string" with just one line
```
                    ^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**multiline_string_complex.md:3:23:3:31:**
```roc
value1 = """This is a "string" with just one line
```
                      ^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**multiline_string_complex.md:3:32:3:37:**
```roc
value1 = """This is a "string" with just one line
```
                               ^^^^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**multiline_string_complex.md:3:37:3:41:**
```roc
value1 = """This is a "string" with just one line
```
                                    ^^^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**multiline_string_complex.md:3:42:3:45:**
```roc
value1 = """This is a "string" with just one line
```
                                         ^^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**multiline_string_complex.md:3:46:3:50:**
```roc
value1 = """This is a "string" with just one line
```
                                             ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**multiline_string_complex.md:6:5:6:9:**
```roc
	"""This is a "string" with just one line
```
	   ^^^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**multiline_string_complex.md:6:10:6:12:**
```roc
	"""This is a "string" with just one line
```
	        ^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**multiline_string_complex.md:6:13:6:14:**
```roc
	"""This is a "string" with just one line
```
	           ^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**multiline_string_complex.md:6:15:6:23:**
```roc
	"""This is a "string" with just one line
```
	             ^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**multiline_string_complex.md:6:24:6:29:**
```roc
	"""This is a "string" with just one line
```
	                      ^^^^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**multiline_string_complex.md:6:29:6:33:**
```roc
	"""This is a "string" with just one line
```
	                           ^^^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**multiline_string_complex.md:6:34:6:37:**
```roc
	"""This is a "string" with just one line
```
	                                ^^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**multiline_string_complex.md:6:38:6:42:**
```roc
	"""This is a "string" with just one line
```
	                                    ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**multiline_string_complex.md:8:13:8:17:**
```roc
value3 = """This is a string
```
            ^^^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**multiline_string_complex.md:8:18:8:20:**
```roc
value3 = """This is a string
```
                 ^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**multiline_string_complex.md:8:21:8:22:**
```roc
value3 = """This is a string
```
                    ^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**multiline_string_complex.md:8:23:8:29:**
```roc
value3 = """This is a string
```
                      ^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**multiline_string_complex.md:9:2:9:5:**
```roc
	"""With multiple lines
```
	^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**multiline_string_complex.md:9:5:9:9:**
```roc
	"""With multiple lines
```
	   ^^^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**multiline_string_complex.md:9:10:9:18:**
```roc
	"""With multiple lines
```
	        ^^^^^^^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**multiline_string_complex.md:9:19:9:24:**
```roc
	"""With multiple lines
```
	                 ^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**multiline_string_complex.md:10:2:10:5:**
```roc
	"""${value1}
```
	^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**multiline_string_complex.md:10:5:10:6:**
```roc
	"""${value1}
```
	   ^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**multiline_string_complex.md:10:6:10:14:**
```roc
	"""${value1}
```
	    ^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**multiline_string_complex.md:13:5:13:9:**
```roc
	"""This is a string
```
	   ^^^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**multiline_string_complex.md:13:10:13:12:**
```roc
	"""This is a string
```
	        ^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**multiline_string_complex.md:13:13:13:14:**
```roc
	"""This is a string
```
	           ^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**multiline_string_complex.md:13:15:13:21:**
```roc
	"""This is a string
```
	             ^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**multiline_string_complex.md:15:2:15:5:**
```roc
	"""With multiple lines
```
	^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**multiline_string_complex.md:15:5:15:9:**
```roc
	"""With multiple lines
```
	   ^^^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**multiline_string_complex.md:15:10:15:18:**
```roc
	"""With multiple lines
```
	        ^^^^^^^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**multiline_string_complex.md:15:19:15:24:**
```roc
	"""With multiple lines
```
	                 ^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**multiline_string_complex.md:16:2:16:5:**
```roc
	"""${value2}
```
	^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**multiline_string_complex.md:16:5:16:6:**
```roc
	"""${value2}
```
	   ^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**multiline_string_complex.md:16:6:16:14:**
```roc
	"""${value2}
```
	    ^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "value1"))
    (Expr.str_literal_small)
  )
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.assign
    (pattern (Patt.ident "value2"))
    (Expr.str_literal_big)
  )
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.assign
    (pattern (Patt.ident "value3"))
    (Expr.str_literal_big)
  )
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.assign
    (pattern (Patt.ident "value4"))
    (Expr.str_literal_big)
  )
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
~~~
