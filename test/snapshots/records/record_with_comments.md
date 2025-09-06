# META
~~~ini
description=Record creation with comments
type=expr
~~~
# SOURCE
~~~roc
{
	# comment1
	..item,

	# comment2

	person: { name: "Alice", age: 30 }, # comment3
	address: {
		# comment4
		street: "123 Main St",
		# comment5

		city: "Springfield",
		coordinates: { lat: 42.1234, lng: -71.5678 },
	},
	contact: {

		# comment6
		email: "alice@example.com",
		phone: { home: "555-1234", work: "555-5678" }, # comment7
	},
	# comment8
}
~~~
# TOKENS
~~~text
OpenCurly LineComment DoubleDot LowerIdent Comma BlankLine LineComment BlankLine LowerIdent OpColon OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon Int CloseCurly Comma LineComment LowerIdent OpColon OpenCurly LineComment LowerIdent OpColon String Comma LineComment BlankLine LowerIdent OpColon String Comma LowerIdent OpColon OpenCurly LowerIdent OpColon Float Comma LowerIdent OpColon OpUnaryMinus Float CloseCurly Comma CloseCurly Comma LowerIdent OpColon OpenCurly BlankLine LineComment LowerIdent OpColon String Comma LowerIdent OpColon OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon String CloseCurly Comma LineComment CloseCurly Comma LineComment CloseCurly ~~~
# PARSE
~~~clojure
(malformed)
~~~
# FORMATTED
~~~roc
# comment1

# comment2

person# comment3
# comment4
# comment5
# comment6
# comment7
# comment8
~~~
# EXPECTED
UNDEFINED VARIABLE - record_with_comments.md:3:4:3:8
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**record_with_comments.md:1:1:7:2:**
```roc
{
	# comment1
	..item,

	# comment2

	person: { name: "Alice", age: 30 }, # comment3
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **person** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_with_comments.md:7:2:7:8:**
```roc
	person: { name: "Alice", age: 30 }, # comment3
```
	^^^^^^


# CANONICALIZE
~~~clojure
(Expr.malformed)
~~~
# SOLVED
~~~clojure
; Total type variables: 5
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
~~~
# TYPES
~~~roc
~~~
