# META
~~~ini
description=sub_minus_o_apply_minus_crash_bang fail
type=expr
~~~
# SOURCE
~~~roc
h-
-o -crash!
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - sub_minus_o_apply_minus_crash_bang.md:2:1:2:3
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **-o** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**sub_minus_o_apply_minus_crash_bang.md:2:1:2:3:**
```roc
-o -crash!
```
^^


**UNDEFINED VARIABLE**
Nothing is named `h` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpBinaryMinus(1:2-1:3),Newline(1:1-1:1),
OpUnaryMinus(2:1-2:2),LowerIdent(2:2-2:3),OpUnaryMinus(2:4-2:5),LowerIdent(2:5-2:11),EndOfFile(2:11-2:11),
~~~
# PARSE
~~~clojure
(e-binop @1.1-2.3 (op "-")
	(e-ident @1.1-1.2 (raw "h"))
	(e-malformed @2.1-2.3 (reason "expr_unexpected_token")))
~~~
# FORMATTED
~~~roc
h -
	
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-2.3 (op "sub")
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-runtime-error (tag "expr_not_canonicalized")))
~~~
# TYPES
~~~clojure
(expr @1.1-2.3 (type "*"))
~~~
