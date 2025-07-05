# META
~~~ini
description=implements_in_pnc_pattern
type=expr
~~~
# SOURCE
~~~roc
g(implements,x)=c
c
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - implements_in_pnc_pattern.md:1:3:1:14
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **implements,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**implements_in_pnc_pattern.md:1:3:1:14:**
```roc
g(implements,x)=c
```
  ^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `g` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),NoSpaceOpenRound(1:2-1:3),KwImplements(1:3-1:13),Comma(1:13-1:14),LowerIdent(1:14-1:15),CloseRound(1:15-1:16),OpAssign(1:16-1:17),LowerIdent(1:17-1:18),Newline(1:1-1:1),
LowerIdent(2:1-2:2),EndOfFile(2:2-2:2),
~~~
# PARSE
~~~clojure
(e-apply @1.1-1.16
	(e-ident @1.1-1.2 (raw "g"))
	(e-malformed @1.3-1.14 (reason "expr_unexpected_token"))
	(e-ident @1.14-1.15 (raw "x")))
~~~
# FORMATTED
~~~roc
g(, x)
~~~
# CANONICALIZE
~~~clojure
(e-call @1.1-1.16
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-runtime-error (tag "ident_not_in_scope")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.16 (type "*"))
~~~
