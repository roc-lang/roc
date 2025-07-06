# META
~~~ini
description=single_arg_with_underscore_closure
type=expr
~~~
# SOURCE
~~~roc
\the_answer -> 42
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - single_arg_with_underscore_closure.md:1:1:1:12
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **\the_answer** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**single_arg_with_underscore_closure.md:1:1:1:12:**
```roc
\the_answer -> 42
```
^^^^^^^^^^^


# TOKENS
~~~zig
OpBackslash(1:1-1:2),LowerIdent(1:2-1:12),OpArrow(1:13-1:15),Int(1:16-1:18),EndOfFile(1:18-1:18),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.12 (reason "expr_unexpected_token"))
~~~
# FORMATTED
~~~roc

~~~
# CANONICALIZE
~~~clojure
(can-ir (empty true))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
