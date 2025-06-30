# META
~~~ini
description=return_multiline
type=expr
~~~
# SOURCE
~~~roc
return
        something
            |> pipeToFunction
                |> andAnother
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**return_multiline.md:1:1:1:1:**
```roc
return
```



# TOKENS
~~~zig
KwReturn(1:1-1:7),Newline(1:1-1:1),
LowerIdent(2:9-2:18),Newline(1:1-1:1),
OpPizza(3:13-3:15),LowerIdent(3:16-3:30),Newline(1:1-1:1),
OpPizza(4:17-4:19),LowerIdent(4:20-4:30),EndOfFile(4:30-4:30),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.1 (reason "expr_unexpected_token"))
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
