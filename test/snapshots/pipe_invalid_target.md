# META
~~~ini
description=Pipe target diagnostic
type=expr
~~~
# SOURCE
~~~roc
1 |> 2
~~~
# EXPECTED
EXPECTED PIPE TARGET - pipe_invalid_target.md:1:6:1:7
# PROBLEMS
── ✗ expected pipe target ─────────────────────────── pipe_invalid_target.md:1:6

I was parsing a pipe expression, and I expected a name or parenthesized
expression after `|>`.

1 |> 2
     ^

The right side of a pipe must start with a value name, tag name, or
parenthesized expression.

For example:
    value |> next

I found 2 here.

# TOKENS
~~~zig
Int,OpPizza,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-malformed (reason "expr_pipe_expects_ident"))
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
