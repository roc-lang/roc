# META
~~~ini
description=Bug test: If-then-else expression
type=expr
~~~

# SOURCE
~~~roc
if True 1 else 2
~~~

# TOKENS
~~~zig
KwIf(0-2),UpperIdent(3-7),Int(8-9),KwElse(10-14),Int(15-16),EndOfFile(16-16)
~~~

# PARSE_AST2
~~~clojure
(if_else(uc "True" @3) (num_literal_i32 1 @8) (num_literal_i32 2 @15) @0)

~~~
