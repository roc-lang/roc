# META
~~~ini
description=List with integer literals
type=expr
~~~

# SOURCE
~~~roc
[1, 2, 3]
~~~

# TOKENS
~~~zig
OpenSquare(0-1),Int(1-2),Comma(2-3),Int(4-5),Comma(5-6),Int(7-8),CloseSquare(8-9),EndOfFile(9-9)
~~~

# PARSE_AST2
~~~clojure
(num_literal_i32 1 @1)

~~~
