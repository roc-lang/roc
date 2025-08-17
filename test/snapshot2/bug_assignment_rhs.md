# META
~~~ini
description=Bug test: Assignment RHS should be the value, not the identifier
type=file
~~~

# SOURCE
~~~roc
module [x]
x = 42
~~~

# TOKENS
~~~zig
KwModule(0-6),OpenSquare(7-8),LowerIdent(8-9),CloseSquare(9-10),LowerIdent(11-12),OpAssign(13-14),Int(15-17),EndOfFile(17-17)
~~~

# PARSE_AST2
~~~clojure
(file
  (module-header
    (exposes (lc "x" @8))
  )
  (statements
    (binop_equals @13
      lhs: (lc "x" @8)
      rhs: (lc "x" @11)
  )
)

~~~
