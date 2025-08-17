# META
~~~ini
description=Simple function with string literal
type=file
~~~

# SOURCE
~~~roc
module [greet]
greet = "Hello, World!"
~~~

# TOKENS
~~~zig
KwModule(0-6),OpenSquare(7-8),LowerIdent(8-13),CloseSquare(13-14),LowerIdent(15-20),OpAssign(21-22),StringStart(23-24),StringPart(24-37),StringEnd(37-38),EndOfFile(38-38)
~~~

# PARSE_AST2
~~~clojure
(file
  (module-header
    (exposes (lc "greet" @8))
  )
  (statements
    (binop_equals @21
      lhs: (lc "greet" @8)
      rhs: (lc "greet" @15)
  )
)

~~~
