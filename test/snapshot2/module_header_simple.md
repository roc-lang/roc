# META
~~~ini
description=Simple module header with exposed values
type=file
~~~

# SOURCE
~~~roc
module [something, SomeType]
~~~

# TOKENS
~~~zig
KwModule(0-6),OpenSquare(7-8),LowerIdent(8-17),Comma(17-18),UpperIdent(19-27),CloseSquare(27-28),EndOfFile(28-28)
~~~

# PARSE_AST2
~~~clojure
(file
  (module
    (exposes (lc "something" @8), (uc "SomeType" @19))
  )
)

~~~
