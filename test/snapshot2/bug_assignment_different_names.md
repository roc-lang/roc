# META
~~~ini
description=Assignment with different names to avoid confusion
type=file
~~~

# SOURCE
~~~roc
module [foo]
bar = 99
~~~

# TOKENS
~~~zig
KwModule(0-6),OpenSquare(7-8),LowerIdent(8-11),CloseSquare(11-12),LowerIdent(13-16),OpAssign(17-18),Int(19-21),EndOfFile(21-21)
~~~

# PARSE_AST2
~~~clojure
(file
  (module
    (exposes (lc "foo" @8))
  )
  (= (lc "bar" @13) (num 99 @19) @17)
)

~~~
