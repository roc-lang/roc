# META
~~~ini
description=Lambda expression with correct Roc syntax
type=file
~~~

# SOURCE
~~~roc
module [f]
f = |x| x + 1
~~~

# TOKENS
~~~zig
KwModule(0-6),OpenSquare(7-8),LowerIdent(8-9),CloseSquare(9-10),LowerIdent(11-12),OpAssign(13-14),OpBar(15-16),LowerIdent(16-17),OpBar(17-18),LowerIdent(19-20),OpPlus(21-22),Int(23-24),EndOfFile(24-24)
~~~

# PARSE_AST2
~~~clojure
(file
  (module
    (exposes (lc "f" @8))
  )
  (= (lc "f" @11) (lambda [(lc "x" @16)] (+ (lc "x" @19) (num 1 @23) @21) @15) @13)
)

~~~
