# META
~~~ini
description=Test import statement
type=file
~~~

# SOURCE
~~~roc
module []
import Foo
~~~

# TOKENS
~~~zig
KwModule(0-6),OpenSquare(7-8),CloseSquare(8-9),KwImport(10-16),UpperIdent(17-20),EndOfFile(20-20)
~~~

# PARSE_AST2
~~~clojure
(file
  (module
    (exposes)
  )
  (import (uc "Foo" @17) @10)
)

~~~
