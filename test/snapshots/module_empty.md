# META
~~~ini
description=An empty module with no exposes
type=file
~~~
# SOURCE
~~~roc
module []
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(empty)
~~~
# SOLVED
~~~clojure
; Total type variables: 0
~~~
# TYPES
~~~roc
~~~
