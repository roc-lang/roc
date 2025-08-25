# META
~~~ini
description=Empty package header
type=header
~~~
# SOURCE
~~~roc
package [] {}
~~~
# TOKENS
~~~text
KwPackage OpenSquare CloseSquare OpenCurly CloseCurly ~~~
# PARSE
~~~clojure
(header-only)
~~~
# FORMATTED
~~~roc
package []

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
; No expression to type check
~~~
# TYPES
~~~roc
# Header type not yet fully supported
~~~
