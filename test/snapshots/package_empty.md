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
(package-header)
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
~~~
# TYPES
~~~roc
# Header type not yet fully supported
~~~
