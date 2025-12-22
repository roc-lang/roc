# META
~~~ini
description=Mono test: True tag
type=mono
~~~
# SOURCE
~~~roc
True
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-tag (raw "True"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-tag (name "True"))
~~~
# TYPES
~~~clojure
(expr (type "[True, .._others]"))
~~~
# MONO
~~~roc
True
~~~
