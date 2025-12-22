# META
~~~ini
description=Mono test: True tag
type=mono
~~~
# SOURCE
~~~roc
True
~~~
# MONO
~~~roc
True : [True, .._others]
~~~
# FORMATTED
~~~roc
NO CHANGE
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
# CANONICALIZE
~~~clojure
(e-tag (name "True"))
~~~
# TYPES
~~~clojure
(expr (type "[True, .._others]"))
~~~
