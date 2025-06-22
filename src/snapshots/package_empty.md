# META
~~~ini
description=Empty package header
type=header
~~~
# SOURCE
~~~roc
package [] {}
~~~
# PROBLEMS
~~~txt
NIL
~~~
# TOKENS
~~~zig
KwPackage(1:1-1:8),OpenSquare(1:9-1:10),CloseSquare(1:10-1:11),OpenCurly(1:12-1:13),CloseCurly(1:13-1:14),EndOfFile(1:14-1:14),
~~~
# PARSE
~~~clojure
(package (1:1-1:14)
	(exposes (1:9-1:11))
	(packages (1:12-1:14)))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can_ir "empty")
~~~
# TYPES
~~~clojure
(inferred_types (defs) (expressions))
~~~