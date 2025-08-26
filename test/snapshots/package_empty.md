# META
~~~ini
description=Empty package header
type=header
~~~
# SOURCE
~~~roc
package [] {}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwPackage(1:1-1:8),OpenSquare(1:9-1:10),CloseSquare(1:10-1:11),OpenCurly(1:12-1:13),CloseCurly(1:13-1:14),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(package @1.1-1.14
	(exposes @1.9-1.11)
	(packages @1.12-1.14))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir (empty true))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
