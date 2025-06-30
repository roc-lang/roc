# META
~~~ini
description=record_double_newline_comment_field
type=expr
~~~
# SOURCE
~~~roc
{

#
}
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly(1:1-1:2),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:2),
CloseCurly(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-record @1.1-4.2)
~~~
# FORMATTED
~~~roc
{}
~~~
# CANONICALIZE
~~~clojure
(e-empty_record @1.1-4.2)
~~~
# TYPES
~~~clojure
(expr @1.1-4.2 (type "{}"))
~~~
