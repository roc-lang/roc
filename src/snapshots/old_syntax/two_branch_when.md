# META
~~~ini
description=two_branch_when
type=expr
~~~
# SOURCE
~~~roc
when x is
 "" -> 1
 "mise" -> 2
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:5),LowerIdent(1:6-1:7),LowerIdent(1:8-1:10),Newline(1:1-1:1),
StringStart(2:2-2:3),StringPart(2:3-2:3),StringEnd(2:3-2:4),OpArrow(2:5-2:7),Int(2:8-2:9),Newline(1:1-1:1),
StringStart(3:2-3:3),StringPart(3:3-3:7),StringEnd(3:7-3:8),OpArrow(3:9-3:11),Int(3:12-3:13),EndOfFile(3:13-3:13),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.5 (raw "when"))
~~~
# FORMATTED
~~~roc
when
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.5 (type "Error"))
~~~
