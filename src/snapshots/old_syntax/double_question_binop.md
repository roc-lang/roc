# META
~~~ini
description=double_question_binop
type=expr
~~~
# SOURCE
~~~roc
get_name! {} ?? "Bob"
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `get_name!` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:10),OpenCurly(1:11-1:12),CloseCurly(1:12-1:13),OpDoubleQuestion(1:14-1:16),StringStart(1:17-1:18),StringPart(1:18-1:21),StringEnd(1:21-1:22),EndOfFile(1:22-1:22),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.10 (qaul "") (raw "get_name!"))
~~~
# FORMATTED
~~~roc
get_name!
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
