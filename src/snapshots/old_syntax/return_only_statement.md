# META
~~~ini
description=return_only_statement
type=expr
~~~
# SOURCE
~~~roc
identityFn = \x ->
  return     x


identityFn 45
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `identityFn` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:11),OpAssign(1:12-1:13),OpBackslash(1:14-1:15),LowerIdent(1:15-1:16),OpArrow(1:17-1:19),Newline(1:1-1:1),
KwReturn(2:3-2:9),LowerIdent(2:14-2:15),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(5:1-5:11),Int(5:12-5:14),EndOfFile(5:14-5:14),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.11 (qaul "") (raw "identityFn"))
~~~
# FORMATTED
~~~roc
identityFn
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
