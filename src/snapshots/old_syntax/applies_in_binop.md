# META
~~~ini
description=applies_in_binop
type=expr
~~~
# SOURCE
~~~roc
Str.getUnsafe haystack haystackIndex
==
Str.getUnsafe needle needleIndex
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `getUnsafe` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
UpperIdent(1:1-1:4),NoSpaceDotLowerIdent(1:4-1:14),LowerIdent(1:15-1:23),LowerIdent(1:24-1:37),Newline(1:1-1:1),
OpEquals(2:1-2:3),Newline(1:1-1:1),
UpperIdent(3:1-3:4),NoSpaceDotLowerIdent(3:4-3:14),LowerIdent(3:15-3:21),LowerIdent(3:22-3:33),EndOfFile(3:33-3:33),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.14 (raw "Str.getUnsafe"))
~~~
# FORMATTED
~~~roc
Str.getUnsafe
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.14 (type "Error"))
~~~
