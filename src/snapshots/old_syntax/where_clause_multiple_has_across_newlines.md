# META
~~~ini
description=where_clause_multiple_has_across_newlines
type=expr
~~~
# SOURCE
~~~roc
f : a -> (b -> c)
    where a implements Hash,
      b implements Eq,
      c implements Ord

f
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `f` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:3-1:4),LowerIdent(1:5-1:6),OpArrow(1:7-1:9),OpenRound(1:10-1:11),LowerIdent(1:11-1:12),OpArrow(1:13-1:15),LowerIdent(1:16-1:17),CloseRound(1:17-1:18),Newline(1:1-1:1),
KwWhere(2:5-2:10),LowerIdent(2:11-2:12),KwImplements(2:13-2:23),UpperIdent(2:24-2:28),Comma(2:28-2:29),Newline(1:1-1:1),
LowerIdent(3:7-3:8),KwImplements(3:9-3:19),UpperIdent(3:20-3:22),Comma(3:22-3:23),Newline(1:1-1:1),
LowerIdent(4:7-4:8),KwImplements(4:9-4:19),UpperIdent(4:20-4:23),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(6:1-6:2),EndOfFile(6:2-6:2),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (qaul "") (raw "f"))
~~~
# FORMATTED
~~~roc
f
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
