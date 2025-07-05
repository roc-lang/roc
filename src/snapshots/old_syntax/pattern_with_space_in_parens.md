# META
~~~ini
description=pattern_with_space_in_parens
type=expr
~~~
# SOURCE
~~~roc
when Delmin (Del rx) 0 is
    Delmin (Del ry ) _ -> Node Black 0 Bool.false ry
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:5),UpperIdent(1:6-1:12),OpenRound(1:13-1:14),UpperIdent(1:14-1:17),LowerIdent(1:18-1:20),CloseRound(1:20-1:21),Int(1:22-1:23),LowerIdent(1:24-1:26),Newline(1:1-1:1),
UpperIdent(2:5-2:11),OpenRound(2:12-2:13),UpperIdent(2:13-2:16),LowerIdent(2:17-2:19),CloseRound(2:20-2:21),Underscore(2:22-2:23),OpArrow(2:24-2:26),UpperIdent(2:27-2:31),UpperIdent(2:32-2:37),Int(2:38-2:39),UpperIdent(2:40-2:44),NoSpaceDotLowerIdent(2:44-2:50),LowerIdent(2:51-2:53),EndOfFile(2:53-2:53),
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
