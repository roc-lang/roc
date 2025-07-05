# META
~~~ini
description=pattern_as_spaces
type=expr
~~~
# SOURCE
~~~roc
when 0 is
    0 # foobar
        as # barfoo
        n -> {}
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:5),Int(1:6-1:7),LowerIdent(1:8-1:10),Newline(1:1-1:1),
Int(2:5-2:6),Newline(2:8-2:15),
KwAs(3:9-3:11),Newline(3:13-3:20),
LowerIdent(4:9-4:10),OpArrow(4:11-4:13),OpenCurly(4:14-4:15),CloseCurly(4:15-4:16),Newline(1:1-1:1),
MalformedUnknownToken(5:1-5:2),MalformedUnknownToken(5:2-5:3),MalformedUnknownToken(5:3-5:4),EndOfFile(5:4-5:4),
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
