# META
~~~ini
description=return_in_if
type=expr
~~~
# SOURCE
~~~roc
maybeEarlyReturn = \x ->
    y =
        if x > 5 then
            return "abc"
        else
            x + 2

    Num.to_str y

maybeEarlyReturn 10
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `maybeEarlyReturn` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:17),OpAssign(1:18-1:19),OpBackslash(1:20-1:21),LowerIdent(1:21-1:22),OpArrow(1:23-1:25),Newline(1:1-1:1),
LowerIdent(2:5-2:6),OpAssign(2:7-2:8),Newline(1:1-1:1),
KwIf(3:9-3:11),LowerIdent(3:12-3:13),OpGreaterThan(3:14-3:15),Int(3:16-3:17),LowerIdent(3:18-3:22),Newline(1:1-1:1),
KwReturn(4:13-4:19),StringStart(4:20-4:21),StringPart(4:21-4:24),StringEnd(4:24-4:25),Newline(1:1-1:1),
KwElse(5:9-5:13),Newline(1:1-1:1),
LowerIdent(6:13-6:14),OpPlus(6:15-6:16),Int(6:17-6:18),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(8:5-8:8),NoSpaceDotLowerIdent(8:8-8:15),LowerIdent(8:16-8:17),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(10:1-10:17),Int(10:18-10:20),EndOfFile(10:20-10:20),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.17 (qaul "") (raw "maybeEarlyReturn"))
~~~
# FORMATTED
~~~roc
maybeEarlyReturn
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
