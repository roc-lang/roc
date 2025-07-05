# META
~~~ini
description=return_in_when
type=expr
~~~
# SOURCE
~~~roc
maybeEarlyReturn = \x ->
    y =
    when x is
        5 ->
                    return
                        "abc"

        _ -> x + 2


    Num.to_str y

maybeEarlyRetun 3
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `maybeEarlyReturn` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:17),OpAssign(1:18-1:19),OpBackslash(1:20-1:21),LowerIdent(1:21-1:22),OpArrow(1:23-1:25),Newline(1:1-1:1),
LowerIdent(2:5-2:6),OpAssign(2:7-2:8),Newline(1:1-1:1),
LowerIdent(3:5-3:9),LowerIdent(3:10-3:11),LowerIdent(3:12-3:14),Newline(1:1-1:1),
Int(4:9-4:10),OpArrow(4:11-4:13),Newline(1:1-1:1),
KwReturn(5:21-5:27),Newline(1:1-1:1),
StringStart(6:25-6:26),StringPart(6:26-6:29),StringEnd(6:29-6:30),Newline(1:1-1:1),
Newline(1:1-1:1),
Underscore(8:9-8:10),OpArrow(8:11-8:13),LowerIdent(8:14-8:15),OpPlus(8:16-8:17),Int(8:18-8:19),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(11:5-11:8),NoSpaceDotLowerIdent(11:8-11:15),LowerIdent(11:16-11:17),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(13:1-13:16),Int(13:17-13:18),EndOfFile(13:18-13:18),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.17 (raw "maybeEarlyReturn"))
~~~
# FORMATTED
~~~roc
maybeEarlyReturn
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.17 (type "Error"))
~~~
