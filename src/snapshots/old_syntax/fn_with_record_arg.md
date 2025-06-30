# META
~~~ini
description=fn_with_record_arg
type=expr
~~~
# SOURCE
~~~roc
table : {
    height : Pixels
    } -> Table
table = \{height} -> crash "not implemented"
table
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `table` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:6),OpColon(1:7-1:8),OpenCurly(1:9-1:10),Newline(1:1-1:1),
LowerIdent(2:5-2:11),OpColon(2:12-2:13),UpperIdent(2:14-2:20),Newline(1:1-1:1),
CloseCurly(3:5-3:6),OpArrow(3:7-3:9),UpperIdent(3:10-3:15),Newline(1:1-1:1),
LowerIdent(4:1-4:6),OpAssign(4:7-4:8),OpBackslash(4:9-4:10),OpenCurly(4:10-4:11),LowerIdent(4:11-4:17),CloseCurly(4:17-4:18),OpArrow(4:19-4:21),KwCrash(4:22-4:27),StringStart(4:28-4:29),StringPart(4:29-4:44),StringEnd(4:44-4:45),Newline(1:1-1:1),
LowerIdent(5:1-5:6),EndOfFile(5:6-5:6),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.6 (qaul "") (raw "table"))
~~~
# FORMATTED
~~~roc
table
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
