# META
~~~ini
description=record_func_type_decl
type=expr
~~~
# SOURCE
~~~roc
f :
    {
        getLine : Effect Str,
        putLine : Str -> Effect Int,
        text: Str,
        value: Int *
    }

42
~~~
# EXPECTED
UNDEFINED VARIABLE - record_func_type_decl.md:1:1:1:2
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `f` in this scope.
Is there an `import` or `exposing` missing up-top?

**record_func_type_decl.md:1:1:1:2:**
```roc
f :
```
^


# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:3-1:4),Newline(1:1-1:1),
OpenCurly(2:5-2:6),Newline(1:1-1:1),
LowerIdent(3:9-3:16),OpColon(3:17-3:18),UpperIdent(3:19-3:25),UpperIdent(3:26-3:29),Comma(3:29-3:30),Newline(1:1-1:1),
LowerIdent(4:9-4:16),OpColon(4:17-4:18),UpperIdent(4:19-4:22),OpArrow(4:23-4:25),UpperIdent(4:26-4:32),UpperIdent(4:33-4:36),Comma(4:36-4:37),Newline(1:1-1:1),
LowerIdent(5:9-5:13),OpColon(5:13-5:14),UpperIdent(5:15-5:18),Comma(5:18-5:19),Newline(1:1-1:1),
LowerIdent(6:9-6:14),OpColon(6:14-6:15),UpperIdent(6:16-6:19),OpStar(6:20-6:21),Newline(1:1-1:1),
CloseCurly(7:5-7:6),Newline(1:1-1:1),
Newline(1:1-1:1),
Int(9:1-9:3),EndOfFile(9:3-9:3),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "f"))
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
