# META
~~~ini
description=multiline_string
type=expr
~~~
# SOURCE
~~~roc
a = "Hello,\n\nWorld!"
b = """Hello,\n\nWorld!"""
c =
    """
    Hello,

    World!
    """
42
~~~
# EXPECTED
UNDEFINED VARIABLE - multiline_string.md:1:1:1:2
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `a` in this scope.
Is there an `import` or `exposing` missing up-top?

**multiline_string.md:1:1:1:2:**
```roc
a = "Hello,\n\nWorld!"
```
^


# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpAssign(1:3-1:4),StringStart(1:5-1:6),StringPart(1:6-1:22),StringEnd(1:22-1:23),Newline(1:1-1:1),
LowerIdent(2:1-2:2),OpAssign(2:3-2:4),MultilineStringStart(2:5-2:8),StringPart(2:8-2:24),MultilineStringEnd(2:24-2:27),Newline(1:1-1:1),
LowerIdent(3:1-3:2),OpAssign(3:3-3:4),Newline(1:1-1:1),
MultilineStringStart(4:5-4:8),StringPart(4:8-4:8),Newline(1:1-1:1),
UpperIdent(5:5-5:10),Comma(5:10-5:11),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(7:5-7:11),Newline(1:1-1:1),
MultilineStringStart(8:5-8:8),StringPart(8:8-8:8),Newline(1:1-1:1),
Int(9:1-9:3),EndOfFile(9:3-9:3),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "a"))
~~~
# FORMATTED
~~~roc
a
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
