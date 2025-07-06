# META
~~~ini
description=list_patterns
type=expr
~~~
# SOURCE
~~~roc
when [] is
  [] -> {}
  [..] -> {}
  [_, .., _, ..] -> {}
  [a, b, c, d] -> {}
  [a, b, ..] -> {}
  [.., c, d] -> {}
  [[A], [..], [a]] -> {}
  [[[], []], [[], x]] -> {}
~~~
# EXPECTED
UNDEFINED VARIABLE - list_patterns.md:1:1:1:5
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

**list_patterns.md:1:1:1:5:**
```roc
when [] is
```
^^^^


# TOKENS
~~~zig
LowerIdent(1:1-1:5),OpenSquare(1:6-1:7),CloseSquare(1:7-1:8),LowerIdent(1:9-1:11),Newline(1:1-1:1),
OpenSquare(2:3-2:4),CloseSquare(2:4-2:5),OpArrow(2:6-2:8),OpenCurly(2:9-2:10),CloseCurly(2:10-2:11),Newline(1:1-1:1),
OpenSquare(3:3-3:4),DoubleDot(3:4-3:6),CloseSquare(3:6-3:7),OpArrow(3:8-3:10),OpenCurly(3:11-3:12),CloseCurly(3:12-3:13),Newline(1:1-1:1),
OpenSquare(4:3-4:4),Underscore(4:4-4:5),Comma(4:5-4:6),DoubleDot(4:7-4:9),Comma(4:9-4:10),Underscore(4:11-4:12),Comma(4:12-4:13),DoubleDot(4:14-4:16),CloseSquare(4:16-4:17),OpArrow(4:18-4:20),OpenCurly(4:21-4:22),CloseCurly(4:22-4:23),Newline(1:1-1:1),
OpenSquare(5:3-5:4),LowerIdent(5:4-5:5),Comma(5:5-5:6),LowerIdent(5:7-5:8),Comma(5:8-5:9),LowerIdent(5:10-5:11),Comma(5:11-5:12),LowerIdent(5:13-5:14),CloseSquare(5:14-5:15),OpArrow(5:16-5:18),OpenCurly(5:19-5:20),CloseCurly(5:20-5:21),Newline(1:1-1:1),
OpenSquare(6:3-6:4),LowerIdent(6:4-6:5),Comma(6:5-6:6),LowerIdent(6:7-6:8),Comma(6:8-6:9),DoubleDot(6:10-6:12),CloseSquare(6:12-6:13),OpArrow(6:14-6:16),OpenCurly(6:17-6:18),CloseCurly(6:18-6:19),Newline(1:1-1:1),
OpenSquare(7:3-7:4),DoubleDot(7:4-7:6),Comma(7:6-7:7),LowerIdent(7:8-7:9),Comma(7:9-7:10),LowerIdent(7:11-7:12),CloseSquare(7:12-7:13),OpArrow(7:14-7:16),OpenCurly(7:17-7:18),CloseCurly(7:18-7:19),Newline(1:1-1:1),
OpenSquare(8:3-8:4),OpenSquare(8:4-8:5),UpperIdent(8:5-8:6),CloseSquare(8:6-8:7),Comma(8:7-8:8),OpenSquare(8:9-8:10),DoubleDot(8:10-8:12),CloseSquare(8:12-8:13),Comma(8:13-8:14),OpenSquare(8:15-8:16),LowerIdent(8:16-8:17),CloseSquare(8:17-8:18),CloseSquare(8:18-8:19),OpArrow(8:20-8:22),OpenCurly(8:23-8:24),CloseCurly(8:24-8:25),Newline(1:1-1:1),
OpenSquare(9:3-9:4),OpenSquare(9:4-9:5),OpenSquare(9:5-9:6),CloseSquare(9:6-9:7),Comma(9:7-9:8),OpenSquare(9:9-9:10),CloseSquare(9:10-9:11),CloseSquare(9:11-9:12),Comma(9:12-9:13),OpenSquare(9:14-9:15),OpenSquare(9:15-9:16),CloseSquare(9:16-9:17),Comma(9:17-9:18),LowerIdent(9:19-9:20),CloseSquare(9:20-9:21),CloseSquare(9:21-9:22),OpArrow(9:23-9:25),OpenCurly(9:26-9:27),CloseCurly(9:27-9:28),EndOfFile(9:28-9:28),
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
