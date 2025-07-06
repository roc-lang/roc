# META
~~~ini
description=multiline_binop_when_with_comments
type=expr
~~~
# SOURCE
~~~roc
when
    x
    + 1 # comment 1
    > 0 # comment 2
is
    y ->
        3
        * 2 # comment 3
        < 1 # comment 4

    z ->
        4
            / 5  # comment 5
                < 1    # comment 6

    46 # first pattern comment
    | 95 # alternative comment 1
    | 126 # alternative comment 2
    | 150 -> # This comment came after the ->
        # This comment is for the expr
            foo bar
                |> Result.withDefault "" # one last comment

    _ ->
        42
~~~
# EXPECTED
UNDEFINED VARIABLE - multiline_binop_when_with_comments.md:1:1:1:5
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

**multiline_binop_when_with_comments.md:1:1:1:5:**
```roc
when
```
^^^^


# TOKENS
~~~zig
LowerIdent(1:1-1:5),Newline(1:1-1:1),
LowerIdent(2:5-2:6),Newline(1:1-1:1),
OpPlus(3:5-3:6),Int(3:7-3:8),Newline(3:10-3:20),
OpGreaterThan(4:5-4:6),Int(4:7-4:8),Newline(4:10-4:20),
LowerIdent(5:1-5:3),Newline(1:1-1:1),
LowerIdent(6:5-6:6),OpArrow(6:7-6:9),Newline(1:1-1:1),
Int(7:9-7:10),Newline(1:1-1:1),
OpStar(8:9-8:10),Int(8:11-8:12),Newline(8:14-8:24),
OpLessThan(9:9-9:10),Int(9:11-9:12),Newline(9:14-9:24),
Newline(1:1-1:1),
LowerIdent(11:5-11:6),OpArrow(11:7-11:9),Newline(1:1-1:1),
Int(12:9-12:10),Newline(1:1-1:1),
OpSlash(13:13-13:14),Int(13:15-13:16),Newline(13:19-13:29),
OpLessThan(14:17-14:18),Int(14:19-14:20),Newline(14:25-14:35),
Newline(1:1-1:1),
Int(16:5-16:7),Newline(16:9-16:31),
OpBar(17:5-17:6),Int(17:7-17:9),Newline(17:11-17:33),
OpBar(18:5-18:6),Int(18:7-18:10),Newline(18:12-18:34),
OpBar(19:5-19:6),Int(19:7-19:10),OpArrow(19:11-19:13),Newline(19:15-19:46),
Newline(20:10-20:39),
LowerIdent(21:13-21:16),LowerIdent(21:17-21:20),Newline(1:1-1:1),
OpPizza(22:17-22:19),UpperIdent(22:20-22:26),NoSpaceDotLowerIdent(22:26-22:38),StringStart(22:39-22:40),StringPart(22:40-22:40),StringEnd(22:40-22:41),Newline(22:43-22:60),
Newline(1:1-1:1),
Underscore(24:5-24:6),OpArrow(24:7-24:9),Newline(1:1-1:1),
Int(25:9-25:11),EndOfFile(25:11-25:11),
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
