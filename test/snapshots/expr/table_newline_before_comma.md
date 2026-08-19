# META
~~~ini
description=Body-depth newline before a comma ends the table row (width error, not a silent merge)
type=expr
~~~
# SOURCE
~~~roc
table name, age {
    "Bob"
    , 12,
    "Alice", 17,
}
~~~
# EXPECTED
TABLE ROW WIDTH - table_newline_before_comma.md:2:5:2:10
# PROBLEMS
── ✗ table row width ───────────────────────── table_newline_before_comma.md:2:5

I was parsing a table row, and it does not have the same number of values as
there are columns.

"Bob"
^^^^^

Each row must have one value per column, separated by commas. A newline starts
the next row.

For example:
    table name, age {
        "Ada", 36,
        "Alan", 42,
    }

I found "Bob" here.

# TOKENS
~~~zig
LowerIdent,LowerIdent,Comma,LowerIdent,OpenCurly,
StringStart,StringPart,StringEnd,
Comma,Int,Comma,
StringStart,StringPart,StringEnd,Comma,Int,Comma,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-malformed (reason "table_row_width_mismatch"))
~~~
# FORMATTED
~~~roc

~~~
# CANONICALIZE
~~~clojure
(can-ir (empty true))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
