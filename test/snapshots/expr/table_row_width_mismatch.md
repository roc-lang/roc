# META
~~~ini
description=Table row with the wrong number of values
type=expr
~~~
# SOURCE
~~~roc
table name, age {
    "Bob", 12, "extra",
}
~~~
# EXPECTED
TABLE ROW WIDTH - table_row_width_mismatch.md:2:5:2:24
# PROBLEMS
── ✗ table row width ─────────────────────────── table_row_width_mismatch.md:2:5

I was parsing a table row, and it does not have the same number of values as
there are columns.

"Bob", 12, "extra",
^^^^^^^^^^^^^^^^^^^

Each row must have one value per column, separated by commas. A newline starts
the next row.

For example:
    table name, age {
        "Ada", 36,
        "Alan", 42,
    }

I found "Bob", 12, "extra", here.

# TOKENS
~~~zig
LowerIdent,LowerIdent,Comma,LowerIdent,OpenCurly,
StringStart,StringPart,StringEnd,Comma,Int,Comma,StringStart,StringPart,StringEnd,Comma,
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
