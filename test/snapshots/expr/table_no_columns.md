# META
~~~ini
description=table {} is invalid because a table needs at least one column
type=expr
~~~
# SOURCE
~~~roc
table {}
~~~
# EXPECTED
EXPECTED TABLE COLUMN - table_no_columns.md:1:7:1:8
# PROBLEMS
── ✗ expected table column ───────────────────────────── table_no_columns.md:1:7

I was parsing a table literal, and I expected a lowercase column name.

table {}
      ^

A table starts with table followed by one or more column names, then a { body.
Each column can optionally have a type after :.

For example:
    table name, age {
        "Ada", 36,
    }

I found { here.

# TOKENS
~~~zig
LowerIdent,OpenCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-malformed (reason "table_expected_column_name"))
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
