# META
~~~ini
description=Table with a duplicate column name
type=expr
~~~
# SOURCE
~~~roc
table name, name {
    "Bob", "Robert",
}
~~~
# EXPECTED
DUPLICATE TABLE COLUMN - table_duplicate_column.md:1:13:1:17
# PROBLEMS
── ✗ duplicate table column ───────────────────── table_duplicate_column.md:1:13

I was parsing a table literal, and this column name is already used.

table name, name {
            ^^^^

Each column name in a table must be unique.

For example:
    table name, age {
        "Ada", 36,
    }

I found name here.
Names that start with lowercase letters are value names or record field names,
depending on the surrounding syntax.

# TOKENS
~~~zig
LowerIdent,LowerIdent,Comma,LowerIdent,OpenCurly,
StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,Comma,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-malformed (reason "table_duplicate_column"))
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
