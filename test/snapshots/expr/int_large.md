# META
~~~ini
description=Large integer literal
type=expr
~~~
# SOURCE
~~~roc
999999999999999999999999999999
~~~
# EXPECTED
INVALID NUMBER - int_large.md:1:1:1:31
# PROBLEMS

┌────────────────┐
│ INVALID NUMBER ├─ This number literal does not fit in the inferred type. ───┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  999999999999999999999999999999                                            │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                            │
 └────────────────────────────────────────────────────────── int_large.md:1:1 ┘

    The inferred type is:

        Dec

# TOKENS
~~~zig
Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-int (raw "999999999999999999999999999999"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-num (value "999999999999999999999999999999"))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
