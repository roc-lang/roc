# META
~~~ini
description=Invalid integer literal that exceeds i128 range
type=expr
~~~
# SOURCE
~~~roc
99999999999999999999999999999999999999999
~~~
# EXPECTED
INVALID NUMBER - expr_int_invalid.md:1:1:1:42
# PROBLEMS

┌────────────────┐
│ INVALID NUMBER ├─ This number literal does not fit in the inferred type. ───┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  99999999999999999999999999999999999999999                                 │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                 │
 └─────────────────────────────────────────────────── expr_int_invalid.md:1:1 ┘

    The inferred type is:

        Dec

# TOKENS
~~~zig
Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-int (raw "99999999999999999999999999999999999999999"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-num-from-numeral)
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
