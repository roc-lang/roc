# META
~~~ini
description=Dec literal with negative exponent scientific notation
type=expr
~~~
# SOURCE
~~~roc
1.23456789012345678e-10
~~~
# EXPECTED
INVALID NUMBER - dec_scientific_negative_exp.md:1:1:1:24
# PROBLEMS

┌────────────────┐
│ INVALID NUMBER ├─ This number literal does not fit in the inferred type. ───┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  1.23456789012345678e-10                                                   │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                   │
 └──────────────────────────────────────── dec_scientific_negative_exp.md:1:1 ┘

    The inferred type is:

        Dec

# TOKENS
~~~zig
Float,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-frac (raw "1.23456789012345678e-10"))
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
