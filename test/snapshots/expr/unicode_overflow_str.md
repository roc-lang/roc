# META
~~~ini
description=Unicode overflow (should error)
type=expr
~~~
# SOURCE
~~~roc
"\u(FFFFFF)"
~~~
# EXPECTED
INVALID UNICODE ESCAPE SEQUENCE - unicode_overflow_str.md:1:2:1:12
# PROBLEMS
                                             ┌─────────────────────────────────┐
┌─ This Unicode escape sequence is not ──────┤ INVALID UNICODE ESCAPE SEQUENCE │
│  valid.                                    └────────────────────────────────┬┘
│                                                                             │
│  "\u(FFFFFF)"                                                               │
│   ‾‾‾‾‾‾‾‾‾‾                                                                │
└─────────────────────────────────────────────────────────────────────────────┘
    unicode_overflow_str.md:1:2

# TOKENS
~~~zig
StringStart,MalformedStringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-string)
~~~
# FORMATTED
~~~roc
""
~~~
# CANONICALIZE
~~~clojure
(e-string)
~~~
# TYPES
~~~clojure
(expr (type "Str"))
~~~
