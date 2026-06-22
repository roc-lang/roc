# META
~~~ini
description=Unclosed string literal
type=expr
~~~
# SOURCE
~~~roc
"hello
~~~
# EXPECTED
UNCLOSED STRING - parse_unclosed_string.md:1:1:1:7
# PROBLEMS
                                                             ┌─────────────────┐
┌─ This string is missing a closing quote. ──────────────────┤ UNCLOSED STRING │
│                                                            └────────────────┬┘
│                                                                             │
│  "hello                                                                     │
│  ‾‾‾‾‾‾                                                                     │
└─────────────────────────────────────────────────────────────────────────────┘
    parse_unclosed_string.md:1:1

# TOKENS
~~~zig
StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-string
	(e-string-part (raw "hello")))
~~~
# FORMATTED
~~~roc
"hello"
~~~
# CANONICALIZE
~~~clojure
(e-string
	(e-literal (string "hello")))
~~~
# TYPES
~~~clojure
(expr (type "Str"))
~~~
