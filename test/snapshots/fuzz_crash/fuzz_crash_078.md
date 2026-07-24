# META
~~~ini
description=fuzz crash
type=snippet
~~~
# SOURCE
~~~roc
import#\
~~~
# EXPECTED
INCOMPLETE IMPORT - fuzz_crash_078.md:2:1:2:1
# PROBLEMS

┌───────────────────┐
│ INCOMPLETE IMPORT ├─ I was parsing an import, and the mod path is ───────┐
└┬──────────────────┘  incomplete.                                            │
 │                                                                            │
 │                                                                            │
 │  ‾                                                                         │
 └───────────────────────────────────────────────────── fuzz_crash_078.md:2:1 ┘

    Imports must name a mod, optionally with a qualifier and exposing list.

    For example:
        import Json.Decode exposing [decode]

    I reached the end of the file before this construct was complete.

# TOKENS
~~~zig
KwImport,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-malformed (tag "incomplete_import"))))
~~~
# FORMATTED
~~~roc
# \
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
