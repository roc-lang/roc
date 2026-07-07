# META
~~~ini
description=Bad record accessor syntax reports a targeted parse error
type=expr
~~~
# SOURCE
~~~roc
person.@
~~~
# EXPECTED
EXPECTED RECORD ACCESSOR - error_bad_record_accessor.md:1:7:1:8
# PROBLEMS

┌──────────────────────────┐
│ EXPECTED RECORD ACCESSOR ├─ I was parsing access after `.`, and I ──────────┐
└┬─────────────────────────┘  expected a field name or tuple index.           │
 │                                                                            │
 │  person.@                                                                  │
 │        ‾                                                                   │
 └────────────────────────────────────────── error_bad_record_accessor.md:1:7 ┘

    Record access uses a lowercase field name like `.name`. Tuple access uses a
    number like `.0`. Uppercase names, malformed names, and a bare `.` are not
    valid accessors.

    For example:
        person.name
        pair.0

    I found `.` here.

# TOKENS
~~~zig
LowerIdent,Dot,MalformedOpaqueNameWithoutName,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-malformed (reason "expr_dot_suffix_not_allowed"))
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
