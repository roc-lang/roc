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
── ✗ expected record accessor ───────────────── error_bad_record_accessor.md:1:7

I was parsing access after `.`, and I expected a field name or tuple index.

person.@
      ^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found . here.

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
