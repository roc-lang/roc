# META
~~~ini
description=Match expression with no branches should produce error
type=expr
~~~
# SOURCE
~~~roc
match 42 {}
~~~
# EXPECTED
EMPTY MATCH - empty_match.md:1:1:1:6
# PROBLEMS

┌─────────────┐
│ EMPTY MATCH ├─ I was parsing a match expression, but it has no branches. ───┐
└┬────────────┘                                                               │
 │                                                                            │
 │  match 42 {}                                                               │
 │  ‾‾‾‾‾                                                                     │
 └──────────────────────────────────────────────────────── empty_match.md:1:1 ┘

    A match expression needs at least one branch inside the braces.

    For example:
        match result {
            Ok(value) => value
        }

    I found `match` here.
    That word is reserved by Roc, so it cannot be used as a name in this
    position.

# TOKENS
~~~zig
KwMatch,Int,OpenCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-malformed (reason "match_has_no_branches"))
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
