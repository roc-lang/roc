# META
~~~ini
description=Malformed record syntax (error case)
type=expr
~~~
# SOURCE
~~~roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
~~~
# EXPECTED
EXPECTED RECORD FIELD - error_malformed_syntax.md:1:18:1:19
# PROBLEMS

┌───────────────────────┐
│ EXPECTED RECORD FIELD ├─ I was parsing a record expression, and I ──────────┐
└┬──────────────────────┘  expected a lowercase field name.                   │
 │                                                                            │
 │  …Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number…│
 │           ‾                                                                │
 └──────────────────────────────────────────── error_malformed_syntax.md:1:18 ┘

    Record fields start with lowercase names. After the name, either write `:
    value` or omit the value to use field punning.

    For example:
        { name: "Ada", age }

    I found `:` here.

# TOKENS
~~~zig
OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,OpColon,Int,Comma,Comma,LowerIdent,OpColon,Comma,LowerIdent,UpperIdent,NoSpaceDotLowerIdent,Comma,StringStart,StringPart,StringEnd,OpColon,LowerIdent,Comma,Int,OpColon,StringStart,StringPart,StringEnd,Comma,OpColon,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-malformed (reason "expected_expr_record_field_name"))
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
