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
PARSE ERROR - error_malformed_syntax.md:1:18:1:19
PARSE ERROR - error_malformed_syntax.md:1:20:1:22
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_expr_record_field_name`
This is an unexpected parsing error. Please check your syntax.

**error_malformed_syntax.md:1:18:1:19:**
```roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
```
                 ^


**PARSE ERROR**
A parsing error occurred: `expected_expr_close_curly_or_comma`
This is an unexpected parsing error. Please check your syntax.

**error_malformed_syntax.md:1:20:1:22:**
```roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
```
                   ^^


# TOKENS
~~~zig
OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,OpColon,Int,Comma,Comma,LowerIdent,OpColon,Comma,LowerIdent,UpperIdent,NoSpaceDotLowerIdent,Comma,StringStart,StringPart,StringEnd,OpColon,LowerIdent,Comma,Int,OpColon,StringStart,StringPart,StringEnd,Comma,OpColon,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-malformed (reason "expected_expr_close_curly_or_comma"))
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
