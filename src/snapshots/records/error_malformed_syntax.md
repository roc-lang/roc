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

Here is the problematic code:
**error_malformed_syntax.md:1:18:1:19:**
```roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
```
                 ^


**PARSE ERROR**
A parsing error occurred: `expected_expr_close_curly_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**error_malformed_syntax.md:1:20:1:22:**
```roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
```
                   ^^


# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:3-1:7),OpColon(1:7-1:8),StringStart(1:9-1:10),StringPart(1:10-1:15),StringEnd(1:15-1:16),Comma(1:16-1:17),OpColon(1:18-1:19),Int(1:20-1:22),Comma(1:22-1:23),Comma(1:24-1:25),LowerIdent(1:26-1:31),OpColon(1:31-1:32),Comma(1:33-1:34),LowerIdent(1:35-1:41),UpperIdent(1:42-1:46),NoSpaceDotLowerIdent(1:46-1:51),Comma(1:51-1:52),StringStart(1:53-1:54),StringPart(1:54-1:61),StringEnd(1:61-1:62),OpColon(1:62-1:63),LowerIdent(1:64-1:69),Comma(1:69-1:70),Int(1:71-1:73),OpColon(1:73-1:74),StringStart(1:75-1:76),StringPart(1:76-1:86),StringEnd(1:86-1:87),Comma(1:87-1:88),OpColon(1:89-1:90),CloseCurly(1:91-1:92),EndOfFile(1:92-1:92),
~~~
# PARSE
~~~clojure
(e-malformed @1.20-1.22 (reason "expected_expr_close_curly_or_comma"))
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
