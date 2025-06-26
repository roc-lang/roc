# META
~~~ini
description=Function with record parameter destructuring and string interpolation
type=expr
~~~
# SOURCE
~~~roc
|{ name, age }| "Hello ${name}, you are ${age.to_str()} years old"
~~~
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_colon_after_pat_field_name`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**function_record_parameter.md:1:4:1:9:**
```roc
|{ name, age }| "Hello ${name}, you are ${age.to_str()} years old"
```


**UNEXPECTED TOKEN IN PATTERN**
The token **{ name** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**function_record_parameter.md:1:2:1:8:**
```roc
|{ name, age }| "Hello ${name}, you are ${age.to_str()} years old"
```


**PARSE ERROR**
A parsing error occurred: `expected_expr_bar`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**function_record_parameter.md:1:17:1:24:**
```roc
|{ name, age }| "Hello ${name}, you are ${age.to_str()} years old"
```


# TOKENS
~~~zig
OpBar(1:1-1:2),OpenCurly(1:2-1:3),LowerIdent(1:4-1:8),Comma(1:8-1:9),LowerIdent(1:10-1:13),CloseCurly(1:14-1:15),OpBar(1:15-1:16),StringStart(1:17-1:18),StringPart(1:18-1:24),OpenStringInterpolation(1:24-1:26),LowerIdent(1:26-1:30),CloseStringInterpolation(1:30-1:31),StringPart(1:31-1:41),OpenStringInterpolation(1:41-1:43),LowerIdent(1:43-1:46),NoSpaceDotLowerIdent(1:46-1:53),NoSpaceOpenRound(1:53-1:54),CloseRound(1:54-1:55),CloseStringInterpolation(1:55-1:56),StringPart(1:56-1:66),StringEnd(1:66-1:67),EndOfFile(1:67-1:67),
~~~
# PARSE
~~~clojure
(e-malformed @1-17-1-24 (reason "expected_expr_bar"))
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