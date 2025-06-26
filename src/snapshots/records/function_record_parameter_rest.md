# META
~~~ini
description=Function with record parameter and rest pattern
type=expr
~~~
# SOURCE
~~~roc
|{ first_name, ..rest }| "Hello ${first_name} ${rest.last_name}"
~~~
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_colon_after_pat_field_name`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**function_record_parameter_rest.md:1:4:1:15:**
```roc
|{ first_name, ..rest }| "Hello ${first_name} ${rest.last_name}"
```


**UNEXPECTED TOKEN IN PATTERN**
The token **{ first_name** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**function_record_parameter_rest.md:1:2:1:14:**
```roc
|{ first_name, ..rest }| "Hello ${first_name} ${rest.last_name}"
```


**PARSE ERROR**
A parsing error occurred: `expected_expr_bar`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**function_record_parameter_rest.md:1:26:1:33:**
```roc
|{ first_name, ..rest }| "Hello ${first_name} ${rest.last_name}"
```


# TOKENS
~~~zig
OpBar(1:1-1:2),OpenCurly(1:2-1:3),LowerIdent(1:4-1:14),Comma(1:14-1:15),DoubleDot(1:16-1:18),LowerIdent(1:18-1:22),CloseCurly(1:23-1:24),OpBar(1:24-1:25),StringStart(1:26-1:27),StringPart(1:27-1:33),OpenStringInterpolation(1:33-1:35),LowerIdent(1:35-1:45),CloseStringInterpolation(1:45-1:46),StringPart(1:46-1:47),OpenStringInterpolation(1:47-1:49),LowerIdent(1:49-1:53),NoSpaceDotLowerIdent(1:53-1:63),CloseStringInterpolation(1:63-1:64),StringPart(1:64-1:64),StringEnd(1:64-1:65),EndOfFile(1:65-1:65),
~~~
# PARSE
~~~clojure
(e-malformed @1-26-1-33 (reason "expected_expr_bar"))
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