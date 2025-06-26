# META
~~~ini
description=Function with record parameter destructuring
type=expr
~~~
# SOURCE
~~~roc
|{ name, age }| "Hello $(name), you are $(Num.toStr age) years old"
~~~
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_colon_after_pat_field_name`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**function_record_parameter.md:1:4:1:9:**
```roc
|{ name, age }| "Hello $(name), you are $(Num.toStr age) years old"
```


**UNEXPECTED TOKEN IN PATTERN**
The token **{ name** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**function_record_parameter.md:1:2:1:8:**
```roc
|{ name, age }| "Hello $(name), you are $(Num.toStr age) years old"
```


**PARSE ERROR**
A parsing error occurred: `expected_expr_bar`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**function_record_parameter.md:1:17:1:67:**
```roc
|{ name, age }| "Hello $(name), you are $(Num.toStr age) years old"
```


# TOKENS
~~~zig
OpBar(1:1-1:2),OpenCurly(1:2-1:3),LowerIdent(1:4-1:8),Comma(1:8-1:9),LowerIdent(1:10-1:13),CloseCurly(1:14-1:15),OpBar(1:15-1:16),StringStart(1:17-1:18),StringPart(1:18-1:67),StringEnd(1:67-1:68),EndOfFile(1:68-1:68),
~~~
# PARSE
~~~clojure
(e-malformed @1-17-1-67 (reason "expected_expr_bar"))
~~~
# FORMATTED
~~~roc

~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~