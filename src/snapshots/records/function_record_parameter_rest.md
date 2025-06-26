# META
~~~ini
description=Function with record parameter and rest pattern
type=expr
~~~
# SOURCE
~~~roc
|{ name, age, ..rest }| { greeting: "Hello $(name)", fullRecord: person, isAdult: age >= 18 }
~~~
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_colon_after_pat_field_name`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**function_record_parameter_rest.md:1:4:1:9:**
```roc
|{ name, age, ..rest }| { greeting: "Hello $(name)", fullRecord: person, isAdult: age >= 18 }
```


**UNEXPECTED TOKEN IN PATTERN**
The token **{ name** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**function_record_parameter_rest.md:1:2:1:8:**
```roc
|{ name, age, ..rest }| { greeting: "Hello $(name)", fullRecord: person, isAdult: age >= 18 }
```


**PARSE ERROR**
A parsing error occurred: `expected_expr_bar`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**function_record_parameter_rest.md:1:25:1:35:**
```roc
|{ name, age, ..rest }| { greeting: "Hello $(name)", fullRecord: person, isAdult: age >= 18 }
```


# TOKENS
~~~zig
OpBar(1:1-1:2),OpenCurly(1:2-1:3),LowerIdent(1:4-1:8),Comma(1:8-1:9),LowerIdent(1:10-1:13),Comma(1:13-1:14),DoubleDot(1:15-1:17),LowerIdent(1:17-1:21),CloseCurly(1:22-1:23),OpBar(1:23-1:24),OpenCurly(1:25-1:26),LowerIdent(1:27-1:35),OpColon(1:35-1:36),StringStart(1:37-1:38),StringPart(1:38-1:51),StringEnd(1:51-1:52),Comma(1:52-1:53),LowerIdent(1:54-1:64),OpColon(1:64-1:65),LowerIdent(1:66-1:72),Comma(1:72-1:73),LowerIdent(1:74-1:81),OpColon(1:81-1:82),LowerIdent(1:83-1:86),OpGreaterThanOrEq(1:87-1:89),Int(1:90-1:92),CloseCurly(1:93-1:94),EndOfFile(1:94-1:94),
~~~
# PARSE
~~~clojure
(e-malformed @1-25-1-35 (reason "expected_expr_bar"))
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