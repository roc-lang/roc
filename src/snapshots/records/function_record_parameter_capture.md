# META
~~~ini
description=Function with record parameter destructuring and rest pattern, capture whole record using as
type=expr
~~~
# SOURCE
~~~roc
|{ name, age, .. } as person| { greeting: "Hello $(name)", fullRecord: person, isAdult: age >= 18 }
~~~
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_colon_after_pat_field_name`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**function_record_parameter_capture.md:1:4:1:9:**
```roc
|{ name, age, .. } as person| { greeting: "Hello $(name)", fullRecord: person, isAdult: age >= 18 }
```


**UNEXPECTED TOKEN IN PATTERN**
The token **{ name** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**function_record_parameter_capture.md:1:2:1:8:**
```roc
|{ name, age, .. } as person| { greeting: "Hello $(name)", fullRecord: person, isAdult: age >= 18 }
```


**PARSE ERROR**
A parsing error occurred: `expected_expr_bar`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**function_record_parameter_capture.md:1:23:1:30:**
```roc
|{ name, age, .. } as person| { greeting: "Hello $(name)", fullRecord: person, isAdult: age >= 18 }
```


# TOKENS
~~~zig
OpBar(1:1-1:2),OpenCurly(1:2-1:3),LowerIdent(1:4-1:8),Comma(1:8-1:9),LowerIdent(1:10-1:13),Comma(1:13-1:14),DoubleDot(1:15-1:17),CloseCurly(1:18-1:19),KwAs(1:20-1:22),LowerIdent(1:23-1:29),OpBar(1:29-1:30),OpenCurly(1:31-1:32),LowerIdent(1:33-1:41),OpColon(1:41-1:42),StringStart(1:43-1:44),StringPart(1:44-1:57),StringEnd(1:57-1:58),Comma(1:58-1:59),LowerIdent(1:60-1:70),OpColon(1:70-1:71),LowerIdent(1:72-1:78),Comma(1:78-1:79),LowerIdent(1:80-1:87),OpColon(1:87-1:88),LowerIdent(1:89-1:92),OpGreaterThanOrEq(1:93-1:95),Int(1:96-1:98),CloseCurly(1:99-1:100),EndOfFile(1:100-1:100),
~~~
# PARSE
~~~clojure
(e-malformed @1-23-1-30 (reason "expected_expr_bar"))
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