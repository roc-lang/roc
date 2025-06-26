# META
~~~ini
description=Nested record destructuring pattern in a match expression
type=expr
~~~
# SOURCE
~~~roc
match person {
    { name, address: { street, city, zipCode } } => "${name} lives on ${street} in ${city}"
}
~~~
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_colon_after_pat_field_name`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**pattern_destructure_nested.md:2:7:2:12:**
```roc
    { name, address: { street, city, zipCode } } => "${name} lives on ${street} in ${city}"
```


**UNEXPECTED TOKEN IN PATTERN**
The token **match person {
    { name, address: { street, city, zipCode } } => "${name} lives on ${street} in ${city}"
}** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**pattern_destructure_nested.md:1:1:3:2:**
```roc
match person {
    { name, address: { street, city, zipCode } } => "${name} lives on ${street} in ${city}"
}
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**pattern_destructure_nested.md:3:1:3:2:**
```roc
}
```


**PARSE ERROR**
A parsing error occurred: `expected_close_curly_at_end_of_match`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**pattern_destructure_nested.md:3:2:3:2:**
```roc
}
```


# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:13),OpenCurly(1:14-1:15),Newline(1:1-1:1),
OpenCurly(2:5-2:6),LowerIdent(2:7-2:11),Comma(2:11-2:12),LowerIdent(2:13-2:20),OpColon(2:20-2:21),OpenCurly(2:22-2:23),LowerIdent(2:24-2:30),Comma(2:30-2:31),LowerIdent(2:32-2:36),Comma(2:36-2:37),LowerIdent(2:38-2:45),CloseCurly(2:46-2:47),CloseCurly(2:48-2:49),OpFatArrow(2:50-2:52),StringStart(2:53-2:54),StringPart(2:54-2:54),OpenStringInterpolation(2:54-2:56),LowerIdent(2:56-2:60),CloseStringInterpolation(2:60-2:61),StringPart(2:61-2:71),OpenStringInterpolation(2:71-2:73),LowerIdent(2:73-2:79),CloseStringInterpolation(2:79-2:80),StringPart(2:80-2:84),OpenStringInterpolation(2:84-2:86),LowerIdent(2:86-2:90),CloseStringInterpolation(2:90-2:91),StringPart(2:91-2:91),StringEnd(2:91-2:92),Newline(1:1-1:1),
CloseCurly(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-malformed @3-2-3-2 (reason "expected_close_curly_at_end_of_match"))
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
