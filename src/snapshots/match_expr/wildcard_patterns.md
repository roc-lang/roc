# META
~~~ini
description=Match expression with tag patterns and variable catch-all pattern
type=expr
~~~
# SOURCE
~~~roc
match value {
    Answer => "the answer"
    Zero => "zero"
    other => "something else"
}
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN PATTERN**
The token **match value {
    Answer => "the answer"
    Zero** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**wildcard_patterns.md:1:1:3:9:**
```roc
match value {
    Answer => "the answer"
    Zero => "zero"
```


**UNEXPECTED TOKEN IN PATTERN**
The token **=> "** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**wildcard_patterns.md:3:10:3:14:**
```roc
    Zero => "zero"
```
         ^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **match value {
    Answer => "the answer"
    Zero => "zero"
    other** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**wildcard_patterns.md:1:1:4:10:**
```roc
match value {
    Answer => "the answer"
    Zero => "zero"
    other => "something else"
```


**UNEXPECTED TOKEN IN PATTERN**
The token **=> "** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**wildcard_patterns.md:4:11:4:15:**
```roc
    other => "something else"
```
          ^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **match value {
    Answer => "the answer"
    Zero => "zero"
    other => "something else"
}** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**wildcard_patterns.md:1:1:5:2:**
```roc
match value {
    Answer => "the answer"
    Zero => "zero"
    other => "something else"
}
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**wildcard_patterns.md:5:1:5:2:**
```roc
}
```
^


**PARSE ERROR**
A parsing error occurred: `expected_close_curly_at_end_of_match`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**wildcard_patterns.md:5:2:5:2:**
```roc
}
```
 


# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:12),OpenCurly(1:13-1:14),Newline(1:1-1:1),
UpperIdent(2:5-2:11),OpFatArrow(2:12-2:14),StringStart(2:15-2:16),StringPart(2:16-2:26),StringEnd(2:26-2:27),Newline(1:1-1:1),
UpperIdent(3:5-3:9),OpFatArrow(3:10-3:12),StringStart(3:13-3:14),StringPart(3:14-3:18),StringEnd(3:18-3:19),Newline(1:1-1:1),
LowerIdent(4:5-4:10),OpFatArrow(4:11-4:13),StringStart(4:14-4:15),StringPart(4:15-4:29),StringEnd(4:29-4:30),Newline(1:1-1:1),
CloseCurly(5:1-5:2),EndOfFile(5:2-5:2),
~~~
# PARSE
~~~clojure
(e-malformed @5.2-5.2 (reason "expected_close_curly_at_end_of_match"))
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
