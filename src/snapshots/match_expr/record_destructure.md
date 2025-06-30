# META
~~~ini
description=Match expression with record destructuring patterns
type=expr
~~~
# SOURCE
~~~roc
match person {
    { name, age } => name
    { name, address: { city } } => city
    {} => "empty"
}
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN PATTERN**
The token **match person {
    { name, age } => name
    { name, address: { city } } => city
    {} => "empty"
}** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**record_destructure.md:1:1:5:2:**
```roc
match person {
    { name, age } => name
    { name, address: { city } } => city
    {} => "empty"
}
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_destructure.md:5:1:5:2:**
```roc
}
```
^


**PARSE ERROR**
A parsing error occurred: `expected_close_curly_at_end_of_match`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**record_destructure.md:5:2:5:2:**
```roc
}
```
 


# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:13),OpenCurly(1:14-1:15),Newline(1:1-1:1),
OpenCurly(2:5-2:6),LowerIdent(2:7-2:11),Comma(2:11-2:12),LowerIdent(2:13-2:16),CloseCurly(2:17-2:18),OpFatArrow(2:19-2:21),LowerIdent(2:22-2:26),Newline(1:1-1:1),
OpenCurly(3:5-3:6),LowerIdent(3:7-3:11),Comma(3:11-3:12),LowerIdent(3:13-3:20),OpColon(3:20-3:21),OpenCurly(3:22-3:23),LowerIdent(3:24-3:28),CloseCurly(3:29-3:30),CloseCurly(3:31-3:32),OpFatArrow(3:33-3:35),LowerIdent(3:36-3:40),Newline(1:1-1:1),
OpenCurly(4:5-4:6),CloseCurly(4:6-4:7),OpFatArrow(4:8-4:10),StringStart(4:11-4:12),StringPart(4:12-4:17),StringEnd(4:17-4:18),Newline(1:1-1:1),
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
