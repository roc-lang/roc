# META
~~~ini
description=Match expression with boolean-like tag patterns
type=expr
~~~
# SOURCE
~~~roc
match isReady {
    True => "ready to go!"
    False => "not ready yet"
}
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN PATTERN**
The token **match isReady {
    True => "ready to go!"
    False** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**boolean_patterns.md:1:1:3:10:**
```roc
match isReady {
    True => "ready to go!"
    False => "not ready yet"
```


**UNEXPECTED TOKEN IN PATTERN**
The token **=> "** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**boolean_patterns.md:3:11:3:15:**
```roc
    False => "not ready yet"
```
          ^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **match isReady {
    True => "ready to go!"
    False => "not ready yet"
}** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**boolean_patterns.md:1:1:4:2:**
```roc
match isReady {
    True => "ready to go!"
    False => "not ready yet"
}
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**boolean_patterns.md:4:1:4:2:**
```roc
}
```
^


**PARSE ERROR**
A parsing error occurred: `expected_close_curly_at_end_of_match`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**boolean_patterns.md:4:2:4:2:**
```roc
}
```
 


# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:14),OpenCurly(1:15-1:16),Newline(1:1-1:1),
UpperIdent(2:5-2:9),OpFatArrow(2:10-2:12),StringStart(2:13-2:14),StringPart(2:14-2:26),StringEnd(2:26-2:27),Newline(1:1-1:1),
UpperIdent(3:5-3:10),OpFatArrow(3:11-3:13),StringStart(3:14-3:15),StringPart(3:15-3:28),StringEnd(3:28-3:29),Newline(1:1-1:1),
CloseCurly(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-malformed @4.2-4.2 (reason "expected_close_curly_at_end_of_match"))
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
