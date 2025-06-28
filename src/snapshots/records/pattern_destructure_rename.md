# META
~~~ini
description=Record destructuring with field renaming
type=expr
~~~
# SOURCE
~~~roc
match person {
    { name: userName, age: userAge } => "User ${userName} is ${userAge.to_str()} years old"
}
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN PATTERN**
The token **match person {
    { name: userName, age: userAge } => "User ${userName} is ${userAge.to_str()} years old"
}** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**pattern_destructure_rename.md:1:1:3:2:**
```roc
match person {
    { name: userName, age: userAge } => "User ${userName} is ${userAge.to_str()} years old"
}
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**pattern_destructure_rename.md:3:1:3:2:**
```roc
}
```


**PARSE ERROR**
A parsing error occurred: `expected_close_curly_at_end_of_match`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**pattern_destructure_rename.md:3:2:3:2:**
```roc
}
```


# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:13),OpenCurly(1:14-1:15),Newline(1:1-1:1),
OpenCurly(2:5-2:6),LowerIdent(2:7-2:11),OpColon(2:11-2:12),LowerIdent(2:13-2:21),Comma(2:21-2:22),LowerIdent(2:23-2:26),OpColon(2:26-2:27),LowerIdent(2:28-2:35),CloseCurly(2:36-2:37),OpFatArrow(2:38-2:40),StringStart(2:41-2:42),StringPart(2:42-2:47),OpenStringInterpolation(2:47-2:49),LowerIdent(2:49-2:57),CloseStringInterpolation(2:57-2:58),StringPart(2:58-2:62),OpenStringInterpolation(2:62-2:64),LowerIdent(2:64-2:71),NoSpaceDotLowerIdent(2:71-2:78),NoSpaceOpenRound(2:78-2:79),CloseRound(2:79-2:80),CloseStringInterpolation(2:80-2:81),StringPart(2:81-2:91),StringEnd(2:91-2:92),Newline(1:1-1:1),
CloseCurly(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-malformed @3-2-3-2 (reason "expected_close_curly_at_end_of_match"))
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
