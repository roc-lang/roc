# META
~~~ini
description=Match expression with guard conditions using if clauses
type=expr
~~~
# SOURCE
~~~roc
match value {
    x if x > 0 => "positive: ${Num.toStr x}"
    x if x < 0 => "negative: ${Num.toStr x}"
    0 => "zero"
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
    _ => "other"
}
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> "** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**guards.md:2:16:2:20:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
               ^^^^


**PARSE ERROR**
A parsing error occurred: `no_else`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**guards.md:2:19:2:30:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
                  ^^^^^^^^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **positive: ${** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**guards.md:2:20:2:32:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
                   ^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **${Num** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**guards.md:2:30:2:35:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
                             ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.toStr x** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**guards.md:2:35:2:43:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
                                  ^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**guards.md:2:43:2:44:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
                                          ^


**UNEXPECTED TOKEN IN PATTERN**
The token **"** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**guards.md:2:44:2:45:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
                                           ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**guards.md:2:44:2:44:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
                                           


**UNEXPECTED TOKEN IN PATTERN**
The token **match value {
    x if x > 0 => "positive: ${Num.toStr x}"
    x** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**guards.md:1:1:3:6:**
```roc
match value {
    x if x > 0 => "positive: ${Num.toStr x}"
    x if x < 0 => "negative: ${Num.toStr x}"
```


**UNEXPECTED TOKEN IN PATTERN**
The token **if x** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**guards.md:3:7:3:11:**
```roc
    x if x < 0 => "negative: ${Num.toStr x}"
```
      ^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **=> "** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**guards.md:3:16:3:20:**
```roc
    x if x < 0 => "negative: ${Num.toStr x}"
```
               ^^^^


**PARSE ERROR**
A parsing error occurred: `string_expected_close_interpolation`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**guards.md:3:19:3:30:**
```roc
    x if x < 0 => "negative: ${Num.toStr x}"
```
                  ^^^^^^^^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **}** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**guards.md:3:43:3:44:**
```roc
    x if x < 0 => "negative: ${Num.toStr x}"
```
                                          ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **"** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**guards.md:3:44:3:45:**
```roc
    x if x < 0 => "negative: ${Num.toStr x}"
```
                                           ^


**UNEXPECTED TOKEN IN PATTERN**
The token  is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**guards.md:3:44:3:44:**
```roc
    x if x < 0 => "negative: ${Num.toStr x}"
```
                                           


**UNEXPECTED TOKEN IN EXPRESSION**
The token **match value {
    x if x > 0 => "positive: ${Num.toStr x}"
    x if x < 0 => "negative: ${Num.toStr x}"
    0** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**guards.md:1:1:4:6:**
```roc
match value {
    x if x > 0 => "positive: ${Num.toStr x}"
    x if x < 0 => "negative: ${Num.toStr x}"
    0 => "zero"
```


**UNEXPECTED TOKEN IN PATTERN**
The token **match value {
    x if x > 0 => "positive: ${Num.toStr x}"
    x if x < 0 => "negative: ${Num.toStr x}"
    0 => "zero"
    [** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**guards.md:1:1:5:6:**
```roc
match value {
    x if x > 0 => "positive: ${Num.toStr x}"
    x if x < 0 => "negative: ${Num.toStr x}"
    0 => "zero"
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.. as** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**guards.md:5:13:5:18:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
            ^^^^^


**LIST NOT CLOSED**
This list is missing a closing bracket or has a syntax error.
Lists must be closed with **]** and list items must be separated by commas.
For example:     [1, 2, 3]

Here is the problematic code:
**guards.md:5:23:5:27:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
                      ^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **if List** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**guards.md:5:25:5:32:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
                        ^^^^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **=> "** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**guards.md:5:47:5:51:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
                                              ^^^^


**PARSE ERROR**
A parsing error occurred: `string_expected_close_interpolation`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**guards.md:5:50:5:75:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
                                                 ^^^^^^^^^^^^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **}** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**guards.md:5:92:5:93:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
                                                                                           ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **"** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**guards.md:5:93:5:94:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
                                                                                            ^


**UNEXPECTED TOKEN IN PATTERN**
The token  is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**guards.md:5:93:5:93:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
                                                                                            


**UNEXPECTED TOKEN IN EXPRESSION**
The token **match value {
    x if x > 0 => "positive: ${Num.toStr x}"
    x if x < 0 => "negative: ${Num.toStr x}"
    0 => "zero"
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
    [** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**guards.md:1:1:6:6:**
```roc
match value {
    x if x > 0 => "positive: ${Num.toStr x}"
    x if x < 0 => "negative: ${Num.toStr x}"
    0 => "zero"
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> "** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**guards.md:6:22:6:26:**
```roc
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
```
                     ^^^^


**PARSE ERROR**
A parsing error occurred: `no_else`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**guards.md:6:25:6:48:**
```roc
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
```
                        ^^^^^^^^^^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **pair of equal values: ${** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**guards.md:6:26:6:50:**
```roc
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
```
                         ^^^^^^^^^^^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **${Num** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**guards.md:6:48:6:53:**
```roc
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
```
                                               ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.toStr x** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**guards.md:6:53:6:61:**
```roc
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
```
                                                    ^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**guards.md:6:61:6:62:**
```roc
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
```
                                                            ^


**UNEXPECTED TOKEN IN PATTERN**
The token **"** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**guards.md:6:62:6:63:**
```roc
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
```
                                                             ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**guards.md:6:62:6:62:**
```roc
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
```
                                                             


**UNEXPECTED TOKEN IN PATTERN**
The token **match value {
    x if x > 0 => "positive: ${Num.toStr x}"
    x if x < 0 => "negative: ${Num.toStr x}"
    0 => "zero"
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
    _** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**guards.md:1:1:7:6:**
```roc
match value {
    x if x > 0 => "positive: ${Num.toStr x}"
    x if x < 0 => "negative: ${Num.toStr x}"
    0 => "zero"
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
    _ => "other"
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **_ =>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**guards.md:7:5:7:9:**
```roc
    _ => "other"
```
    ^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **=> "** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**guards.md:7:7:7:11:**
```roc
    _ => "other"
```
      ^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **match value {
    x if x > 0 => "positive: ${Num.toStr x}"
    x if x < 0 => "negative: ${Num.toStr x}"
    0 => "zero"
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
    _ => "other"
}** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**guards.md:1:1:8:2:**
```roc
match value {
    x if x > 0 => "positive: ${Num.toStr x}"
    x if x < 0 => "negative: ${Num.toStr x}"
    0 => "zero"
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
    _ => "other"
}
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**guards.md:8:1:8:2:**
```roc
}
```
^


**PARSE ERROR**
A parsing error occurred: `expected_close_curly_at_end_of_match`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**guards.md:8:2:8:2:**
```roc
}
```
 


# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:12),OpenCurly(1:13-1:14),Newline(1:1-1:1),
LowerIdent(2:5-2:6),KwIf(2:7-2:9),LowerIdent(2:10-2:11),OpGreaterThan(2:12-2:13),Int(2:14-2:15),OpFatArrow(2:16-2:18),StringStart(2:19-2:20),StringPart(2:20-2:30),OpenStringInterpolation(2:30-2:32),UpperIdent(2:32-2:35),NoSpaceDotLowerIdent(2:35-2:41),LowerIdent(2:42-2:43),CloseStringInterpolation(2:43-2:44),StringPart(2:44-2:44),StringEnd(2:44-2:45),Newline(1:1-1:1),
LowerIdent(3:5-3:6),KwIf(3:7-3:9),LowerIdent(3:10-3:11),OpLessThan(3:12-3:13),Int(3:14-3:15),OpFatArrow(3:16-3:18),StringStart(3:19-3:20),StringPart(3:20-3:30),OpenStringInterpolation(3:30-3:32),UpperIdent(3:32-3:35),NoSpaceDotLowerIdent(3:35-3:41),LowerIdent(3:42-3:43),CloseStringInterpolation(3:43-3:44),StringPart(3:44-3:44),StringEnd(3:44-3:45),Newline(1:1-1:1),
Int(4:5-4:6),OpFatArrow(4:7-4:9),StringStart(4:10-4:11),StringPart(4:11-4:15),StringEnd(4:15-4:16),Newline(1:1-1:1),
OpenSquare(5:5-5:6),LowerIdent(5:6-5:11),Comma(5:11-5:12),DoubleDot(5:13-5:15),KwAs(5:16-5:18),LowerIdent(5:19-5:23),CloseSquare(5:23-5:24),KwIf(5:25-5:27),UpperIdent(5:28-5:32),NoSpaceDotLowerIdent(5:32-5:36),NoSpaceOpenRound(5:36-5:37),LowerIdent(5:37-5:41),CloseRound(5:41-5:42),OpGreaterThan(5:43-5:44),Int(5:45-5:46),OpFatArrow(5:47-5:49),StringStart(5:50-5:51),StringPart(5:51-5:75),OpenStringInterpolation(5:75-5:77),UpperIdent(5:77-5:80),NoSpaceDotLowerIdent(5:80-5:86),LowerIdent(5:87-5:92),CloseStringInterpolation(5:92-5:93),StringPart(5:93-5:93),StringEnd(5:93-5:94),Newline(1:1-1:1),
OpenSquare(6:5-6:6),LowerIdent(6:6-6:7),Comma(6:7-6:8),LowerIdent(6:9-6:10),CloseSquare(6:10-6:11),KwIf(6:12-6:14),LowerIdent(6:15-6:16),OpEquals(6:17-6:19),LowerIdent(6:20-6:21),OpFatArrow(6:22-6:24),StringStart(6:25-6:26),StringPart(6:26-6:48),OpenStringInterpolation(6:48-6:50),UpperIdent(6:50-6:53),NoSpaceDotLowerIdent(6:53-6:59),LowerIdent(6:60-6:61),CloseStringInterpolation(6:61-6:62),StringPart(6:62-6:62),StringEnd(6:62-6:63),Newline(1:1-1:1),
Underscore(7:5-7:6),OpFatArrow(7:7-7:9),StringStart(7:10-7:11),StringPart(7:11-7:16),StringEnd(7:16-7:17),Newline(1:1-1:1),
CloseCurly(8:1-8:2),EndOfFile(8:2-8:2),
~~~
# PARSE
~~~clojure
(e-malformed @8.2-8.2 (reason "expected_close_curly_at_end_of_match"))
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
