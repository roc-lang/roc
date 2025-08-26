# META
~~~ini
description=Match expression with guard conditions using if clauses
type=expr
~~~
# SOURCE
~~~roc
match value {
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
    _ => "other"
}
~~~
# EXPECTED
PARSE ERROR - guards_2.md:2:25:2:25
UNEXPECTED TOKEN IN EXPRESSION - guards_2.md:2:47:2:49
IF WITHOUT ELSE - guards_2.md:2:25:2:27
UNEXPECTED TOKEN IN PATTERN - guards_2.md:2:51:2:75
PARSE ERROR - guards_2.md:2:75:2:75
UNEXPECTED TOKEN IN EXPRESSION - guards_2.md:2:75:2:77
UNEXPECTED TOKEN IN PATTERN - guards_2.md:2:77:2:80
PARSE ERROR - guards_2.md:2:92:2:92
UNEXPECTED TOKEN IN EXPRESSION - guards_2.md:2:92:2:93
UNEXPECTED TOKEN IN PATTERN - guards_2.md:2:93:2:93
PARSE ERROR - guards_2.md:2:93:2:93
UNEXPECTED TOKEN IN EXPRESSION - guards_2.md:2:93:2:94
PARSE ERROR - guards_2.md:3:12:3:12
UNEXPECTED TOKEN IN EXPRESSION - guards_2.md:3:22:3:24
IF WITHOUT ELSE - guards_2.md:3:12:3:14
UNEXPECTED TOKEN IN PATTERN - guards_2.md:3:26:3:48
PARSE ERROR - guards_2.md:3:48:3:48
UNEXPECTED TOKEN IN EXPRESSION - guards_2.md:3:48:3:50
UNEXPECTED TOKEN IN PATTERN - guards_2.md:3:50:3:53
PARSE ERROR - guards_2.md:3:61:3:61
UNEXPECTED TOKEN IN EXPRESSION - guards_2.md:3:61:3:62
UNEXPECTED TOKEN IN PATTERN - guards_2.md:3:62:3:62
PARSE ERROR - guards_2.md:3:62:3:62
UNEXPECTED TOKEN IN EXPRESSION - guards_2.md:3:62:3:63
UNDEFINED VARIABLE - guards_2.md:1:7:1:12
UNRECOGNIZED SYNTAX - guards_2.md:2:25:2:51
UNUSED VARIABLE - guards_2.md:2:6:2:11
UNUSED VARIABLE - guards_2.md:1:1:1:1
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

**guards_2.md:2:25:2:25:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
                        ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**guards_2.md:2:47:2:49:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
                                              ^^


**IF WITHOUT ELSE**
This `if` is being used as an expression, but it doesn't have an `else`.

When `if` is used as an expression (to evaluate to a value), it must have an `else` branch to specify what value to use when the condition is `False`.

**guards_2.md:2:25:2:27:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
                        ^^


**UNEXPECTED TOKEN IN PATTERN**
The token **long list starting with ** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

**guards_2.md:2:51:2:75:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
                                                  ^^^^^^^^^^^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

**guards_2.md:2:75:2:75:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
                                                                          ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **${** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**guards_2.md:2:75:2:77:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
                                                                          ^^


**UNEXPECTED TOKEN IN PATTERN**
The token **Num** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

**guards_2.md:2:77:2:80:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
                                                                            ^^^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

**guards_2.md:2:92:2:92:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
                                                                                           ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**guards_2.md:2:92:2:93:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
                                                                                           ^


**UNEXPECTED TOKEN IN PATTERN**
The token  is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

**guards_2.md:2:93:2:93:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
                                                                                            ^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

**guards_2.md:2:93:2:93:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
                                                                                            ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **"** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**guards_2.md:2:93:2:94:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
                                                                                            ^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

**guards_2.md:3:12:3:12:**
```roc
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
```
           ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**guards_2.md:3:22:3:24:**
```roc
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
```
                     ^^


**IF WITHOUT ELSE**
This `if` is being used as an expression, but it doesn't have an `else`.

When `if` is used as an expression (to evaluate to a value), it must have an `else` branch to specify what value to use when the condition is `False`.

**guards_2.md:3:12:3:14:**
```roc
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
```
           ^^


**UNEXPECTED TOKEN IN PATTERN**
The token **pair of equal values: ** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

**guards_2.md:3:26:3:48:**
```roc
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
```
                         ^^^^^^^^^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

**guards_2.md:3:48:3:48:**
```roc
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
```
                                               ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **${** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**guards_2.md:3:48:3:50:**
```roc
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
```
                                               ^^


**UNEXPECTED TOKEN IN PATTERN**
The token **Num** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

**guards_2.md:3:50:3:53:**
```roc
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
```
                                                 ^^^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

**guards_2.md:3:61:3:61:**
```roc
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
```
                                                            ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**guards_2.md:3:61:3:62:**
```roc
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
```
                                                            ^


**UNEXPECTED TOKEN IN PATTERN**
The token  is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

**guards_2.md:3:62:3:62:**
```roc
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
```
                                                             ^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

**guards_2.md:3:62:3:62:**
```roc
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
```
                                                             ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **"** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**guards_2.md:3:62:3:63:**
```roc
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
```
                                                             ^


**UNDEFINED VARIABLE**
Nothing is named `value` in this scope.
Is there an `import` or `exposing` missing up-top?

**guards_2.md:1:7:1:12:**
```roc
match value {
```
      ^^^^^


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**guards_2.md:2:25:2:51:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

**UNUSED VARIABLE**
Variable `first` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_first` to suppress this warning.
The unused variable is declared here:
**guards_2.md:2:6:2:11:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
     ^^^^^


**UNUSED VARIABLE**
Variable `rest` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_rest` to suppress this warning.
The unused variable is declared here:
**guards_2.md:1:1:1:1:**
```roc
match value {
```
^


# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:12),OpenCurly(1:13-1:14),
OpenSquare(2:5-2:6),LowerIdent(2:6-2:11),Comma(2:11-2:12),DoubleDot(2:13-2:15),KwAs(2:16-2:18),LowerIdent(2:19-2:23),CloseSquare(2:23-2:24),KwIf(2:25-2:27),UpperIdent(2:28-2:32),NoSpaceDotLowerIdent(2:32-2:36),NoSpaceOpenRound(2:36-2:37),LowerIdent(2:37-2:41),CloseRound(2:41-2:42),OpGreaterThan(2:43-2:44),Int(2:45-2:46),OpFatArrow(2:47-2:49),StringStart(2:50-2:51),StringPart(2:51-2:75),OpenStringInterpolation(2:75-2:77),UpperIdent(2:77-2:80),NoSpaceDotLowerIdent(2:80-2:86),LowerIdent(2:87-2:92),CloseStringInterpolation(2:92-2:93),StringPart(2:93-2:93),StringEnd(2:93-2:94),
OpenSquare(3:5-3:6),LowerIdent(3:6-3:7),Comma(3:7-3:8),LowerIdent(3:9-3:10),CloseSquare(3:10-3:11),KwIf(3:12-3:14),LowerIdent(3:15-3:16),OpEquals(3:17-3:19),LowerIdent(3:20-3:21),OpFatArrow(3:22-3:24),StringStart(3:25-3:26),StringPart(3:26-3:48),OpenStringInterpolation(3:48-3:50),UpperIdent(3:50-3:53),NoSpaceDotLowerIdent(3:53-3:59),LowerIdent(3:60-3:61),CloseStringInterpolation(3:61-3:62),StringPart(3:62-3:62),StringEnd(3:62-3:63),
Underscore(4:5-4:6),OpFatArrow(4:7-4:9),StringStart(4:10-4:11),StringPart(4:11-4:16),StringEnd(4:16-4:17),
CloseCurly(5:1-5:2),EndOfFile(5:2-5:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.12 (raw "value"))
	(branches
		(branch @2.5-2.51
			(p-list @2.5-2.24
				(p-ident @2.6-2.11 (raw "first"))
				(p-list-rest @2.13-2.23 (name "rest")))
			(e-malformed @2.25-2.51 (reason "no_else")))
		(branch @2.51-2.77
			(p-malformed @2.51-2.75 (tag "pattern_unexpected_token"))
			(e-malformed @2.75-2.77 (reason "expr_unexpected_token")))
		(branch @2.77-2.93
			(p-malformed @2.77-2.92 (tag "pattern_unexpected_token"))
			(e-malformed @2.92-2.93 (reason "expr_unexpected_token")))
		(branch @2.93-2.94
			(p-malformed @2.93-2.93 (tag "pattern_unexpected_token"))
			(e-malformed @2.93-2.94 (reason "expr_unexpected_token")))
		(branch @3.5-3.26
			(p-list @3.5-3.11
				(p-ident @3.6-3.7 (raw "x"))
				(p-ident @3.9-3.10 (raw "y")))
			(e-malformed @3.12-3.26 (reason "no_else")))
		(branch @3.26-3.50
			(p-malformed @3.26-3.48 (tag "pattern_unexpected_token"))
			(e-malformed @3.48-3.50 (reason "expr_unexpected_token")))
		(branch @3.50-3.62
			(p-malformed @3.50-3.61 (tag "pattern_unexpected_token"))
			(e-malformed @3.61-3.62 (reason "expr_unexpected_token")))
		(branch @3.62-3.63
			(p-malformed @3.62-3.62 (tag "pattern_unexpected_token"))
			(e-malformed @3.62-3.63 (reason "expr_unexpected_token")))
		(branch @4.5-4.17
			(p-underscore)
			(e-string @4.10-4.17
				(e-string-part @4.11-4.16 (raw "other"))))))
~~~
# FORMATTED
~~~roc
match value {
	[first, .. as rest] => 
	 => 
	 => 
	 => 
	[x, y] => 
	 => 
	 => 
	 => 
	_ => "other"
}
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "expr_not_canonicalized"))
~~~
# TYPES
~~~clojure
(expr @2.25-2.51 (type "Error"))
~~~
