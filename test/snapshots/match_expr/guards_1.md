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
    _ => "other"
}
~~~
# EXPECTED
PARSE ERROR - guards_1.md:2:7:2:7
UNEXPECTED TOKEN IN EXPRESSION - guards_1.md:2:16:2:18
IF WITHOUT ELSE - guards_1.md:2:7:2:9
UNEXPECTED TOKEN IN PATTERN - guards_1.md:2:20:2:30
PARSE ERROR - guards_1.md:2:30:2:30
UNEXPECTED TOKEN IN EXPRESSION - guards_1.md:2:30:2:32
UNEXPECTED TOKEN IN PATTERN - guards_1.md:2:32:2:35
PARSE ERROR - guards_1.md:2:43:2:43
UNEXPECTED TOKEN IN EXPRESSION - guards_1.md:2:43:2:44
UNEXPECTED TOKEN IN PATTERN - guards_1.md:2:44:2:44
PARSE ERROR - guards_1.md:2:44:2:44
UNEXPECTED TOKEN IN EXPRESSION - guards_1.md:2:44:2:45
PARSE ERROR - guards_1.md:3:7:3:7
UNEXPECTED TOKEN IN EXPRESSION - guards_1.md:3:16:3:18
IF WITHOUT ELSE - guards_1.md:3:7:3:9
UNEXPECTED TOKEN IN PATTERN - guards_1.md:3:20:3:30
PARSE ERROR - guards_1.md:3:30:3:30
UNEXPECTED TOKEN IN EXPRESSION - guards_1.md:3:30:3:32
UNEXPECTED TOKEN IN PATTERN - guards_1.md:3:32:3:35
PARSE ERROR - guards_1.md:3:43:3:43
UNEXPECTED TOKEN IN EXPRESSION - guards_1.md:3:43:3:44
UNEXPECTED TOKEN IN PATTERN - guards_1.md:3:44:3:44
PARSE ERROR - guards_1.md:3:44:3:44
UNEXPECTED TOKEN IN EXPRESSION - guards_1.md:3:44:3:45
UNDEFINED VARIABLE - guards_1.md:1:7:1:12
UNRECOGNIZED SYNTAX - guards_1.md:2:7:2:20
UNUSED VARIABLE - guards_1.md:2:5:2:6
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

**guards_1.md:2:7:2:7:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
      ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**guards_1.md:2:16:2:18:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
               ^^


**IF WITHOUT ELSE**
This `if` is being used as an expression, but it doesn't have an `else`.

When `if` is used as an expression (to evaluate to a value), it must have an `else` branch to specify what value to use when the condition is `False`.

**guards_1.md:2:7:2:9:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
      ^^


**UNEXPECTED TOKEN IN PATTERN**
The token **positive: ** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

**guards_1.md:2:20:2:30:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
                   ^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

**guards_1.md:2:30:2:30:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
                             ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **${** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**guards_1.md:2:30:2:32:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
                             ^^


**UNEXPECTED TOKEN IN PATTERN**
The token **Num** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

**guards_1.md:2:32:2:35:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
                               ^^^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

**guards_1.md:2:43:2:43:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
                                          ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**guards_1.md:2:43:2:44:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
                                          ^


**UNEXPECTED TOKEN IN PATTERN**
The token  is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

**guards_1.md:2:44:2:44:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
                                           ^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

**guards_1.md:2:44:2:44:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
                                           ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **"** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**guards_1.md:2:44:2:45:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
                                           ^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

**guards_1.md:3:7:3:7:**
```roc
    x if x < 0 => "negative: ${Num.toStr x}"
```
      ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**guards_1.md:3:16:3:18:**
```roc
    x if x < 0 => "negative: ${Num.toStr x}"
```
               ^^


**IF WITHOUT ELSE**
This `if` is being used as an expression, but it doesn't have an `else`.

When `if` is used as an expression (to evaluate to a value), it must have an `else` branch to specify what value to use when the condition is `False`.

**guards_1.md:3:7:3:9:**
```roc
    x if x < 0 => "negative: ${Num.toStr x}"
```
      ^^


**UNEXPECTED TOKEN IN PATTERN**
The token **negative: ** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

**guards_1.md:3:20:3:30:**
```roc
    x if x < 0 => "negative: ${Num.toStr x}"
```
                   ^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

**guards_1.md:3:30:3:30:**
```roc
    x if x < 0 => "negative: ${Num.toStr x}"
```
                             ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **${** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**guards_1.md:3:30:3:32:**
```roc
    x if x < 0 => "negative: ${Num.toStr x}"
```
                             ^^


**UNEXPECTED TOKEN IN PATTERN**
The token **Num** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

**guards_1.md:3:32:3:35:**
```roc
    x if x < 0 => "negative: ${Num.toStr x}"
```
                               ^^^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

**guards_1.md:3:43:3:43:**
```roc
    x if x < 0 => "negative: ${Num.toStr x}"
```
                                          ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**guards_1.md:3:43:3:44:**
```roc
    x if x < 0 => "negative: ${Num.toStr x}"
```
                                          ^


**UNEXPECTED TOKEN IN PATTERN**
The token  is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

**guards_1.md:3:44:3:44:**
```roc
    x if x < 0 => "negative: ${Num.toStr x}"
```
                                           ^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

**guards_1.md:3:44:3:44:**
```roc
    x if x < 0 => "negative: ${Num.toStr x}"
```
                                           ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **"** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**guards_1.md:3:44:3:45:**
```roc
    x if x < 0 => "negative: ${Num.toStr x}"
```
                                           ^


**UNDEFINED VARIABLE**
Nothing is named `value` in this scope.
Is there an `import` or `exposing` missing up-top?

**guards_1.md:1:7:1:12:**
```roc
match value {
```
      ^^^^^


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**guards_1.md:2:7:2:20:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
      ^^^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

**UNUSED VARIABLE**
Variable `x` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:
**guards_1.md:2:5:2:6:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
    ^


# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:12),OpenCurly(1:13-1:14),
LowerIdent(2:5-2:6),KwIf(2:7-2:9),LowerIdent(2:10-2:11),OpGreaterThan(2:12-2:13),Int(2:14-2:15),OpFatArrow(2:16-2:18),StringStart(2:19-2:20),StringPart(2:20-2:30),OpenStringInterpolation(2:30-2:32),UpperIdent(2:32-2:35),NoSpaceDotLowerIdent(2:35-2:41),LowerIdent(2:42-2:43),CloseStringInterpolation(2:43-2:44),StringPart(2:44-2:44),StringEnd(2:44-2:45),
LowerIdent(3:5-3:6),KwIf(3:7-3:9),LowerIdent(3:10-3:11),OpLessThan(3:12-3:13),Int(3:14-3:15),OpFatArrow(3:16-3:18),StringStart(3:19-3:20),StringPart(3:20-3:30),OpenStringInterpolation(3:30-3:32),UpperIdent(3:32-3:35),NoSpaceDotLowerIdent(3:35-3:41),LowerIdent(3:42-3:43),CloseStringInterpolation(3:43-3:44),StringPart(3:44-3:44),StringEnd(3:44-3:45),
Underscore(4:5-4:6),OpFatArrow(4:7-4:9),StringStart(4:10-4:11),StringPart(4:11-4:16),StringEnd(4:16-4:17),
CloseCurly(5:1-5:2),EndOfFile(5:2-5:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.12 (raw "value"))
	(branches
		(branch @2.5-2.20
			(p-ident @2.5-2.6 (raw "x"))
			(e-malformed @2.7-2.20 (reason "no_else")))
		(branch @2.20-2.32
			(p-malformed @2.20-2.30 (tag "pattern_unexpected_token"))
			(e-malformed @2.30-2.32 (reason "expr_unexpected_token")))
		(branch @2.32-2.44
			(p-malformed @2.32-2.43 (tag "pattern_unexpected_token"))
			(e-malformed @2.43-2.44 (reason "expr_unexpected_token")))
		(branch @2.44-2.45
			(p-malformed @2.44-2.44 (tag "pattern_unexpected_token"))
			(e-malformed @2.44-2.45 (reason "expr_unexpected_token")))
		(branch @3.5-3.20
			(p-ident @3.5-3.6 (raw "x"))
			(e-malformed @3.7-3.20 (reason "no_else")))
		(branch @3.20-3.32
			(p-malformed @3.20-3.30 (tag "pattern_unexpected_token"))
			(e-malformed @3.30-3.32 (reason "expr_unexpected_token")))
		(branch @3.32-3.44
			(p-malformed @3.32-3.43 (tag "pattern_unexpected_token"))
			(e-malformed @3.43-3.44 (reason "expr_unexpected_token")))
		(branch @3.44-3.45
			(p-malformed @3.44-3.44 (tag "pattern_unexpected_token"))
			(e-malformed @3.44-3.45 (reason "expr_unexpected_token")))
		(branch @4.5-4.17
			(p-underscore)
			(e-string @4.10-4.17
				(e-string-part @4.11-4.16 (raw "other"))))))
~~~
# FORMATTED
~~~roc
match value {
	x => 
			=> 
			=> 
			=> 
	x => 
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
(expr @2.7-2.20 (type "Error"))
~~~
