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
KwMatch,LowerIdent,OpenCurly,
LowerIdent,KwIf,LowerIdent,OpGreaterThan,Int,OpFatArrow,StringStart,StringPart,OpenStringInterpolation,UpperIdent,NoSpaceDotLowerIdent,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
LowerIdent,KwIf,LowerIdent,OpLessThan,Int,OpFatArrow,StringStart,StringPart,OpenStringInterpolation,UpperIdent,NoSpaceDotLowerIdent,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
Underscore,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "value"))
	(branches
		(branch
			(p-ident (raw "x"))
			(e-malformed (reason "no_else")))
		(branch
			(p-malformed (tag "pattern_unexpected_token"))
			(e-malformed (reason "expr_unexpected_token")))
		(branch
			(p-malformed (tag "pattern_unexpected_token"))
			(e-malformed (reason "expr_unexpected_token")))
		(branch
			(p-malformed (tag "pattern_unexpected_token"))
			(e-malformed (reason "expr_unexpected_token")))
		(branch
			(p-ident (raw "x"))
			(e-malformed (reason "no_else")))
		(branch
			(p-malformed (tag "pattern_unexpected_token"))
			(e-malformed (reason "expr_unexpected_token")))
		(branch
			(p-malformed (tag "pattern_unexpected_token"))
			(e-malformed (reason "expr_unexpected_token")))
		(branch
			(p-malformed (tag "pattern_unexpected_token"))
			(e-malformed (reason "expr_unexpected_token")))
		(branch
			(p-underscore)
			(e-string
				(e-string-part (raw "other"))))))
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
(expr (type "Error"))
~~~
