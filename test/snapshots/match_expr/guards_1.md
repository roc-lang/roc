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
PARSE ERROR - guards_1.md:2:19:2:20
PARSE ERROR - guards_1.md:2:43:2:43
UNEXPECTED TOKEN IN EXPRESSION - guards_1.md:2:43:2:44
UNEXPECTED TOKEN IN PATTERN - guards_1.md:2:44:2:44
PARSE ERROR - guards_1.md:2:44:2:44
UNEXPECTED TOKEN IN EXPRESSION - guards_1.md:2:44:2:45
PARSE ERROR - guards_1.md:3:7:3:7
UNEXPECTED TOKEN IN EXPRESSION - guards_1.md:3:16:3:18
PARSE ERROR - guards_1.md:3:19:3:20
PARSE ERROR - guards_1.md:3:43:3:43
UNEXPECTED TOKEN IN EXPRESSION - guards_1.md:3:43:3:44
UNEXPECTED TOKEN IN PATTERN - guards_1.md:3:44:3:44
PARSE ERROR - guards_1.md:3:44:3:44
UNEXPECTED TOKEN IN EXPRESSION - guards_1.md:3:44:3:45
UNDEFINED VARIABLE - guards_1.md:1:7:1:12
INVALID IF BRANCH - :0:0:0:0
INVALID PATTERN ARGUMENT - :0:0:0:0
UNRECOGNIZED SYNTAX - guards_1.md:2:43:2:44
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


**PARSE ERROR**
A parsing error occurred: `string_expected_close_interpolation`
This is an unexpected parsing error. Please check your syntax.

**guards_1.md:2:19:2:20:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
                  ^


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


**PARSE ERROR**
A parsing error occurred: `string_expected_close_interpolation`
This is an unexpected parsing error. Please check your syntax.

**guards_1.md:3:19:3:20:**
```roc
    x if x < 0 => "negative: ${Num.toStr x}"
```
                  ^


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


**INVALID IF BRANCH**
The branch in this `if` expression could not be processed.

The branch must contain a valid expression. Check for syntax errors or missing values.

**INVALID PATTERN ARGUMENT**
Pattern arguments must be valid patterns like identifiers, literals, or destructuring patterns.

**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**guards_1.md:2:43:2:44:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
                                          ^

This might be a syntax error, an unsupported language feature, or a typo.

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
			(e-if-without-else
				(e-binop (op ">")
					(e-ident (raw "x"))
					(e-int (raw "0")))
				(e-malformed (reason "expr_unexpected_token"))))
		(branch
			(p-string (raw """))
			(e-malformed (reason "expr_unexpected_token")))
		(branch
			(p-malformed (tag "pattern_unexpected_token"))
			(e-malformed (reason "expr_unexpected_token")))
		(branch
			(p-ident (raw "x"))
			(e-if-without-else
				(e-binop (op "<")
					(e-ident (raw "x"))
					(e-int (raw "0")))
				(e-malformed (reason "expr_unexpected_token"))))
		(branch
			(p-string (raw """))
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
	x => if x > 0 
	 => 
	 => 
	x => if x < 0 
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
