# META
~~~ini
description=Match expression with typed suffix in pattern (not supported)
type=expr
~~~
# SOURCE
~~~roc
match x {
    3.14.F64 => "pi"
    0.0.F64 => "zero"
    value => "other"
}
~~~
# EXPECTED
PARSE ERROR - f64_pattern_literal_error.md:2:9:2:9
UNEXPECTED TOKEN IN EXPRESSION - f64_pattern_literal_error.md:2:9:2:13
UNEXPECTED TOKEN IN PATTERN - f64_pattern_literal_error.md:2:14:2:16
PARSE ERROR - f64_pattern_literal_error.md:2:17:2:17
PARSE ERROR - f64_pattern_literal_error.md:3:8:3:8
UNEXPECTED TOKEN IN EXPRESSION - f64_pattern_literal_error.md:3:8:3:12
UNEXPECTED TOKEN IN PATTERN - f64_pattern_literal_error.md:3:13:3:15
PARSE ERROR - f64_pattern_literal_error.md:3:16:3:16
UNDEFINED VARIABLE - f64_pattern_literal_error.md:1:7:1:8
UNRECOGNIZED SYNTAX - f64_pattern_literal_error.md:2:9:2:13
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

**f64_pattern_literal_error.md:2:9:2:9:**
```roc
    3.14.F64 => "pi"
```
        ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.F64** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**f64_pattern_literal_error.md:2:9:2:13:**
```roc
    3.14.F64 => "pi"
```
        ^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **=>** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

**f64_pattern_literal_error.md:2:14:2:16:**
```roc
    3.14.F64 => "pi"
```
             ^^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

**f64_pattern_literal_error.md:2:17:2:17:**
```roc
    3.14.F64 => "pi"
```
                ^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

**f64_pattern_literal_error.md:3:8:3:8:**
```roc
    0.0.F64 => "zero"
```
       ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.F64** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**f64_pattern_literal_error.md:3:8:3:12:**
```roc
    0.0.F64 => "zero"
```
       ^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **=>** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

**f64_pattern_literal_error.md:3:13:3:15:**
```roc
    0.0.F64 => "zero"
```
            ^^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

**f64_pattern_literal_error.md:3:16:3:16:**
```roc
    0.0.F64 => "zero"
```
               ^


**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

**f64_pattern_literal_error.md:1:7:1:8:**
```roc
match x {
```
      ^


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**f64_pattern_literal_error.md:2:9:2:13:**
```roc
    3.14.F64 => "pi"
```
        ^^^^

This might be a syntax error, an unsupported language feature, or a typo.

# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
Float,NoSpaceDotUpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
Float,NoSpaceDotUpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
LowerIdent,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "x"))
	(branches
		(branch
			(p-frac (raw "3.14"))
			(e-malformed (reason "expr_unexpected_token")))
		(branch
			(p-malformed (tag "pattern_unexpected_token"))
			(e-string
				(e-string-part (raw "pi"))))
		(branch
			(p-frac (raw "0.0"))
			(e-malformed (reason "expr_unexpected_token")))
		(branch
			(p-malformed (tag "pattern_unexpected_token"))
			(e-string
				(e-string-part (raw "zero"))))
		(branch
			(p-ident (raw "value"))
			(e-string
				(e-string-part (raw "other"))))))
~~~
# FORMATTED
~~~roc
match x {
	3.14 => 
	 => "pi"
	0.0 => 
	 => "zero"
	value => "other"
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
