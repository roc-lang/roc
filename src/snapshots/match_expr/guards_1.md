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
UNEXPECTED TOKEN IN PATTERN - guards_1.md:2:20:2:30
PARSE ERROR - guards_1.md:2:30:2:30
UNEXPECTED TOKEN IN EXPRESSION - guards_1.md:2:30:2:32
PARSE ERROR - guards_1.md:2:42:2:42
UNEXPECTED TOKEN IN PATTERN - guards_1.md:2:43:2:44
PARSE ERROR - guards_1.md:2:44:2:44
UNEXPECTED TOKEN IN EXPRESSION - guards_1.md:2:44:2:44
UNEXPECTED TOKEN IN PATTERN - guards_1.md:2:44:2:45
PARSE ERROR - guards_1.md:3:5:3:5
UNEXPECTED TOKEN IN PATTERN - guards_1.md:3:7:3:9
PARSE ERROR - guards_1.md:3:10:3:10
UNEXPECTED TOKEN IN PATTERN - guards_1.md:3:16:3:18
PARSE ERROR - guards_1.md:3:19:3:19
PARSE ERROR - guards_1.md:3:19:3:20
UNEXPECTED TOKEN IN PATTERN - guards_1.md:3:43:3:44
PARSE ERROR - guards_1.md:3:44:3:44
UNEXPECTED TOKEN IN EXPRESSION - guards_1.md:3:44:3:44
UNEXPECTED TOKEN IN PATTERN - guards_1.md:3:44:3:45
PARSE ERROR - guards_1.md:4:5:4:5
UNEXPECTED TOKEN IN EXPRESSION - guards_1.md:4:5:4:6
UNEXPECTED TOKEN IN PATTERN - guards_1.md:4:7:4:9
PARSE ERROR - guards_1.md:4:10:4:10
UNDEFINED VARIABLE - guards_1.md:1:7:1:12
COMPILER DIAGNOSTIC - guards_1.md:0:0:0:0
UNUSED VARIABLE - guards_1.md:2:5:2:6
COMPILER DIAGNOSTIC - guards_1.md:0:0:0:0
COMPILER DIAGNOSTIC - guards_1.md:0:0:0:0
UNDEFINED VARIABLE - guards_1.md:2:42:2:43
UNUSED VARIABLE - guards_1.md:2:32:2:41
COMPILER DIAGNOSTIC - guards_1.md:0:0:0:0
COMPILER DIAGNOSTIC - guards_1.md:0:0:0:0
COMPILER DIAGNOSTIC - guards_1.md:0:0:0:0
UNDEFINED VARIABLE - guards_1.md:3:5:3:6
COMPILER DIAGNOSTIC - guards_1.md:0:0:0:0
UNDEFINED VARIABLE - guards_1.md:3:10:3:11
COMPILER DIAGNOSTIC - guards_1.md:0:0:0:0
COMPILER DIAGNOSTIC - guards_1.md:0:0:0:0
COMPILER DIAGNOSTIC - guards_1.md:0:0:0:0
COMPILER DIAGNOSTIC - guards_1.md:0:0:0:0
COMPILER DIAGNOSTIC - guards_1.md:0:0:0:0
COMPILER DIAGNOSTIC - guards_1.md:0:0:0:0
COMPILER DIAGNOSTIC - guards_1.md:0:0:0:0
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**guards_1.md:2:7:2:7:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
      


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**guards_1.md:2:16:2:18:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
               ^^


**PARSE ERROR**
A parsing error occurred: `no_else`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**guards_1.md:2:19:2:20:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
                  ^


**UNEXPECTED TOKEN IN PATTERN**
The token **positive: ** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**guards_1.md:2:20:2:30:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
                   ^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**guards_1.md:2:30:2:30:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
                             


**UNEXPECTED TOKEN IN EXPRESSION**
The token **${** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**guards_1.md:2:30:2:32:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
                             ^^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**guards_1.md:2:42:2:42:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
                                         


**UNEXPECTED TOKEN IN PATTERN**
The token **}** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**guards_1.md:2:43:2:44:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
                                          ^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**guards_1.md:2:44:2:44:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
                                           


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**guards_1.md:2:44:2:44:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
                                           


**UNEXPECTED TOKEN IN PATTERN**
The token **"** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**guards_1.md:2:44:2:45:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
                                           ^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**guards_1.md:3:5:3:5:**
```roc
    x if x < 0 => "negative: ${Num.toStr x}"
```
    


**UNEXPECTED TOKEN IN PATTERN**
The token **if** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**guards_1.md:3:7:3:9:**
```roc
    x if x < 0 => "negative: ${Num.toStr x}"
```
      ^^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**guards_1.md:3:10:3:10:**
```roc
    x if x < 0 => "negative: ${Num.toStr x}"
```
         


**UNEXPECTED TOKEN IN PATTERN**
The token **=>** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**guards_1.md:3:16:3:18:**
```roc
    x if x < 0 => "negative: ${Num.toStr x}"
```
               ^^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**guards_1.md:3:19:3:19:**
```roc
    x if x < 0 => "negative: ${Num.toStr x}"
```
                  


**PARSE ERROR**
A parsing error occurred: `string_expected_close_interpolation`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**guards_1.md:3:19:3:20:**
```roc
    x if x < 0 => "negative: ${Num.toStr x}"
```
                  ^


**UNEXPECTED TOKEN IN PATTERN**
The token **}** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**guards_1.md:3:43:3:44:**
```roc
    x if x < 0 => "negative: ${Num.toStr x}"
```
                                          ^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**guards_1.md:3:44:3:44:**
```roc
    x if x < 0 => "negative: ${Num.toStr x}"
```
                                           


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**guards_1.md:3:44:3:44:**
```roc
    x if x < 0 => "negative: ${Num.toStr x}"
```
                                           


**UNEXPECTED TOKEN IN PATTERN**
The token **"** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**guards_1.md:3:44:3:45:**
```roc
    x if x < 0 => "negative: ${Num.toStr x}"
```
                                           ^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**guards_1.md:4:5:4:5:**
```roc
    _ => "other"
```
    


**UNEXPECTED TOKEN IN EXPRESSION**
The token **_** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**guards_1.md:4:5:4:6:**
```roc
    _ => "other"
```
    ^


**UNEXPECTED TOKEN IN PATTERN**
The token **=>** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**guards_1.md:4:7:4:9:**
```roc
    _ => "other"
```
      ^^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**guards_1.md:4:10:4:10:**
```roc
    _ => "other"
```
         


**UNDEFINED VARIABLE**

**Undefined Variable**
The variable 'value' is not defined:
**guards_1.md:1:7:1:12:**
```roc
match value {
```
      ^^^^^


**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'expr_not_canonicalized' is not yet handled in report generation.
**guards_1.md:0:0:0:0**

**UNUSED VARIABLE**
Variable `x` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:
**guards_1.md:2:5:2:6:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
    ^


**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'pattern_not_canonicalized' is not yet handled in report generation.
**guards_1.md:0:0:0:0**

**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'expr_not_canonicalized' is not yet handled in report generation.
**guards_1.md:0:0:0:0**

**UNDEFINED VARIABLE**

**Undefined Variable**
The variable 'x' is not defined:
**guards_1.md:2:42:2:43:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
                                         ^


**UNUSED VARIABLE**
Variable `toStr` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_toStr` to suppress this warning.
The unused variable is declared here:
**guards_1.md:2:32:2:41:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
```
                               ^^^^^^^^^


**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'pattern_not_canonicalized' is not yet handled in report generation.
**guards_1.md:0:0:0:0**

**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'expr_not_canonicalized' is not yet handled in report generation.
**guards_1.md:0:0:0:0**

**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'pattern_not_canonicalized' is not yet handled in report generation.
**guards_1.md:0:0:0:0**

**UNDEFINED VARIABLE**

**Undefined Variable**
The variable 'x' is not defined:
**guards_1.md:3:5:3:6:**
```roc
    x if x < 0 => "negative: ${Num.toStr x}"
```
    ^


**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'pattern_not_canonicalized' is not yet handled in report generation.
**guards_1.md:0:0:0:0**

**UNDEFINED VARIABLE**

**Undefined Variable**
The variable 'x' is not defined:
**guards_1.md:3:10:3:11:**
```roc
    x if x < 0 => "negative: ${Num.toStr x}"
```
         ^


**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'pattern_not_canonicalized' is not yet handled in report generation.
**guards_1.md:0:0:0:0**

**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'expr_not_canonicalized' is not yet handled in report generation.
**guards_1.md:0:0:0:0**

**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'pattern_not_canonicalized' is not yet handled in report generation.
**guards_1.md:0:0:0:0**

**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'expr_not_canonicalized' is not yet handled in report generation.
**guards_1.md:0:0:0:0**

**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'pattern_not_canonicalized' is not yet handled in report generation.
**guards_1.md:0:0:0:0**

**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'expr_not_canonicalized' is not yet handled in report generation.
**guards_1.md:0:0:0:0**

**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'pattern_not_canonicalized' is not yet handled in report generation.
**guards_1.md:0:0:0:0**

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
			(e-malformed @2.19-2.20 (reason "no_else")))
		(branch @2.20-2.32
			(p-malformed @2.20-2.30 (tag "pattern_unexpected_token"))
			(e-malformed @2.30-2.32 (reason "expr_unexpected_token")))
		(branch @2.32-2.43
			(p-ident @2.32-2.41 (raw ".toStr"))
			(e-ident @2.42-2.43 (raw "x")))
		(branch @2.43-2.44
			(p-malformed @2.43-2.44 (tag "pattern_unexpected_token"))
			(e-malformed @2.44-2.44 (reason "expr_unexpected_token")))
		(branch @2.44-3.6
			(p-malformed @2.44-2.45 (tag "pattern_unexpected_token"))
			(e-ident @3.5-3.6 (raw "x")))
		(branch @3.7-3.15
			(p-malformed @3.7-3.9 (tag "pattern_unexpected_token"))
			(e-binop @3.10-3.15 (op "<")
				(e-ident @3.10-3.11 (raw "x"))
				(e-int @3.14-3.15 (raw "0"))))
		(branch @3.16-3.43
			(p-malformed @3.16-3.18 (tag "pattern_unexpected_token"))
			(e-malformed @3.19-3.43 (reason "string_expected_close_interpolation")))
		(branch @3.43-3.44
			(p-malformed @3.43-3.44 (tag "pattern_unexpected_token"))
			(e-malformed @3.44-3.44 (reason "expr_unexpected_token")))
		(branch @3.44-4.6
			(p-malformed @3.44-3.45 (tag "pattern_unexpected_token"))
			(e-malformed @4.5-4.6 (reason "expr_unexpected_token")))
		(branch @4.7-4.17
			(p-malformed @4.7-4.9 (tag "pattern_unexpected_token"))
			(e-string @4.10-4.17
				(e-string-part @4.11-4.16 (raw "other"))))))
~~~
# FORMATTED
~~~roc
match value {
	x => 
	 => 
	toStr => x
	 => 
	 =>
		x
	 => x < 0
	 => 
	 => 
	 =>
		
	 => "other"
}
~~~
# CANONICALIZE
~~~clojure
(e-match @1.1-5.2
	(match @1.1-5.2
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-assign @2.5-2.6 (ident "x"))))
				(value
					(e-runtime-error (tag "expr_not_canonicalized"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error @1.1-1.1 (tag "pattern_not_canonicalized"))))
				(value
					(e-runtime-error (tag "expr_not_canonicalized"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-assign @2.32-2.41 (ident "toStr"))))
				(value
					(e-runtime-error (tag "ident_not_in_scope"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error @1.1-1.1 (tag "pattern_not_canonicalized"))))
				(value
					(e-runtime-error (tag "expr_not_canonicalized"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error @1.1-1.1 (tag "pattern_not_canonicalized"))))
				(value
					(e-runtime-error (tag "ident_not_in_scope"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error @1.1-1.1 (tag "pattern_not_canonicalized"))))
				(value
					(e-binop @3.10-3.15 (op "lt")
						(e-runtime-error (tag "ident_not_in_scope"))
						(e-int @3.14-3.15 (value "0")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error @1.1-1.1 (tag "pattern_not_canonicalized"))))
				(value
					(e-runtime-error (tag "expr_not_canonicalized"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error @1.1-1.1 (tag "pattern_not_canonicalized"))))
				(value
					(e-runtime-error (tag "expr_not_canonicalized"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error @1.1-1.1 (tag "pattern_not_canonicalized"))))
				(value
					(e-runtime-error (tag "expr_not_canonicalized"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error @1.1-1.1 (tag "pattern_not_canonicalized"))))
				(value
					(e-string @4.10-4.17
						(e-literal @4.11-4.16 (string "other"))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-5.2 (type "Error"))
~~~
