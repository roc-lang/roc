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
UNEXPECTED TOKEN IN EXPRESSION - guards_2.md:2:47:2:49
PARSE ERROR - guards_2.md:2:50:2:51
UNEXPECTED TOKEN IN PATTERN - guards_2.md:2:51:2:75
UNEXPECTED TOKEN IN EXPRESSION - guards_2.md:2:75:2:77
UNEXPECTED TOKEN IN PATTERN - guards_2.md:2:92:2:93
UNEXPECTED TOKEN IN EXPRESSION - guards_2.md:2:93:2:93
UNEXPECTED TOKEN IN PATTERN - guards_2.md:2:93:2:94
UNEXPECTED TOKEN IN PATTERN - guards_2.md:3:12:3:14
UNEXPECTED TOKEN IN PATTERN - guards_2.md:3:22:3:24
PARSE ERROR - guards_2.md:3:25:3:26
UNEXPECTED TOKEN IN PATTERN - guards_2.md:3:61:3:62
UNEXPECTED TOKEN IN EXPRESSION - guards_2.md:3:62:3:62
UNEXPECTED TOKEN IN PATTERN - guards_2.md:3:62:3:63
UNEXPECTED TOKEN IN EXPRESSION - guards_2.md:4:5:4:6
UNEXPECTED TOKEN IN PATTERN - guards_2.md:4:7:4:9
UNDEFINED VARIABLE - guards_2.md:1:7:1:12
UNUSED VARIABLE - guards_2.md:2:6:2:11
UNUSED VARIABLE - guards_2.md:1:1:1:1
UNDEFINED VARIABLE - guards_2.md:2:87:2:92
UNUSED VARIABLE - guards_2.md:2:77:2:86
UNDEFINED VARIABLE - guards_2.md:3:6:3:7
UNDEFINED VARIABLE - guards_2.md:3:9:3:10
UNDEFINED VARIABLE - guards_2.md:3:15:3:16
UNDEFINED VARIABLE - guards_2.md:3:20:3:21
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **=>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**guards_2.md:2:47:2:49:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
                                              ^^


**PARSE ERROR**
A parsing error occurred: `no_else`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**guards_2.md:2:50:2:51:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
                                                 ^


**UNEXPECTED TOKEN IN PATTERN**
The token **long list starting with ** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**guards_2.md:2:51:2:75:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
                                                  ^^^^^^^^^^^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **${** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**guards_2.md:2:75:2:77:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
                                                                          ^^


**UNEXPECTED TOKEN IN PATTERN**
The token **}** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**guards_2.md:2:92:2:93:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
                                                                                           ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**guards_2.md:2:93:2:93:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
                                                                                            


**UNEXPECTED TOKEN IN PATTERN**
The token **"** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**guards_2.md:2:93:2:94:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
                                                                                            ^


**UNEXPECTED TOKEN IN PATTERN**
The token **if** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**guards_2.md:3:12:3:14:**
```roc
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
```
           ^^


**UNEXPECTED TOKEN IN PATTERN**
The token **=>** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**guards_2.md:3:22:3:24:**
```roc
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
```
                     ^^


**PARSE ERROR**
A parsing error occurred: `string_expected_close_interpolation`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**guards_2.md:3:25:3:26:**
```roc
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
```
                        ^


**UNEXPECTED TOKEN IN PATTERN**
The token **}** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**guards_2.md:3:61:3:62:**
```roc
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
```
                                                            ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**guards_2.md:3:62:3:62:**
```roc
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
```
                                                             


**UNEXPECTED TOKEN IN PATTERN**
The token **"** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**guards_2.md:3:62:3:63:**
```roc
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
```
                                                             ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **_** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**guards_2.md:4:5:4:6:**
```roc
    _ => "other"
```
    ^


**UNEXPECTED TOKEN IN PATTERN**
The token **=>** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**guards_2.md:4:7:4:9:**
```roc
    _ => "other"
```
      ^^


**UNDEFINED VARIABLE**
Nothing is named `value` in this scope.
Is there an `import` or `exposing` missing up-top?

**guards_2.md:1:7:1:12:**
```roc
match value {
```
      ^^^^^


**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

**UNUSED VARIABLE**
Variable `first` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_first to suppress this warning.
The unused variable is declared here:
**guards_2.md:2:6:2:11:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
     ^^^^^


**UNUSED VARIABLE**
Variable `rest` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_rest to suppress this warning.
The unused variable is declared here:
**guards_2.md:1:1:1:1:**
```roc

```



**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

**UNDEFINED VARIABLE**
Nothing is named `first` in this scope.
Is there an `import` or `exposing` missing up-top?

**guards_2.md:2:87:2:92:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
                                                                                      ^^^^^


**UNUSED VARIABLE**
Variable `toStr` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_toStr to suppress this warning.
The unused variable is declared here:
**guards_2.md:2:77:2:86:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
                                                                            ^^^^^^^^^


**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

**guards_2.md:3:6:3:7:**
```roc
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
```
     ^


**UNDEFINED VARIABLE**
Nothing is named `y` in this scope.
Is there an `import` or `exposing` missing up-top?

**guards_2.md:3:9:3:10:**
```roc
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
```
        ^


**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

**guards_2.md:3:15:3:16:**
```roc
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
```
              ^


**UNDEFINED VARIABLE**
Nothing is named `y` in this scope.
Is there an `import` or `exposing` missing up-top?

**guards_2.md:3:20:3:21:**
```roc
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
```
                   ^


**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

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
			(e-malformed @2.50-2.51 (reason "no_else")))
		(branch @2.51-2.77
			(p-malformed @2.51-2.75 (tag "pattern_unexpected_token"))
			(e-malformed @2.75-2.77 (reason "expr_unexpected_token")))
		(branch @2.77-2.92
			(p-ident @2.77-2.86 (raw ".toStr"))
			(e-ident @2.87-2.92 (raw "first")))
		(branch @2.92-2.93
			(p-malformed @2.92-2.93 (tag "pattern_unexpected_token"))
			(e-malformed @2.93-2.93 (reason "expr_unexpected_token")))
		(branch @2.93-3.11
			(p-malformed @2.93-2.94 (tag "pattern_unexpected_token"))
			(e-list @3.5-3.11
				(e-ident @3.6-3.7 (raw "x"))
				(e-ident @3.9-3.10 (raw "y"))))
		(branch @3.12-3.21
			(p-malformed @3.12-3.14 (tag "pattern_unexpected_token"))
			(e-binop @3.15-3.21 (op "==")
				(e-ident @3.15-3.16 (raw "x"))
				(e-ident @3.20-3.21 (raw "y"))))
		(branch @3.22-3.61
			(p-malformed @3.22-3.24 (tag "pattern_unexpected_token"))
			(e-malformed @3.25-3.61 (reason "string_expected_close_interpolation")))
		(branch @3.61-3.62
			(p-malformed @3.61-3.62 (tag "pattern_unexpected_token"))
			(e-malformed @3.62-3.62 (reason "expr_unexpected_token")))
		(branch @3.62-4.6
			(p-malformed @3.62-3.63 (tag "pattern_unexpected_token"))
			(e-malformed @4.5-4.6 (reason "expr_unexpected_token")))
		(branch @4.7-4.17
			(p-malformed @4.7-4.9 (tag "pattern_unexpected_token"))
			(e-string @4.10-4.17
				(e-string-part @4.11-4.16 (raw "other"))))))
~~~
# FORMATTED
~~~roc
match value {
	[first, .. as rest] => 	 => 	toStr => first	 => 	 =>
		[x, y]	 => x == y	 => 	 => 	 =>
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
						(p-list @2.5-2.24
							(patterns
								(p-assign @2.6-2.11 (ident "first")))
							(rest-at (index 1)
								(p-assign @1.1-1.1 (ident "rest"))))))
				(value
					(e-runtime-error (tag "expr_not_canonicalized"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error @2.51-2.75 (tag "pattern_not_canonicalized"))))
				(value
					(e-runtime-error (tag "expr_not_canonicalized"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-assign @2.77-2.86 (ident "toStr"))))
				(value
					(e-runtime-error (tag "ident_not_in_scope"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error @2.92-2.93 (tag "pattern_not_canonicalized"))))
				(value
					(e-runtime-error (tag "expr_not_canonicalized"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error @2.93-2.94 (tag "pattern_not_canonicalized"))))
				(value
					(e-list @3.5-3.11
						(elems
							(e-runtime-error (tag "ident_not_in_scope"))
							(e-runtime-error (tag "ident_not_in_scope"))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error @3.12-3.14 (tag "pattern_not_canonicalized"))))
				(value
					(e-binop @3.15-3.21 (op "eq")
						(e-runtime-error (tag "ident_not_in_scope"))
						(e-runtime-error (tag "ident_not_in_scope")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error @3.22-3.24 (tag "pattern_not_canonicalized"))))
				(value
					(e-runtime-error (tag "expr_not_canonicalized"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error @3.61-3.62 (tag "pattern_not_canonicalized"))))
				(value
					(e-runtime-error (tag "expr_not_canonicalized"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error @3.62-3.63 (tag "pattern_not_canonicalized"))))
				(value
					(e-runtime-error (tag "expr_not_canonicalized"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error @4.7-4.9 (tag "pattern_not_canonicalized"))))
				(value
					(e-string @4.10-4.17
						(e-literal @4.11-4.16 (string "other"))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-5.2 (type "Error"))
~~~
