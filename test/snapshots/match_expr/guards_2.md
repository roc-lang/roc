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
PARSE ERROR - guards_2.md:2:50:2:51
PARSE ERROR - guards_2.md:2:92:2:92
UNEXPECTED TOKEN IN EXPRESSION - guards_2.md:2:92:2:93
UNEXPECTED TOKEN IN PATTERN - guards_2.md:2:93:2:93
PARSE ERROR - guards_2.md:2:93:2:93
UNEXPECTED TOKEN IN EXPRESSION - guards_2.md:2:93:2:94
PARSE ERROR - guards_2.md:3:12:3:12
UNEXPECTED TOKEN IN EXPRESSION - guards_2.md:3:22:3:24
PARSE ERROR - guards_2.md:3:25:3:26
PARSE ERROR - guards_2.md:3:61:3:61
UNEXPECTED TOKEN IN EXPRESSION - guards_2.md:3:61:3:62
UNEXPECTED TOKEN IN PATTERN - guards_2.md:3:62:3:62
PARSE ERROR - guards_2.md:3:62:3:62
UNEXPECTED TOKEN IN EXPRESSION - guards_2.md:3:62:3:63
UNDEFINED VARIABLE - guards_2.md:1:7:1:12
INVALID IF BRANCH - :0:0:0:0
UNUSED VARIABLE - guards_2.md:2:6:2:11
UNRECOGNIZED SYNTAX - guards_2.md:2:92:2:93
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


**PARSE ERROR**
A parsing error occurred: `string_expected_close_interpolation`
This is an unexpected parsing error. Please check your syntax.

**guards_2.md:2:50:2:51:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
                                                 ^


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


**PARSE ERROR**
A parsing error occurred: `string_expected_close_interpolation`
This is an unexpected parsing error. Please check your syntax.

**guards_2.md:3:25:3:26:**
```roc
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
```
                        ^


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


**INVALID IF BRANCH**
The branch in this `if` expression could not be processed.

The branch must contain a valid expression. Check for syntax errors or missing values.

**UNUSED VARIABLE**
Variable `first` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_first` to suppress this warning.
The unused variable is declared here:
**guards_2.md:2:6:2:11:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
     ^^^^^


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**guards_2.md:2:92:2:93:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
                                                                                           ^

This might be a syntax error, an unsupported language feature, or a typo.

# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenSquare,LowerIdent,Comma,DoubleDot,KwAs,LowerIdent,CloseSquare,KwIf,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpGreaterThan,Int,OpFatArrow,StringStart,StringPart,OpenStringInterpolation,UpperIdent,NoSpaceDotLowerIdent,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
OpenSquare,LowerIdent,Comma,LowerIdent,CloseSquare,KwIf,LowerIdent,OpEquals,LowerIdent,OpFatArrow,StringStart,StringPart,OpenStringInterpolation,UpperIdent,NoSpaceDotLowerIdent,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
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
			(p-list
				(p-ident (raw "first"))
				(p-list-rest (name "rest")))
			(e-if-without-else
				(e-binop (op ">")
					(e-apply
						(e-ident (raw "List.len"))
						(e-ident (raw "rest")))
					(e-int (raw "5")))
				(e-malformed (reason "expr_unexpected_token"))))
		(branch
			(p-string (raw """))
			(e-malformed (reason "expr_unexpected_token")))
		(branch
			(p-malformed (tag "pattern_unexpected_token"))
			(e-malformed (reason "expr_unexpected_token")))
		(branch
			(p-list
				(p-ident (raw "x"))
				(p-ident (raw "y")))
			(e-if-without-else
				(e-binop (op "==")
					(e-ident (raw "x"))
					(e-ident (raw "y")))
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
	[first, .. as rest] => if List.len(rest) > 5 
	 => 
	 => 
	[x, y] => if x == y 
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
