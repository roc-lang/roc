# META
~~~ini
description=Pattern alternatives with mixed pattern types
type=expr
~~~
# SOURCE
~~~roc
match ... {
	1 | 2 | 3 => "small numbers"
	"hello" | "world" => "greetings"
	Ok(_) | Some(_) => "success value"
	[] | [_] => "short list"
	(0, _) | (_, 0) => "has zero"
	_ => "other"
}
~~~
# TOKENS
~~~text
KwMatch TripleDot OpenCurly Int OpBar Int OpBar Int OpFatArrow String String OpBar String OpFatArrow String UpperIdent OpenRound Underscore CloseRound OpBar UpperIdent OpenRound Underscore CloseRound OpFatArrow String OpenSquare CloseSquare OpBar OpenSquare Underscore CloseSquare OpFatArrow String OpenRound Int Comma Underscore CloseRound OpBar OpenRound Underscore Comma Int CloseRound OpFatArrow String Underscore OpFatArrow String CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (ellipsis)
)
  (branch1     (binop_thick_arrow
      (binop_or
        (binop_or
          (num_literal_i32 1)
          (num_literal_i32 2)
        )
        (num_literal_i32 3)
      )
      (block
        (str_literal_big "small numbers")
        (str_literal_big "hello")
        (malformed)
        (str_literal_big "greetings")
      )
    )
)
  (branch2     (binop_thick_arrow
      (binop_or
        (apply_uc
          (uc "Ok")
          (underscore)
        )
        (apply_uc
          (uc "Some")
          (underscore)
        )
      )
      (block
        (str_literal_big "success value")
        (list_literal)
        (malformed)
        (apply_anon
          (str_literal_big "short list")
          (tuple_literal
            (num_literal_i32 0)
            (underscore)
          )
        )
        (malformed)
        (str_literal_big "has zero")
      )
    )
)
  (branch3     (binop_thick_arrow
      (underscore)
      (str_literal_big "other")
    )
))
~~~
# FORMATTED
~~~roc
match ...
	(1 || 2) || 3 => 
		"small numbers"
		"hello"
		=> 
		"greetings"
	Ok(_) || Some(_) => 
		"success value"
		[]
		=> 
		"short list"((0, _))
		=> 
		"has zero"
	_ => "other"
~~~
# EXPECTED
INCOMPATIBLE MATCH PATTERNS - pattern_alternatives_mixed.md:1:1:1:1
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**pattern_alternatives_mixed.md:3:20:3:23:**
```roc
	"hello" | "world" => "greetings"
```
	                  ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**pattern_alternatives_mixed.md:5:11:5:14:**
```roc
	[] | [_] => "short list"
```
	         ^^^


**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**pattern_alternatives_mixed.md:5:26:6:2:**
```roc
	[] | [_] => "short list"
	(0, _) | (_, 0) => "has zero"
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**pattern_alternatives_mixed.md:6:18:6:21:**
```roc
	(0, _) | (_, 0) => "has zero"
```
	                ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**pattern_alternatives_mixed.md:2:2:3:34:**
```roc
	1 | 2 | 3 => "small numbers"
	"hello" | "world" => "greetings"
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**pattern_alternatives_mixed.md:4:2:4:17:**
```roc
	Ok(_) | Some(_) => "success value"
```
	^^^^^^^^^^^^^^^


**PATTERN IN EXPRESSION CONTEXT**
Found a pattern where an expression was expected.
Patterns can only appear in specific contexts like function parameters, destructuring assignments, or **when** branches.

**pattern_alternatives_mixed.md:6:6:6:7:**
```roc
	(0, _) | (_, 0) => "has zero"
```
	    ^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**pattern_alternatives_mixed.md:7:2:7:14:**
```roc
	_ => "other"
```
	^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
; Total type variables: 42
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 Str)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 Str)
(var #27 Num *)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 _)
(var #35 Str)
(var #36 _)
(var #37 _)
(var #38 _)
(var #39 _)
(var #40 _)
(var #41 _)
~~~
# TYPES
~~~roc
~~~
