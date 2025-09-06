# META
~~~ini
description=Match expression with complex list patterns containing tagged values
type=expr
~~~
# SOURCE
~~~roc
match events {
    [] => "no events"
    [Click(x, y)] => "single click at (${Num.toStr(x)}, ${Num.toStr(y)})"
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr(List.len(rest))} more events"
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr(dx)},${Num.toStr(dy)} then ${Num.toStr(dx2)},${Num.toStr(dy2)}"
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr(amount)} then click at ${Num.toStr(x)},${Num.toStr(y)}"
    _ => "other event pattern"
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare CloseSquare OpFatArrow String OpenSquare UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound CloseSquare OpFatArrow String OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma DoubleDot KwAs LowerIdent CloseSquare OpFatArrow String OpenSquare UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound Comma DoubleDot KwAs LowerIdent CloseSquare OpFatArrow String OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound Comma DoubleDot KwAs LowerIdent CloseSquare OpFatArrow String Underscore OpFatArrow String CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "events")
)
  (branch1     (binop_thick_arrow
      (list_literal)
      (block
        (str_literal_big "no events")
        (binop_thick_arrow
          (list_literal
            (apply_uc
              (uc "Click")
              (tuple_literal
                (lc "x")
                (lc "y")
              )
            )
          )
          (str_literal_big "single click at (${Num.toStr(x)}, ${Num.toStr(y)})")
        )
        (list_literal
          (apply_uc
            (uc "KeyPress")
            (lc "key")
          )
          (unary_double_dot <unary_op>)
        )
        (lc "rest")
        (binop_thick_arrow
          (malformed)
          (str_literal_big "key ${key} pressed, ${Num.toStr(List.len(rest))} more events")
        )
        (list_literal
          (apply_uc
            (uc "Move")
            (tuple_literal
              (lc "dx")
              (lc "dy")
            )
          )
          (apply_uc
            (uc "Move")
            (tuple_literal
              (lc "dx2")
              (lc "dy2")
            )
          )
          (unary_double_dot <unary_op>)
        )
        (lc "others")
        (binop_thick_arrow
          (malformed)
          (str_literal_big "moved ${Num.toStr(dx)},${Num.toStr(dy)} then ${Num.toStr(dx2)},${Num.toStr(dy2)}")
        )
        (list_literal
          (apply_uc
            (uc "Scroll")
            (lc "amount")
          )
          (apply_uc
            (uc "Click")
            (tuple_literal
              (lc "x")
              (lc "y")
            )
          )
          (unary_double_dot <unary_op>)
        )
        (lc "remaining")
        (binop_thick_arrow
          (malformed)
          (str_literal_big "scroll ${Num.toStr(amount)} then click at ${Num.toStr(x)},${Num.toStr(y)}")
        )
      )
    )
)
  (branch2     (binop_thick_arrow
      (underscore)
      (str_literal_big "other event pattern")
    )
))
~~~
# FORMATTED
~~~roc
match events
	[] => 
		"no events"
		[Click((x, y))] => "single click at (${Num.toStr(x)}, ${Num.toStr(y)})"
		[KeyPress(key), ..as ]
		rest
		] => "key ${key} pressed, ${Num.toStr(List.len(rest))} more events"
		[Move((dx, dy)), Move((dx2, dy2)), ..as ]
		others
		] => "moved ${Num.toStr(dx)},${Num.toStr(dy)} then ${Num.toStr(dx2)},${Num.toStr(dy2)}"
		[Scroll(amount), Click((x, y)), ..as ]
		remaining
		] => "scroll ${Num.toStr(amount)} then click at ${Num.toStr(x)},${Num.toStr(y)}"
	_ => "other event pattern"
~~~
# EXPECTED
UNDEFINED VARIABLE - complex_list_tags.md:1:7:1:13
UNDEFINED VARIABLE - complex_list_tags.md:3:42:3:51
UNDEFINED VARIABLE - complex_list_tags.md:3:59:3:68
UNDEFINED VARIABLE - complex_list_tags.md:4:59:4:68
UNDEFINED VARIABLE - complex_list_tags.md:4:69:4:77
UNDEFINED VARIABLE - complex_list_tags.md:5:62:5:71
UNDEFINED VARIABLE - complex_list_tags.md:5:79:5:88
UNDEFINED VARIABLE - complex_list_tags.md:5:101:5:110
UNDEFINED VARIABLE - complex_list_tags.md:5:119:5:128
UNUSED VARIABLE - complex_list_tags.md:1:1:1:1
UNDEFINED VARIABLE - complex_list_tags.md:6:65:6:74
UNDEFINED VARIABLE - complex_list_tags.md:6:100:6:109
UNDEFINED VARIABLE - complex_list_tags.md:6:116:6:125
UNUSED VARIABLE - complex_list_tags.md:1:1:1:1
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**complex_list_tags.md:4:24:4:27:**
```roc
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr(List.len(rest))} more events"
```
                       ^^^


**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**complex_list_tags.md:4:5:4:27:**
```roc
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr(List.len(rest))} more events"
```
    ^^^^^^^^^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **] ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**complex_list_tags.md:4:31:4:33:**
```roc
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr(List.len(rest))} more events"
```
                              ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**complex_list_tags.md:5:39:5:42:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr(dx)},${Num.toStr(dy)} then ${Num.toStr(dx2)},${Num.toStr(dy2)}"
```
                                      ^^^


**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**complex_list_tags.md:5:5:5:42:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr(dx)},${Num.toStr(dy)} then ${Num.toStr(dx2)},${Num.toStr(dy2)}"
```
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **] ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**complex_list_tags.md:5:48:5:50:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr(dx)},${Num.toStr(dy)} then ${Num.toStr(dx2)},${Num.toStr(dy2)}"
```
                                               ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**complex_list_tags.md:6:38:6:41:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr(amount)} then click at ${Num.toStr(x)},${Num.toStr(y)}"
```
                                     ^^^


**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**complex_list_tags.md:6:5:6:41:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr(amount)} then click at ${Num.toStr(x)},${Num.toStr(y)}"
```
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **] ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**complex_list_tags.md:6:50:6:52:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr(amount)} then click at ${Num.toStr(x)},${Num.toStr(y)}"
```
                                                 ^^


**UNDEFINED VARIABLE**
Nothing is named **events** in this scope.
Is there an **import** or **exposing** missing up-top?

**complex_list_tags.md:1:7:1:13:**
```roc
match events {
```
      ^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**complex_list_tags.md:2:5:6:130:**
```roc
    [] => "no events"
    [Click(x, y)] => "single click at (${Num.toStr(x)}, ${Num.toStr(y)})"
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr(List.len(rest))} more events"
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr(dx)},${Num.toStr(dy)} then ${Num.toStr(dx2)},${Num.toStr(dy2)}"
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr(amount)} then click at ${Num.toStr(x)},${Num.toStr(y)}"
```


# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
; Total type variables: 60
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
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 _)
(var #35 _)
(var #36 _)
(var #37 _)
(var #38 _)
(var #39 _)
(var #40 _)
(var #41 _)
(var #42 _)
(var #43 _)
(var #44 _)
(var #45 _)
(var #46 _)
(var #47 _)
(var #48 _)
(var #49 _)
(var #50 _)
(var #51 _)
(var #52 _)
(var #53 _)
(var #54 _)
(var #55 _)
(var #56 _)
(var #57 Str)
(var #58 _)
(var #59 _)
~~~
# TYPES
~~~roc
~~~
