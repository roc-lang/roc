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
(malformed malformed:expr_unexpected_token)
~~~
# FORMATTED
~~~roc
] 
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 4:24 to 4:27

**Parse Error**
at 4:5 to 4:27

**Parse Error**
at 4:5 to 4:31

**Parse Error**
at 4:31 to 4:33

**Unsupported Node**
at 4:31 to 4:33

# CANONICALIZE
~~~clojure
(Stmt.malformed)
~~~
# SOLVED
~~~clojure
; No expression to type check
~~~
# TYPES
~~~roc
# No expression found
~~~
