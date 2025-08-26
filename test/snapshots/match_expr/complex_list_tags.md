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
(match <52 branches>)
~~~
# FORMATTED
~~~roc
when events is {
	[]
	=>
	"no events"
	[Click((x, y))]
	=>
	"single click at (${Num.toStr(x)}, ${Num.toStr(y)})"
	[KeyPress(key), ..as]
	rest
	]
	=>
	"key ${key} pressed, ${Num.toStr(List.len(rest))} more events"
	[Move((dx, dy)), Move((dx2, dy2)), ..as]
	others
	]
	=>
	"moved ${Num.toStr(dx)},${Num.toStr(dy)} then ${Num.toStr(dx2)},${Num.toStr(dy2)}"
	[Scroll(amount), Click((x, y)), ..as]
	remaining
	]
	=>
	"scroll ${Num.toStr(amount)} then click at ${Num.toStr(x)},${Num.toStr(y)}"
	_
	=>
	"other event pattern"
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:14

**Parse Error**
at 2:8 to 2:8

**Parse Error**
at 3:19 to 3:19

**Parse Error**
at 4:24 to 4:24

**Parse Error**
at 4:5 to 4:27

**Parse Error**
at 4:31 to 4:31

**Parse Error**
at 4:33 to 4:33

**Parse Error**
at 5:39 to 5:39

**Parse Error**
at 5:5 to 5:42

**Parse Error**
at 5:48 to 5:48

**Parse Error**
at 5:50 to 5:50

**Parse Error**
at 6:38 to 6:38

**Parse Error**
at 6:5 to 6:41

**Parse Error**
at 6:50 to 6:50

**Parse Error**
at 6:52 to 6:52

**Parse Error**
at 7:7 to 7:7

**Parse Error**
at 1:1 to 8:2

**Parse Error**
at 8:2 to 8:2

# CANONICALIZE
~~~clojure
(Expr.dot_num)
~~~
# SOLVED
~~~clojure
(expr :tag dot_num :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
