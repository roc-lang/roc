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
**UNDEFINED VARIABLE**
Nothing is named `events` in this scope.
Is there an `import` or `exposing` missing up-top?

**complex_list_tags.md:1:7:1:13:**
```roc
match events {
```
      ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `toStr` in this scope.
Is there an `import` or `exposing` missing up-top?

**complex_list_tags.md:3:42:3:51:**
```roc
    [Click(x, y)] => "single click at (${Num.toStr(x)}, ${Num.toStr(y)})"
```
                                         ^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `toStr` in this scope.
Is there an `import` or `exposing` missing up-top?

**complex_list_tags.md:3:59:3:68:**
```roc
    [Click(x, y)] => "single click at (${Num.toStr(x)}, ${Num.toStr(y)})"
```
                                                          ^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `toStr` in this scope.
Is there an `import` or `exposing` missing up-top?

**complex_list_tags.md:4:59:4:68:**
```roc
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr(List.len(rest))} more events"
```
                                                          ^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `len` in this scope.
Is there an `import` or `exposing` missing up-top?

**complex_list_tags.md:4:69:4:77:**
```roc
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr(List.len(rest))} more events"
```
                                                                    ^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `toStr` in this scope.
Is there an `import` or `exposing` missing up-top?

**complex_list_tags.md:5:62:5:71:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr(dx)},${Num.toStr(dy)} then ${Num.toStr(dx2)},${Num.toStr(dy2)}"
```
                                                             ^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `toStr` in this scope.
Is there an `import` or `exposing` missing up-top?

**complex_list_tags.md:5:79:5:88:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr(dx)},${Num.toStr(dy)} then ${Num.toStr(dx2)},${Num.toStr(dy2)}"
```
                                                                              ^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `toStr` in this scope.
Is there an `import` or `exposing` missing up-top?

**complex_list_tags.md:5:101:5:110:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr(dx)},${Num.toStr(dy)} then ${Num.toStr(dx2)},${Num.toStr(dy2)}"
```
                                                                                                    ^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `toStr` in this scope.
Is there an `import` or `exposing` missing up-top?

**complex_list_tags.md:5:119:5:128:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr(dx)},${Num.toStr(dy)} then ${Num.toStr(dx2)},${Num.toStr(dy2)}"
```
                                                                                                                      ^^^^^^^^^


**UNUSED VARIABLE**
Variable `others` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_others` to suppress this warning.
The unused variable is declared here:
**complex_list_tags.md:1:1:1:1:**
```roc
match events {
```
^


**UNDEFINED VARIABLE**
Nothing is named `toStr` in this scope.
Is there an `import` or `exposing` missing up-top?

**complex_list_tags.md:6:65:6:74:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr(amount)} then click at ${Num.toStr(x)},${Num.toStr(y)}"
```
                                                                ^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `toStr` in this scope.
Is there an `import` or `exposing` missing up-top?

**complex_list_tags.md:6:100:6:109:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr(amount)} then click at ${Num.toStr(x)},${Num.toStr(y)}"
```
                                                                                                   ^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `toStr` in this scope.
Is there an `import` or `exposing` missing up-top?

**complex_list_tags.md:6:116:6:125:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr(amount)} then click at ${Num.toStr(x)},${Num.toStr(y)}"
```
                                                                                                                   ^^^^^^^^^


**UNUSED VARIABLE**
Variable `remaining` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_remaining` to suppress this warning.
The unused variable is declared here:
**complex_list_tags.md:1:1:1:1:**
```roc
match events {
```
^


# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenSquare,CloseSquare,OpFatArrow,StringStart,StringPart,StringEnd,
OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,CloseSquare,OpFatArrow,StringStart,StringPart,OpenStringInterpolation,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseStringInterpolation,StringPart,OpenStringInterpolation,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseStringInterpolation,StringPart,StringEnd,
OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,DoubleDot,KwAs,LowerIdent,CloseSquare,OpFatArrow,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,CloseStringInterpolation,StringPart,StringEnd,
OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,DoubleDot,KwAs,LowerIdent,CloseSquare,OpFatArrow,StringStart,StringPart,OpenStringInterpolation,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseStringInterpolation,StringPart,OpenStringInterpolation,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseStringInterpolation,StringPart,OpenStringInterpolation,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseStringInterpolation,StringPart,OpenStringInterpolation,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseStringInterpolation,StringPart,StringEnd,
OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,DoubleDot,KwAs,LowerIdent,CloseSquare,OpFatArrow,StringStart,StringPart,OpenStringInterpolation,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseStringInterpolation,StringPart,OpenStringInterpolation,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseStringInterpolation,StringPart,OpenStringInterpolation,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseStringInterpolation,StringPart,StringEnd,
Underscore,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "events"))
	(branches
		(branch
			(p-list)
			(e-string
				(e-string-part (raw "no events"))))
		(branch
			(p-list
				(p-tag (raw "Click")
					(p-ident (raw "x"))
					(p-ident (raw "y"))))
			(e-string
				(e-string-part (raw "single click at ("))
				(e-apply
					(e-ident (raw "Num.toStr"))
					(e-ident (raw "x")))
				(e-string-part (raw ", "))
				(e-apply
					(e-ident (raw "Num.toStr"))
					(e-ident (raw "y")))
				(e-string-part (raw ")"))))
		(branch
			(p-list
				(p-tag (raw "KeyPress")
					(p-ident (raw "key")))
				(p-list-rest (name "rest")))
			(e-string
				(e-string-part (raw "key "))
				(e-ident (raw "key"))
				(e-string-part (raw " pressed, "))
				(e-apply
					(e-ident (raw "Num.toStr"))
					(e-apply
						(e-ident (raw "List.len"))
						(e-ident (raw "rest"))))
				(e-string-part (raw " more events"))))
		(branch
			(p-list
				(p-tag (raw "Move")
					(p-ident (raw "dx"))
					(p-ident (raw "dy")))
				(p-tag (raw "Move")
					(p-ident (raw "dx2"))
					(p-ident (raw "dy2")))
				(p-list-rest (name "others")))
			(e-string
				(e-string-part (raw "moved "))
				(e-apply
					(e-ident (raw "Num.toStr"))
					(e-ident (raw "dx")))
				(e-string-part (raw ","))
				(e-apply
					(e-ident (raw "Num.toStr"))
					(e-ident (raw "dy")))
				(e-string-part (raw " then "))
				(e-apply
					(e-ident (raw "Num.toStr"))
					(e-ident (raw "dx2")))
				(e-string-part (raw ","))
				(e-apply
					(e-ident (raw "Num.toStr"))
					(e-ident (raw "dy2")))
				(e-string-part (raw ""))))
		(branch
			(p-list
				(p-tag (raw "Scroll")
					(p-ident (raw "amount")))
				(p-tag (raw "Click")
					(p-ident (raw "x"))
					(p-ident (raw "y")))
				(p-list-rest (name "remaining")))
			(e-string
				(e-string-part (raw "scroll "))
				(e-apply
					(e-ident (raw "Num.toStr"))
					(e-ident (raw "amount")))
				(e-string-part (raw " then click at "))
				(e-apply
					(e-ident (raw "Num.toStr"))
					(e-ident (raw "x")))
				(e-string-part (raw ","))
				(e-apply
					(e-ident (raw "Num.toStr"))
					(e-ident (raw "y")))
				(e-string-part (raw ""))))
		(branch
			(p-underscore)
			(e-string
				(e-string-part (raw "other event pattern"))))))
~~~
# FORMATTED
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
# CANONICALIZE
~~~clojure
(e-match
	(match
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns))))
				(value
					(e-string
						(e-literal (string "no events")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-applied-tag)))))
				(value
					(e-string
						(e-literal (string "single click at ("))
						(e-call
							(e-runtime-error (tag "ident_not_in_scope"))
							(e-lookup-local
								(p-assign (ident "x"))))
						(e-literal (string ", "))
						(e-call
							(e-runtime-error (tag "ident_not_in_scope"))
							(e-lookup-local
								(p-assign (ident "y"))))
						(e-literal (string ")")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-applied-tag))
							(rest-at (index 1)
								(p-assign (ident "rest"))))))
				(value
					(e-string
						(e-literal (string "key "))
						(e-lookup-local
							(p-assign (ident "key")))
						(e-literal (string " pressed, "))
						(e-call
							(e-runtime-error (tag "ident_not_in_scope"))
							(e-call
								(e-runtime-error (tag "ident_not_in_scope"))
								(e-lookup-local
									(p-assign (ident "rest")))))
						(e-literal (string " more events")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-applied-tag)
								(p-applied-tag))
							(rest-at (index 2)
								(p-assign (ident "others"))))))
				(value
					(e-string
						(e-literal (string "moved "))
						(e-call
							(e-runtime-error (tag "ident_not_in_scope"))
							(e-lookup-local
								(p-assign (ident "dx"))))
						(e-literal (string ","))
						(e-call
							(e-runtime-error (tag "ident_not_in_scope"))
							(e-lookup-local
								(p-assign (ident "dy"))))
						(e-literal (string " then "))
						(e-call
							(e-runtime-error (tag "ident_not_in_scope"))
							(e-lookup-local
								(p-assign (ident "dx2"))))
						(e-literal (string ","))
						(e-call
							(e-runtime-error (tag "ident_not_in_scope"))
							(e-lookup-local
								(p-assign (ident "dy2"))))
						(e-literal (string "")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-applied-tag)
								(p-applied-tag))
							(rest-at (index 2)
								(p-assign (ident "remaining"))))))
				(value
					(e-string
						(e-literal (string "scroll "))
						(e-call
							(e-runtime-error (tag "ident_not_in_scope"))
							(e-lookup-local
								(p-assign (ident "amount"))))
						(e-literal (string " then click at "))
						(e-call
							(e-runtime-error (tag "ident_not_in_scope"))
							(e-lookup-local
								(p-assign (ident "x"))))
						(e-literal (string ","))
						(e-call
							(e-runtime-error (tag "ident_not_in_scope"))
							(e-lookup-local
								(p-assign (ident "y"))))
						(e-literal (string "")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-underscore)))
				(value
					(e-string
						(e-literal (string "other event pattern"))))))))
~~~
# TYPES
~~~clojure
(expr (type "Str"))
~~~
