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
KwMatch(1:1-1:6),LowerIdent(1:7-1:13),OpenCurly(1:14-1:15),
OpenSquare(2:5-2:6),CloseSquare(2:6-2:7),OpFatArrow(2:8-2:10),StringStart(2:11-2:12),StringPart(2:12-2:21),StringEnd(2:21-2:22),
OpenSquare(3:5-3:6),UpperIdent(3:6-3:11),NoSpaceOpenRound(3:11-3:12),LowerIdent(3:12-3:13),Comma(3:13-3:14),LowerIdent(3:15-3:16),CloseRound(3:16-3:17),CloseSquare(3:17-3:18),OpFatArrow(3:19-3:21),StringStart(3:22-3:23),StringPart(3:23-3:40),OpenStringInterpolation(3:40-3:42),UpperIdent(3:42-3:45),NoSpaceDotLowerIdent(3:45-3:51),NoSpaceOpenRound(3:51-3:52),LowerIdent(3:52-3:53),CloseRound(3:53-3:54),CloseStringInterpolation(3:54-3:55),StringPart(3:55-3:57),OpenStringInterpolation(3:57-3:59),UpperIdent(3:59-3:62),NoSpaceDotLowerIdent(3:62-3:68),NoSpaceOpenRound(3:68-3:69),LowerIdent(3:69-3:70),CloseRound(3:70-3:71),CloseStringInterpolation(3:71-3:72),StringPart(3:72-3:73),StringEnd(3:73-3:74),
OpenSquare(4:5-4:6),UpperIdent(4:6-4:14),NoSpaceOpenRound(4:14-4:15),LowerIdent(4:15-4:18),CloseRound(4:18-4:19),Comma(4:19-4:20),DoubleDot(4:21-4:23),KwAs(4:24-4:26),LowerIdent(4:27-4:31),CloseSquare(4:31-4:32),OpFatArrow(4:33-4:35),StringStart(4:36-4:37),StringPart(4:37-4:41),OpenStringInterpolation(4:41-4:43),LowerIdent(4:43-4:46),CloseStringInterpolation(4:46-4:47),StringPart(4:47-4:57),OpenStringInterpolation(4:57-4:59),UpperIdent(4:59-4:62),NoSpaceDotLowerIdent(4:62-4:68),NoSpaceOpenRound(4:68-4:69),UpperIdent(4:69-4:73),NoSpaceDotLowerIdent(4:73-4:77),NoSpaceOpenRound(4:77-4:78),LowerIdent(4:78-4:82),CloseRound(4:82-4:83),CloseRound(4:83-4:84),CloseStringInterpolation(4:84-4:85),StringPart(4:85-4:97),StringEnd(4:97-4:98),
OpenSquare(5:5-5:6),UpperIdent(5:6-5:10),NoSpaceOpenRound(5:10-5:11),LowerIdent(5:11-5:13),Comma(5:13-5:14),LowerIdent(5:15-5:17),CloseRound(5:17-5:18),Comma(5:18-5:19),UpperIdent(5:20-5:24),NoSpaceOpenRound(5:24-5:25),LowerIdent(5:25-5:28),Comma(5:28-5:29),LowerIdent(5:30-5:33),CloseRound(5:33-5:34),Comma(5:34-5:35),DoubleDot(5:36-5:38),KwAs(5:39-5:41),LowerIdent(5:42-5:48),CloseSquare(5:48-5:49),OpFatArrow(5:50-5:52),StringStart(5:53-5:54),StringPart(5:54-5:60),OpenStringInterpolation(5:60-5:62),UpperIdent(5:62-5:65),NoSpaceDotLowerIdent(5:65-5:71),NoSpaceOpenRound(5:71-5:72),LowerIdent(5:72-5:74),CloseRound(5:74-5:75),CloseStringInterpolation(5:75-5:76),StringPart(5:76-5:77),OpenStringInterpolation(5:77-5:79),UpperIdent(5:79-5:82),NoSpaceDotLowerIdent(5:82-5:88),NoSpaceOpenRound(5:88-5:89),LowerIdent(5:89-5:91),CloseRound(5:91-5:92),CloseStringInterpolation(5:92-5:93),StringPart(5:93-5:99),OpenStringInterpolation(5:99-5:101),UpperIdent(5:101-5:104),NoSpaceDotLowerIdent(5:104-5:110),NoSpaceOpenRound(5:110-5:111),LowerIdent(5:111-5:114),CloseRound(5:114-5:115),CloseStringInterpolation(5:115-5:116),StringPart(5:116-5:117),OpenStringInterpolation(5:117-5:119),UpperIdent(5:119-5:122),NoSpaceDotLowerIdent(5:122-5:128),NoSpaceOpenRound(5:128-5:129),LowerIdent(5:129-5:132),CloseRound(5:132-5:133),CloseStringInterpolation(5:133-5:134),StringPart(5:134-5:134),StringEnd(5:134-5:135),
OpenSquare(6:5-6:6),UpperIdent(6:6-6:12),NoSpaceOpenRound(6:12-6:13),LowerIdent(6:13-6:19),CloseRound(6:19-6:20),Comma(6:20-6:21),UpperIdent(6:22-6:27),NoSpaceOpenRound(6:27-6:28),LowerIdent(6:28-6:29),Comma(6:29-6:30),LowerIdent(6:31-6:32),CloseRound(6:32-6:33),Comma(6:33-6:34),DoubleDot(6:35-6:37),KwAs(6:38-6:40),LowerIdent(6:41-6:50),CloseSquare(6:50-6:51),OpFatArrow(6:52-6:54),StringStart(6:55-6:56),StringPart(6:56-6:63),OpenStringInterpolation(6:63-6:65),UpperIdent(6:65-6:68),NoSpaceDotLowerIdent(6:68-6:74),NoSpaceOpenRound(6:74-6:75),LowerIdent(6:75-6:81),CloseRound(6:81-6:82),CloseStringInterpolation(6:82-6:83),StringPart(6:83-6:98),OpenStringInterpolation(6:98-6:100),UpperIdent(6:100-6:103),NoSpaceDotLowerIdent(6:103-6:109),NoSpaceOpenRound(6:109-6:110),LowerIdent(6:110-6:111),CloseRound(6:111-6:112),CloseStringInterpolation(6:112-6:113),StringPart(6:113-6:114),OpenStringInterpolation(6:114-6:116),UpperIdent(6:116-6:119),NoSpaceDotLowerIdent(6:119-6:125),NoSpaceOpenRound(6:125-6:126),LowerIdent(6:126-6:127),CloseRound(6:127-6:128),CloseStringInterpolation(6:128-6:129),StringPart(6:129-6:129),StringEnd(6:129-6:130),
Underscore(7:5-7:6),OpFatArrow(7:7-7:9),StringStart(7:10-7:11),StringPart(7:11-7:30),StringEnd(7:30-7:31),
CloseCurly(8:1-8:2),
EndOfFile(9:1-9:1),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.13 (raw "events"))
	(branches
		(branch @2.5-2.22
			(p-list @2.5-2.7)
			(e-string @2.11-2.22
				(e-string-part @2.12-2.21 (raw "no events"))))
		(branch @3.5-3.74
			(p-list @3.5-3.18
				(p-tag @3.6-3.17 (raw "Click")
					(p-ident @3.12-3.13 (raw "x"))
					(p-ident @3.15-3.16 (raw "y"))))
			(e-string @3.22-3.74
				(e-string-part @3.23-3.40 (raw "single click at ("))
				(e-apply @3.42-3.54
					(e-ident @3.42-3.51 (raw "Num.toStr"))
					(e-ident @3.52-3.53 (raw "x")))
				(e-string-part @3.55-3.57 (raw ", "))
				(e-apply @3.59-3.71
					(e-ident @3.59-3.68 (raw "Num.toStr"))
					(e-ident @3.69-3.70 (raw "y")))
				(e-string-part @3.72-3.73 (raw ")"))))
		(branch @4.5-4.98
			(p-list @4.5-4.32
				(p-tag @4.6-4.19 (raw "KeyPress")
					(p-ident @4.15-4.18 (raw "key")))
				(p-list-rest @4.21-4.31 (name "rest")))
			(e-string @4.36-4.98
				(e-string-part @4.37-4.41 (raw "key "))
				(e-ident @4.43-4.46 (raw "key"))
				(e-string-part @4.47-4.57 (raw " pressed, "))
				(e-apply @4.59-4.84
					(e-ident @4.59-4.68 (raw "Num.toStr"))
					(e-apply @4.69-4.83
						(e-ident @4.69-4.77 (raw "List.len"))
						(e-ident @4.78-4.82 (raw "rest"))))
				(e-string-part @4.85-4.97 (raw " more events"))))
		(branch @5.5-5.135
			(p-list @5.5-5.49
				(p-tag @5.6-5.18 (raw "Move")
					(p-ident @5.11-5.13 (raw "dx"))
					(p-ident @5.15-5.17 (raw "dy")))
				(p-tag @5.20-5.34 (raw "Move")
					(p-ident @5.25-5.28 (raw "dx2"))
					(p-ident @5.30-5.33 (raw "dy2")))
				(p-list-rest @5.36-5.48 (name "others")))
			(e-string @5.53-5.135
				(e-string-part @5.54-5.60 (raw "moved "))
				(e-apply @5.62-5.75
					(e-ident @5.62-5.71 (raw "Num.toStr"))
					(e-ident @5.72-5.74 (raw "dx")))
				(e-string-part @5.76-5.77 (raw ","))
				(e-apply @5.79-5.92
					(e-ident @5.79-5.88 (raw "Num.toStr"))
					(e-ident @5.89-5.91 (raw "dy")))
				(e-string-part @5.93-5.99 (raw " then "))
				(e-apply @5.101-5.115
					(e-ident @5.101-5.110 (raw "Num.toStr"))
					(e-ident @5.111-5.114 (raw "dx2")))
				(e-string-part @5.116-5.117 (raw ","))
				(e-apply @5.119-5.133
					(e-ident @5.119-5.128 (raw "Num.toStr"))
					(e-ident @5.129-5.132 (raw "dy2")))
				(e-string-part @5.134-5.134 (raw ""))))
		(branch @6.5-6.130
			(p-list @6.5-6.51
				(p-tag @6.6-6.20 (raw "Scroll")
					(p-ident @6.13-6.19 (raw "amount")))
				(p-tag @6.22-6.33 (raw "Click")
					(p-ident @6.28-6.29 (raw "x"))
					(p-ident @6.31-6.32 (raw "y")))
				(p-list-rest @6.35-6.50 (name "remaining")))
			(e-string @6.55-6.130
				(e-string-part @6.56-6.63 (raw "scroll "))
				(e-apply @6.65-6.82
					(e-ident @6.65-6.74 (raw "Num.toStr"))
					(e-ident @6.75-6.81 (raw "amount")))
				(e-string-part @6.83-6.98 (raw " then click at "))
				(e-apply @6.100-6.112
					(e-ident @6.100-6.109 (raw "Num.toStr"))
					(e-ident @6.110-6.111 (raw "x")))
				(e-string-part @6.113-6.114 (raw ","))
				(e-apply @6.116-6.128
					(e-ident @6.116-6.125 (raw "Num.toStr"))
					(e-ident @6.126-6.127 (raw "y")))
				(e-string-part @6.129-6.129 (raw ""))))
		(branch @7.5-7.31
			(p-underscore)
			(e-string @7.10-7.31
				(e-string-part @7.11-7.30 (raw "other event pattern"))))))
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
(e-match @1.1-8.2
	(match @1.1-8.2
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @2.5-2.7
							(patterns))))
				(value
					(e-string @2.11-2.22
						(e-literal @2.12-2.21 (string "no events")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @3.5-3.18
							(patterns
								(p-applied-tag @3.6-3.17)))))
				(value
					(e-string @3.22-3.74
						(e-literal @3.23-3.40 (string "single click at ("))
						(e-call @3.42-3.54
							(e-lookup-local @3.52-3.53
								(p-assign @3.12-3.13 (ident "x"))))
						(e-literal @3.55-3.57 (string ", "))
						(e-call @3.59-3.71
							(e-lookup-local @3.69-3.70
								(p-assign @3.15-3.16 (ident "y"))))
						(e-literal @3.72-3.73 (string ")")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @4.5-4.32
							(patterns
								(p-applied-tag @4.6-4.19))
							(rest-at (index 1)
								(p-assign @1.1-1.1 (ident "rest"))))))
				(value
					(e-string @4.36-4.98
						(e-literal @4.37-4.41 (string "key "))
						(e-lookup-local @4.43-4.46
							(p-assign @4.15-4.18 (ident "key")))
						(e-literal @4.47-4.57 (string " pressed, "))
						(e-call @4.59-4.84
							(e-call @4.69-4.83
								(e-lookup-local @4.78-4.82
									(p-assign @1.1-1.1 (ident "rest")))))
						(e-literal @4.85-4.97 (string " more events")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @5.5-5.49
							(patterns
								(p-applied-tag @5.6-5.18)
								(p-applied-tag @5.20-5.34))
							(rest-at (index 2)
								(p-assign @1.1-1.1 (ident "others"))))))
				(value
					(e-string @5.53-5.135
						(e-literal @5.54-5.60 (string "moved "))
						(e-call @5.62-5.75
							(e-lookup-local @5.72-5.74
								(p-assign @5.11-5.13 (ident "dx"))))
						(e-literal @5.76-5.77 (string ","))
						(e-call @5.79-5.92
							(e-lookup-local @5.89-5.91
								(p-assign @5.15-5.17 (ident "dy"))))
						(e-literal @5.93-5.99 (string " then "))
						(e-call @5.101-5.115
							(e-lookup-local @5.111-5.114
								(p-assign @5.25-5.28 (ident "dx2"))))
						(e-literal @5.116-5.117 (string ","))
						(e-call @5.119-5.133
							(e-lookup-local @5.129-5.132
								(p-assign @5.30-5.33 (ident "dy2"))))
						(e-literal @5.134-5.134 (string "")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @6.5-6.51
							(patterns
								(p-applied-tag @6.6-6.20)
								(p-applied-tag @6.22-6.33))
							(rest-at (index 2)
								(p-assign @1.1-1.1 (ident "remaining"))))))
				(value
					(e-string @6.55-6.130
						(e-literal @6.56-6.63 (string "scroll "))
						(e-call @6.65-6.82
							(e-lookup-local @6.75-6.81
								(p-assign @6.13-6.19 (ident "amount"))))
						(e-literal @6.83-6.98 (string " then click at "))
						(e-call @6.100-6.112
							(e-lookup-local @6.110-6.111
								(p-assign @6.28-6.29 (ident "x"))))
						(e-literal @6.113-6.114 (string ","))
						(e-call @6.116-6.128
							(e-lookup-local @6.126-6.127
								(p-assign @6.31-6.32 (ident "y"))))
						(e-literal @6.129-6.129 (string "")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-underscore @7.5-7.6)))
				(value
					(e-string @7.10-7.31
						(e-literal @7.11-7.30 (string "other event pattern"))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-8.2 (type "Error"))
~~~
