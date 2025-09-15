# META
~~~ini
description=Record destructuring with field renaming
type=expr
~~~
# SOURCE
~~~roc
match person {
    { name: userName, age: userAge } => "User ${userName} is ${userAge.to_str()} years old"
}
~~~
# EXPECTED
UNDEFINED VARIABLE - pattern_destructure_rename.md:1:7:1:13
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `person` in this scope.
Is there an `import` or `exposing` missing up-top?

**pattern_destructure_rename.md:1:7:1:13:**
```roc
match person {
```
      ^^^^^^


# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:13),OpenCurly(1:14-1:15),
OpenCurly(2:5-2:6),LowerIdent(2:7-2:11),OpColon(2:11-2:12),LowerIdent(2:13-2:21),Comma(2:21-2:22),LowerIdent(2:23-2:26),OpColon(2:26-2:27),LowerIdent(2:28-2:35),CloseCurly(2:36-2:37),OpFatArrow(2:38-2:40),StringStart(2:41-2:42),StringPart(2:42-2:47),OpenStringInterpolation(2:47-2:49),LowerIdent(2:49-2:57),CloseStringInterpolation(2:57-2:58),StringPart(2:58-2:62),OpenStringInterpolation(2:62-2:64),LowerIdent(2:64-2:71),NoSpaceDotLowerIdent(2:71-2:78),NoSpaceOpenRound(2:78-2:79),CloseRound(2:79-2:80),CloseStringInterpolation(2:80-2:81),StringPart(2:81-2:91),StringEnd(2:91-2:92),
CloseCurly(3:1-3:2),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.13 (raw "person"))
	(branches
		(branch @2.5-2.92
			(p-record @2.5-2.37
				(field @2.7-2.21 (name "name") (rest false)
					(p-ident @2.13-2.21 (raw "userName")))
				(field @2.23-2.35 (name "age") (rest false)
					(p-ident @2.28-2.35 (raw "userAge"))))
			(e-string @2.41-2.92
				(e-string-part @2.42-2.47 (raw "User "))
				(e-ident @2.49-2.57 (raw "userName"))
				(e-string-part @2.58-2.62 (raw " is "))
				(e-field-access @2.64-2.80
					(e-ident @2.64-2.71 (raw "userAge"))
					(e-apply @2.71-2.80
						(e-ident @2.71-2.78 (raw "to_str"))))
				(e-string-part @2.81-2.91 (raw " years old"))))))
~~~
# FORMATTED
~~~roc
match person {
	{ name: userName, age: userAge } => "User ${userName} is ${userAge.to_str()} years old"
}
~~~
# CANONICALIZE
~~~clojure
(e-match @1.1-3.2
	(match @1.1-3.2
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-record-destructure @2.5-2.37
							(destructs
								(record-destruct @2.7-2.21 (label "name") (ident "name")
									(sub-pattern
										(p-assign @2.13-2.21 (ident "userName"))))
								(record-destruct @2.23-2.35 (label "age") (ident "age")
									(sub-pattern
										(p-assign @2.28-2.35 (ident "userAge"))))))))
				(value
					(e-string @2.41-2.92
						(e-literal @2.42-2.47 (string "User "))
						(e-lookup-local @2.49-2.57
							(p-assign @2.13-2.21 (ident "userName")))
						(e-literal @2.58-2.62 (string " is "))
						(e-dot-access @2.64-2.80 (field "to_str")
							(receiver
								(e-lookup-local @2.64-2.71
									(p-assign @2.28-2.35 (ident "userAge"))))
							(args))
						(e-literal @2.81-2.91 (string " years old"))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-3.2 (type "Error"))
~~~
