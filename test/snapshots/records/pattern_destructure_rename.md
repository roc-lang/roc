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
KwMatch,LowerIdent,OpenCurly,
OpenCurly,LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpColon,LowerIdent,CloseCurly,OpFatArrow,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,CloseStringInterpolation,StringPart,StringEnd,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "person"))
	(branches
		(branch
			(p-record
				(field (name "name") (rest false)
					(p-ident (raw "userName")))
				(field (name "age") (rest false)
					(p-ident (raw "userAge"))))
			(e-string
				(e-string-part (raw "User "))
				(e-ident (raw "userName"))
				(e-string-part (raw " is "))
				(e-field-access
					(e-ident (raw "userAge"))
					(e-apply
						(e-ident (raw "to_str"))))
				(e-string-part (raw " years old"))))))
~~~
# FORMATTED
~~~roc
match person {
	{ name: userName, age: userAge } => "User ${userName} is ${userAge.to_str()} years old"
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
						(p-record-destructure
							(destructs
								(record-destruct (label "name") (ident "name")
									(sub-pattern
										(p-assign (ident "userName"))))
								(record-destruct (label "age") (ident "age")
									(sub-pattern
										(p-assign (ident "userAge"))))))))
				(value
					(e-string
						(e-literal (string "User "))
						(e-lookup-local
							(p-assign (ident "userName")))
						(e-literal (string " is "))
						(e-dot-access (field "to_str")
							(receiver
								(e-lookup-local
									(p-assign (ident "userAge"))))
							(args))
						(e-literal (string " years old"))))))))
~~~
# TYPES
~~~clojure
(expr (type "Str"))
~~~
