# META
~~~ini
description=Example of external nominal tag union fully qualified name
type=file
~~~
# SOURCE
~~~roc
module [handleResult]

import MyResultModule

handleResult : MyResultModule.MyResultType(Str, I32) -> Str
handleResult = |result| {
    match result {
        MyResultModule.MyResultType.Ok(value) => value
        MyResultModule.MyResultType.Err(code) => "Error: $(code.toStr())"
    }
}
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **.MyResultType.Ok** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_external_fully_qualified.md:8:23:8:39:**
```roc
        MyResultModule.MyResultType.Ok(value) => value
```
                      ^^^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **.Ok(** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**nominal_external_fully_qualified.md:8:36:8:40:**
```roc
        MyResultModule.MyResultType.Ok(value) => value
```
                                   ^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **=> value** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**nominal_external_fully_qualified.md:8:47:8:55:**
```roc
        MyResultModule.MyResultType.Ok(value) => value
```
                                              ^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.MyResultType.Err** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_external_fully_qualified.md:9:23:9:40:**
```roc
        MyResultModule.MyResultType.Err(code) => "Error: $(code.toStr())"
```
                      ^^^^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **.Err(** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**nominal_external_fully_qualified.md:9:36:9:41:**
```roc
        MyResultModule.MyResultType.Err(code) => "Error: $(code.toStr())"
```
                                   ^^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **=> "** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**nominal_external_fully_qualified.md:9:47:9:51:**
```roc
        MyResultModule.MyResultType.Err(code) => "Error: $(code.toStr())"
```
                                              ^^^^


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

**UNDEFINED VARIABLE**
Nothing is named `value` in this scope.
Is there an `import` or `exposing` missing up-top?

**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

**UNDEFINED VARIABLE**
Nothing is named `value` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

**UNDEFINED VARIABLE**
Nothing is named `code` in this scope.
Is there an `import` or `exposing` missing up-top?

**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:21),CloseSquare(1:21-1:22),Newline(1:1-1:1),
Newline(1:1-1:1),
KwImport(3:1-3:7),UpperIdent(3:8-3:22),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(5:1-5:13),OpColon(5:14-5:15),UpperIdent(5:16-5:30),NoSpaceDotUpperIdent(5:30-5:43),NoSpaceOpenRound(5:43-5:44),UpperIdent(5:44-5:47),Comma(5:47-5:48),UpperIdent(5:49-5:52),CloseRound(5:52-5:53),OpArrow(5:54-5:56),UpperIdent(5:57-5:60),Newline(1:1-1:1),
LowerIdent(6:1-6:13),OpAssign(6:14-6:15),OpBar(6:16-6:17),LowerIdent(6:17-6:23),OpBar(6:23-6:24),OpenCurly(6:25-6:26),Newline(1:1-1:1),
KwMatch(7:5-7:10),LowerIdent(7:11-7:17),OpenCurly(7:18-7:19),Newline(1:1-1:1),
UpperIdent(8:9-8:23),NoSpaceDotUpperIdent(8:23-8:36),NoSpaceDotUpperIdent(8:36-8:39),NoSpaceOpenRound(8:39-8:40),LowerIdent(8:40-8:45),CloseRound(8:45-8:46),OpFatArrow(8:47-8:49),LowerIdent(8:50-8:55),Newline(1:1-1:1),
UpperIdent(9:9-9:23),NoSpaceDotUpperIdent(9:23-9:36),NoSpaceDotUpperIdent(9:36-9:40),NoSpaceOpenRound(9:40-9:41),LowerIdent(9:41-9:45),CloseRound(9:45-9:46),OpFatArrow(9:47-9:49),StringStart(9:50-9:51),StringPart(9:51-9:73),StringEnd(9:73-9:74),Newline(1:1-1:1),
CloseCurly(10:5-10:6),Newline(1:1-1:1),
CloseCurly(11:1-11:2),EndOfFile(11:2-11:2),
~~~
# PARSE
~~~clojure
(file @1.1-11.2
	(module @1.1-1.22
		(exposes @1.8-1.22
			(exposed-lower-ident (text "handleResult"))))
	(statements
		(s-import @3.1-3.22 (module "MyResultModule"))
		(s-type-anno @5.1-6.13 (name "handleResult")
			(ty-fn @5.16-5.60
				(ty-apply @5.16-5.53
					(ty-mod (module "MyResultType") (name "MyResultModule"))
					(ty (name "Str"))
					(ty (name "I32")))
				(ty (name "Str"))))
		(s-decl @6.1-11.2
			(p-ident @6.1-6.13 (raw "handleResult"))
			(e-lambda @6.16-11.2
				(args
					(p-ident @6.17-6.23 (raw "result")))
				(e-block @6.25-11.2
					(statements
						(e-match
							(e-ident @7.11-7.17 (qaul "") (raw "result"))
							(branches
								(branch @8.9-8.39
									(p-tag @8.9-8.23 (raw "MyResultModule"))
									(e-malformed @8.23-8.39 (reason "expr_unexpected_token")))
								(branch @8.36-8.49
									(p-malformed @8.36-8.40 (tag "pattern_unexpected_token"))
									(e-tuple @8.39-8.46
										(e-ident @8.40-8.45 (qaul "") (raw "value"))))
								(branch @8.47-9.23
									(p-malformed @8.47-8.55 (tag "pattern_unexpected_token"))
									(e-ident @8.50-8.55 (qaul "") (raw "value")))
								(branch @9.9-9.40
									(p-tag @9.9-9.23 (raw "MyResultModule"))
									(e-malformed @9.23-9.40 (reason "expr_unexpected_token")))
								(branch @9.36-9.49
									(p-malformed @9.36-9.41 (tag "pattern_unexpected_token"))
									(e-tuple @9.40-9.46
										(e-ident @9.41-9.45 (qaul "") (raw "code"))))
								(branch @1.1-1.1
									(p-malformed @9.47-9.51 (tag "pattern_unexpected_token"))
									(e-string @9.50-9.74
										(e-string-part @9.51-9.73 (raw "Error: $(code.toStr())"))))))))))))
~~~
# FORMATTED
~~~roc
module [handleResult]

import MyResultModule

handleResult : MyResultModule.MyResultType(Str, I32) -> Str
handleResult = |result| {
	match result {
		MyResultModule => 		 => (value)		 => value
		MyResultModule => 		 => (code)		 => "Error: $(code.toStr())"
	}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.13 (ident "handleResult"))
		(e-lambda @6.16-11.2
			(args
				(p-assign @6.17-6.23 (ident "result")))
			(e-block @6.25-11.2
				(e-match @7.5-10.6
					(match @7.5-10.6
						(cond
							(e-lookup-local @7.11-7.17
								(pattern @6.17-6.23)))
						(branches
							(branch
								(patterns
									(p-applied-tag @8.9-8.23 (degenerate false)))
								(value
									(e-runtime-error (tag "expr_not_canonicalized"))))
							(branch
								(patterns
									(p-runtime-error @8.36-8.40 (tag "pattern_not_canonicalized") (degenerate false)))
								(value
									(e-runtime-error (tag "ident_not_in_scope"))))
							(branch
								(patterns
									(p-runtime-error @8.47-8.55 (tag "pattern_not_canonicalized") (degenerate false)))
								(value
									(e-runtime-error (tag "ident_not_in_scope"))))
							(branch
								(patterns
									(p-applied-tag @9.9-9.23 (degenerate false)))
								(value
									(e-runtime-error (tag "expr_not_canonicalized"))))
							(branch
								(patterns
									(p-runtime-error @9.36-9.41 (tag "pattern_not_canonicalized") (degenerate false)))
								(value
									(e-runtime-error (tag "ident_not_in_scope"))))
							(branch
								(patterns
									(p-runtime-error @9.47-9.51 (tag "pattern_not_canonicalized") (degenerate false)))
								(value
									(e-string @9.50-9.74
										(e-literal @9.51-9.73 (string "Error: $(code.toStr())"))))))))))
		(annotation @6.1-6.13
			(declared-type
				(ty-fn @5.16-5.60 (effectful false)
					(ty-malformed @5.16-5.53)
					(ty @5.57-5.60 (name "Str"))))))
	(s-import @3.1-3.22 (module "MyResultModule")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.13 (type "Error -> Error")))
	(expressions
		(expr @6.16-11.2 (type "Error -> Error"))))
~~~
