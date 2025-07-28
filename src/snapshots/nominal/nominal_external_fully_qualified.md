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
# EXPECTED
MODULE NOT FOUND - nominal_external_fully_qualified.md:3:1:3:22
UNDECLARED TYPE - nominal_external_fully_qualified.md:8:23:8:36
UNDECLARED TYPE - nominal_external_fully_qualified.md:9:23:9:36
UNUSED VARIABLE - nominal_external_fully_qualified.md:9:41:9:45
# PROBLEMS
**MODULE NOT FOUND**
The module `MyResultModule` was not found in this Roc project.

You're attempting to use this module here:
**nominal_external_fully_qualified.md:3:1:3:22:**
```roc
import MyResultModule
```
^^^^^^^^^^^^^^^^^^^^^


**UNDECLARED TYPE**
The type _MyResultType_ is not declared in this scope.

This type is referenced here:
**nominal_external_fully_qualified.md:8:23:8:36:**
```roc
        MyResultModule.MyResultType.Ok(value) => value
```
                      ^^^^^^^^^^^^^


**UNDECLARED TYPE**
The type _MyResultType_ is not declared in this scope.

This type is referenced here:
**nominal_external_fully_qualified.md:9:23:9:36:**
```roc
        MyResultModule.MyResultType.Err(code) => "Error: $(code.toStr())"
```
                      ^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable `code` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_code` to suppress this warning.
The unused variable is declared here:
**nominal_external_fully_qualified.md:9:41:9:45:**
```roc
        MyResultModule.MyResultType.Err(code) => "Error: $(code.toStr())"
```
                                        ^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:21),CloseSquare(1:21-1:22),
KwImport(3:1-3:7),UpperIdent(3:8-3:22),
LowerIdent(5:1-5:13),OpColon(5:14-5:15),UpperIdent(5:16-5:30),NoSpaceDotUpperIdent(5:30-5:43),NoSpaceOpenRound(5:43-5:44),UpperIdent(5:44-5:47),Comma(5:47-5:48),UpperIdent(5:49-5:52),CloseRound(5:52-5:53),OpArrow(5:54-5:56),UpperIdent(5:57-5:60),
LowerIdent(6:1-6:13),OpAssign(6:14-6:15),OpBar(6:16-6:17),LowerIdent(6:17-6:23),OpBar(6:23-6:24),OpenCurly(6:25-6:26),
KwMatch(7:5-7:10),LowerIdent(7:11-7:17),OpenCurly(7:18-7:19),
UpperIdent(8:9-8:23),NoSpaceDotUpperIdent(8:23-8:36),NoSpaceDotUpperIdent(8:36-8:39),NoSpaceOpenRound(8:39-8:40),LowerIdent(8:40-8:45),CloseRound(8:45-8:46),OpFatArrow(8:47-8:49),LowerIdent(8:50-8:55),
UpperIdent(9:9-9:23),NoSpaceDotUpperIdent(9:23-9:36),NoSpaceDotUpperIdent(9:36-9:40),NoSpaceOpenRound(9:40-9:41),LowerIdent(9:41-9:45),CloseRound(9:45-9:46),OpFatArrow(9:47-9:49),StringStart(9:50-9:51),StringPart(9:51-9:73),StringEnd(9:73-9:74),
CloseCurly(10:5-10:6),
CloseCurly(11:1-11:2),EndOfFile(11:2-11:2),
~~~
# PARSE
~~~clojure
(file @1.1-11.2
	(module @1.1-1.22
		(exposes @1.8-1.22
			(exposed-lower-ident @1.9-1.21
				(text "handleResult"))))
	(statements
		(s-import @3.1-3.22 (raw "MyResultModule"))
		(s-type-anno @5.1-5.60 (name "handleResult")
			(ty-fn @5.16-5.60
				(ty-apply @5.16-5.53
					(ty @5.16-5.43 (name "MyResultModule.MyResultType"))
					(ty @5.44-5.47 (name "Str"))
					(ty @5.49-5.52 (name "I32")))
				(ty @5.57-5.60 (name "Str"))))
		(s-decl @6.1-11.2
			(p-ident @6.1-6.13 (raw "handleResult"))
			(e-lambda @6.16-11.2
				(args
					(p-ident @6.17-6.23 (raw "result")))
				(e-block @6.25-11.2
					(statements
						(e-match
							(e-ident @7.11-7.17 (raw "result"))
							(branches
								(branch @8.9-8.55
									(p-tag @8.9-8.46 (raw ".Ok")
										(p-ident @8.40-8.45 (raw "value")))
									(e-ident @8.50-8.55 (raw "value")))
								(branch @9.9-9.74
									(p-tag @9.9-9.46 (raw ".Err")
										(p-ident @9.41-9.45 (raw "code")))
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
		MyResultModule.MyResultType.Ok(value) => value
		MyResultModule.MyResultType.Err(code) => "Error: $(code.toStr())"
	}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.13 (ident "handleResult"))
		(e-closure @6.16-11.2
			(captures
				(capture @8.40-8.45 (ident "value")))
			(e-lambda @6.16-11.2
				(args
					(p-assign @6.17-6.23 (ident "result")))
				(e-block @6.25-11.2
					(e-match @7.5-10.6
						(match @7.5-10.6
							(cond
								(e-lookup-local @7.11-7.17
									(p-assign @6.17-6.23 (ident "result"))))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-runtime-error @8.23-8.36 (tag "undeclared_type"))))
									(value
										(e-lookup-local @8.50-8.55
											(p-assign @8.40-8.45 (ident "value")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-runtime-error @9.23-9.36 (tag "undeclared_type"))))
									(value
										(e-string @9.50-9.74
											(e-literal @9.51-9.73 (string "Error: $(code.toStr())")))))))))))
		(annotation @6.1-6.13
			(declared-type
				(ty-fn @5.16-5.60 (effectful false)
					(ty-apply-external @5.16-5.53
						(module-idx "0")
						(target-node-idx "0"))
					(ty @5.57-5.60 (name "Str"))))))
	(s-import @3.1-3.22 (module "MyResultModule")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.13 (type "Error -> Str")))
	(expressions
		(expr @6.16-11.2 (type "Error -> Str"))))
~~~
