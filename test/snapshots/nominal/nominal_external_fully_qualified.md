# META
~~~ini
description=Example of external nominal tag union fully qualified name
type=snippet
~~~
# SOURCE
~~~roc
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
MODULE NOT FOUND - nominal_external_fully_qualified.md:1:1:1:22
UNUSED VARIABLE - nominal_external_fully_qualified.md:7:41:7:45
# PROBLEMS
**MODULE NOT FOUND**
The module `MyResultModule` was not found in this Roc project.

You're attempting to use this module here:
**nominal_external_fully_qualified.md:1:1:1:22:**
```roc
import MyResultModule
```
^^^^^^^^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable `code` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_code` to suppress this warning.
The unused variable is declared here:
**nominal_external_fully_qualified.md:7:41:7:45:**
```roc
        MyResultModule.MyResultType.Err(code) => "Error: $(code.toStr())"
```
                                        ^^^^


# TOKENS
~~~zig
KwImport(1:1-1:7),UpperIdent(1:8-1:22),
LowerIdent(3:1-3:13),OpColon(3:14-3:15),UpperIdent(3:16-3:30),NoSpaceDotUpperIdent(3:30-3:43),NoSpaceOpenRound(3:43-3:44),UpperIdent(3:44-3:47),Comma(3:47-3:48),UpperIdent(3:49-3:52),CloseRound(3:52-3:53),OpArrow(3:54-3:56),UpperIdent(3:57-3:60),
LowerIdent(4:1-4:13),OpAssign(4:14-4:15),OpBar(4:16-4:17),LowerIdent(4:17-4:23),OpBar(4:23-4:24),OpenCurly(4:25-4:26),
KwMatch(5:5-5:10),LowerIdent(5:11-5:17),OpenCurly(5:18-5:19),
UpperIdent(6:9-6:23),NoSpaceDotUpperIdent(6:23-6:36),NoSpaceDotUpperIdent(6:36-6:39),NoSpaceOpenRound(6:39-6:40),LowerIdent(6:40-6:45),CloseRound(6:45-6:46),OpFatArrow(6:47-6:49),LowerIdent(6:50-6:55),
UpperIdent(7:9-7:23),NoSpaceDotUpperIdent(7:23-7:36),NoSpaceDotUpperIdent(7:36-7:40),NoSpaceOpenRound(7:40-7:41),LowerIdent(7:41-7:45),CloseRound(7:45-7:46),OpFatArrow(7:47-7:49),StringStart(7:50-7:51),StringPart(7:51-7:73),StringEnd(7:73-7:74),
CloseCurly(8:5-8:6),
CloseCurly(9:1-9:2),
EndOfFile(10:1-10:1),
~~~
# PARSE
~~~clojure
(file @1.1-9.2
	(type-module @1.1-1.7)
	(statements
		(s-import @1.1-1.22 (raw "MyResultModule"))
		(s-type-anno @3.1-3.60 (name "handleResult")
			(ty-fn @3.16-3.60
				(ty-apply @3.16-3.53
					(ty @3.16-3.43 (name "MyResultModule.MyResultType"))
					(ty @3.44-3.47 (name "Str"))
					(ty @3.49-3.52 (name "I32")))
				(ty @3.57-3.60 (name "Str"))))
		(s-decl @4.1-9.2
			(p-ident @4.1-4.13 (raw "handleResult"))
			(e-lambda @4.16-9.2
				(args
					(p-ident @4.17-4.23 (raw "result")))
				(e-block @4.25-9.2
					(statements
						(e-match
							(e-ident @5.11-5.17 (raw "result"))
							(branches
								(branch @6.9-6.55
									(p-tag @6.9-6.46 (raw ".Ok")
										(p-ident @6.40-6.45 (raw "value")))
									(e-ident @6.50-6.55 (raw "value")))
								(branch @7.9-7.74
									(p-tag @7.9-7.46 (raw ".Err")
										(p-ident @7.41-7.45 (raw "code")))
									(e-string @7.50-7.74
										(e-string-part @7.51-7.73 (raw "Error: $(code.toStr())"))))))))))))
~~~
# FORMATTED
~~~roc
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
		(p-assign @4.1-4.13 (ident "handleResult"))
		(e-closure @4.16-9.2
			(captures
				(capture @6.40-6.45 (ident "value")))
			(e-lambda @4.16-9.2
				(args
					(p-assign @4.17-4.23 (ident "result")))
				(e-block @4.25-9.2
					(e-match @5.5-8.6
						(match @5.5-8.6
							(cond
								(e-lookup-local @5.11-5.17
									(p-assign @4.17-4.23 (ident "result"))))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-nominal-external @6.9-6.46 (module-idx "0") (target-node-idx "0")
												(p-applied-tag @6.9-6.46))))
									(value
										(e-lookup-local @6.50-6.55
											(p-assign @6.40-6.45 (ident "value")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-nominal-external @7.9-7.46 (module-idx "0") (target-node-idx "0")
												(p-applied-tag @7.9-7.46))))
									(value
										(e-string @7.50-7.74
											(e-literal @7.51-7.73 (string "Error: $(code.toStr())")))))))))))
		(annotation @4.1-4.13
			(declared-type
				(ty-fn @3.16-3.60 (effectful false)
					(ty-apply @3.16-3.53 (name "MyResultType") (external (module-idx "0") (target-node-idx "0"))
						(ty-lookup @3.44-3.47 (name "Str") (builtin))
						(ty-lookup @3.49-3.52 (name "I32") (builtin)))
					(ty-lookup @3.57-3.60 (name "Str") (builtin))))))
	(s-import @1.1-1.22 (module "MyResultModule")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.13 (type "Error -> Str")))
	(expressions
		(expr @4.16-9.2 (type "Error -> Str"))))
~~~
