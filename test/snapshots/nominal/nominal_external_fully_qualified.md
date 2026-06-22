# META
~~~ini
description=Example of external nominal tag union fully qualified name
type=snippet
~~~
# SOURCE
~~~roc
import MyTryModule

handleTry : MyTryModule.MyTryType(Str, I32) -> Str
handleTry = |result| {
    match result {
        MyTryModule.MyTryType.Ok(value) => value
        MyTryModule.MyTryType.Err(code) => "Error: $(code.toStr())"
    }
}
~~~
# EXPECTED
MODULE NOT FOUND - nominal_external_fully_qualified.md:3:24:3:34
MODULE NOT FOUND - nominal_external_fully_qualified.md:6:20:6:30
MODULE NOT FOUND - nominal_external_fully_qualified.md:7:20:7:30
UNUSED VARIABLE - nominal_external_fully_qualified.md:7:35:7:39
# PROBLEMS
                                                            ┌──────────────────┐
┌─ The type MyTryType is qualified by the module ───────────┤ MODULE NOT FOUND │
│  MyTryModule, but that module was not found in this Roc   └─────────────────┬┘
│  project.                                                                   │
│                                                                             │
│  handleTry : MyTryModule.MyTryType(Str, I32) -> Str                         │
│                         ‾‾‾‾‾‾‾‾‾‾                                          │
└─────────────────────────────────── nominal_external_fully_qualified.md:3:24 ┘

    You're attempting to use this type here:
                                                            ┌──────────────────┐
┌─ The type MyTryType is qualified by the module ───────────┤ MODULE NOT FOUND │
│  MyTryModule, but that module was not found in this Roc   └─────────────────┬┘
│  project.                                                                   │
│                                                                             │
│          MyTryModule.MyTryType.Ok(value) => value                           │
│                     ‾‾‾‾‾‾‾‾‾‾                                              │
└─────────────────────────────────── nominal_external_fully_qualified.md:6:20 ┘

    You're attempting to use this type here:
                                                            ┌──────────────────┐
┌─ The type MyTryType is qualified by the module ───────────┤ MODULE NOT FOUND │
│  MyTryModule, but that module was not found in this Roc   └─────────────────┬┘
│  project.                                                                   │
│                                                                             │
│          MyTryModule.MyTryType.Err(code) => "Error: $(code.toStr())"        │
│                     ‾‾‾‾‾‾‾‾‾‾                                              │
└─────────────────────────────────── nominal_external_fully_qualified.md:7:20 ┘

    You're attempting to use this type here:
                                                             ┌─────────────────┐
┌─ Variable code is not used anywhere in your code. ─────────┤ UNUSED VARIABLE │
│                                                            └────────────────┬┘
│                                                                             │
│          MyTryModule.MyTryType.Err(code) => "Error: $(code.toStr())"        │
│                                    ‾‾‾‾                                     │
└─────────────────────────────────── nominal_external_fully_qualified.md:7:35 ┘

    If you don't need this variable, prefix it with an underscore like _code to suppress this warning.
    The unused variable is declared here:
# TOKENS
~~~zig
KwImport,UpperIdent,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,UpperIdent,Comma,UpperIdent,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,
UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-import (raw "MyTryModule"))
		(s-type-anno (name "handleTry")
			(ty-fn
				(ty-apply
					(ty (name "MyTryModule.MyTryType"))
					(ty (name "Str"))
					(ty (name "I32")))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "handleTry"))
			(e-lambda
				(args
					(p-ident (raw "result")))
				(e-block
					(statements
						(e-match
							(e-ident (raw "result"))
							(branches
								(branch
									(p-tag (raw ".Ok")
										(p-ident (raw "value")))
									(e-ident (raw "value")))
								(branch
									(p-tag (raw ".Err")
										(p-ident (raw "code")))
									(e-string
										(e-string-part (raw "Error: $(code.toStr())"))))))))))))
~~~
# FORMATTED
~~~roc
import MyTryModule

handleTry : MyTryModule.MyTryType(Str, I32) -> Str
handleTry = |result| {
	match result {
		MyTryModule.MyTryType.Ok(value) => value
		MyTryModule.MyTryType.Err(code) => "Error: $(code.toStr())"
	}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "handleTry"))
		(e-closure
			(captures
				(capture (ident "value")))
			(e-lambda
				(args
					(p-assign (ident "result")))
				(e-block
					(e-match
						(match
							(cond
								(e-lookup-local
									(p-assign (ident "result"))))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-runtime-error (tag "type_from_missing_module"))))
									(value
										(e-lookup-local
											(p-assign (ident "value")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-runtime-error (tag "type_from_missing_module"))))
									(value
										(e-string
											(e-literal (string "Error: $(code.toStr())")))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-lookup (name "Str") (builtin)))))
	(s-import (module "MyTryModule")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error -> Str")))
	(expressions
		(expr (type "Error -> Str"))))
~~~
