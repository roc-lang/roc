# META
~~~ini
description=Example of external nominal tag union fully qualified name
type=snippet
~~~
# SOURCE
~~~roc
import MyTryMod

handleTry : MyTryMod.MyTryType(Str, I32) -> Str
handleTry = |result| {
    match result {
        MyTryMod.MyTryType.Ok(value) => value
        MyTryMod.MyTryType.Err(code) => "Error: $(code.toStr())"
    }
}
~~~
# EXPECTED
MOD NOT FOUND - nominal_external_fully_qualified.md:3:21:3:31
MOD NOT FOUND - nominal_external_fully_qualified.md:6:17:6:27
MOD NOT FOUND - nominal_external_fully_qualified.md:7:17:7:27
UNUSED VARIABLE - nominal_external_fully_qualified.md:7:32:7:36
# PROBLEMS

┌──────────────────┐
│ MOD NOT FOUND ├─ This `MyTryType` type is declared to be in ─────────────┐
└┬─────────────────┘  `MyTryMod`, which does not exist.                       │
 │                                                                            │
 │  handleTry : MyTryMod.MyTryType(Str, I32) -> Str                           │
 │                      ‾‾‾‾‾‾‾‾‾‾                                            │
 └────────────────────────────────── nominal_external_fully_qualified.md:3:21 ┘



┌──────────────────┐
│ MOD NOT FOUND ├─ This `MyTryType` type is declared to be in ─────────────┐
└┬─────────────────┘  `MyTryMod`, which does not exist.                       │
 │                                                                            │
 │  MyTryMod.MyTryType.Ok(value) => value                                     │
 │          ‾‾‾‾‾‾‾‾‾‾                                                        │
 └────────────────────────────────── nominal_external_fully_qualified.md:6:17 ┘



┌──────────────────┐
│ MOD NOT FOUND ├─ This `MyTryType` type is declared to be in ─────────────┐
└┬─────────────────┘  `MyTryMod`, which does not exist.                       │
 │                                                                            │
 │  MyTryMod.MyTryType.Err(code) => "Error: $(code.toStr())"                  │
 │          ‾‾‾‾‾‾‾‾‾‾                                                        │
 └────────────────────────────────── nominal_external_fully_qualified.md:7:17 ┘



┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `code` is defined here and then never used. ────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  MyTryMod.MyTryType.Err(code) => "Error: $(code.toStr())"                  │
 │                         ‾‾‾‾                                               │
 └────────────────────────────────── nominal_external_fully_qualified.md:7:32 ┘

    If you don't need this variable, prefix it with an underscore like `_code`
    to suppress this warning.

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
	(type-mod)
	(statements
		(s-import (raw "MyTryMod"))
		(s-type-anno (name "handleTry")
			(ty-fn
				(ty-apply
					(ty (name "MyTryMod.MyTryType"))
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
import MyTryMod

handleTry : MyTryMod.MyTryType(Str, I32) -> Str
handleTry = |result| {
	match result {
		MyTryMod.MyTryType.Ok(value) => value
		MyTryMod.MyTryType.Err(code) => "Error: $(code.toStr())"
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
											(p-runtime-error (tag "type_from_missing_mod"))))
									(value
										(e-lookup-local
											(p-assign (ident "value")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-runtime-error (tag "type_from_missing_mod"))))
									(value
										(e-string
											(e-literal (string "Error: $(code.toStr())")))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-lookup (name "Str") (builtin)))))
	(s-import (mod "MyTryMod")
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
