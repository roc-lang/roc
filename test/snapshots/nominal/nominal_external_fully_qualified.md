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
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 3 21) (end 3 31))
		(headline
			(text "This ")
			(annotated code "MyTryType")
			(reflow " type is declared to be in ")
			(annotated code "MyTryMod")
			(reflow ", which does not exist."))
		(document
			(source-region (file "nominal_external_fully_qualified.md") (start 3 21) (end 3 31) (annotation error) (line-text "handleTry : MyTryMod.MyTryType(Str, I32) -> Str"))))
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 6 17) (end 6 27))
		(headline
			(text "This ")
			(annotated code "MyTryType")
			(reflow " type is declared to be in ")
			(annotated code "MyTryMod")
			(reflow ", which does not exist."))
		(document
			(source-region (file "nominal_external_fully_qualified.md") (start 6 17) (end 6 27) (annotation error) (line-text "        MyTryMod.MyTryType.Ok(value) => value"))))
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 7 17) (end 7 27))
		(headline
			(text "This ")
			(annotated code "MyTryType")
			(reflow " type is declared to be in ")
			(annotated code "MyTryMod")
			(reflow ", which does not exist."))
		(document
			(source-region (file "nominal_external_fully_qualified.md") (start 7 17) (end 7 27) (annotation error) (line-text "        MyTryMod.MyTryType.Err(code) => \"Error: $(code.toStr())\""))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 7 32) (end 7 36))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "code")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_code")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "nominal_external_fully_qualified.md") (start 7 32) (end 7 36) (annotation error) (line-text "        MyTryMod.MyTryType.Err(code) => \"Error: $(code.toStr())\"")))))
~~~
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
		(e-runtime-error (tag "erroneous_value_expr"))
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
