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
TYPE NOT EXPOSED - nominal_external_fully_qualified.md:3:30:3:43
UNDECLARED TYPE - nominal_external_fully_qualified.md:6:23:6:36
UNDECLARED TYPE - nominal_external_fully_qualified.md:7:23:7:36
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


**TYPE NOT EXPOSED**
The type `MyResultType` is not an exposed by the module `MyResultModule`.

You're attempting to use this type here:
**nominal_external_fully_qualified.md:3:30:3:43:**
```roc
handleResult : MyResultModule.MyResultType(Str, I32) -> Str
```
                             ^^^^^^^^^^^^^


**UNDECLARED TYPE**
The type _MyResultModule.MyResultType_ is not declared in this scope.

This type is referenced here:
**nominal_external_fully_qualified.md:6:23:6:36:**
```roc
        MyResultModule.MyResultType.Ok(value) => value
```
                      ^^^^^^^^^^^^^


**UNDECLARED TYPE**
The type _MyResultModule.MyResultType_ is not declared in this scope.

This type is referenced here:
**nominal_external_fully_qualified.md:7:23:7:36:**
```roc
        MyResultModule.MyResultType.Err(code) => "Error: $(code.toStr())"
```
                      ^^^^^^^^^^^^^


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
		(s-import (raw "MyResultModule"))
		(s-type-anno (name "handleResult")
			(ty-fn
				(ty-apply
					(ty (name "MyResultModule.MyResultType"))
					(ty (name "Str"))
					(ty (name "I32")))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "handleResult"))
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
		(p-assign (ident "handleResult"))
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
											(p-runtime-error (tag "undeclared_type"))))
									(value
										(e-lookup-local
											(p-assign (ident "value")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-runtime-error (tag "undeclared_type"))))
									(value
										(e-string
											(e-literal (string "Error: $(code.toStr())")))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-lookup (name "Str") (builtin)))))
	(s-import (module "MyResultModule")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error -> Error")))
	(expressions
		(expr (type "Error -> Error"))))
~~~
