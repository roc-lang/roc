# META
~~~ini
description=Qualified type canonicalization
type=file
~~~
# SOURCE
~~~roc
module [
    Color,
    ModuleA.ModuleB.TypeC,
    Result,
    ExternalModule,
]

import Basics.Result
import Color
import ModuleA.ModuleB exposing [TypeC]
import ExternalModule as ExtMod

# Simple qualified type
simpleQualified : Color.RGB
simpleQualified = Color.RGB({ r: 255, g: 0, b: 0 })

# Aliased qualified type
aliasedQualified : ExtMod.DataType
aliasedQualified = ExtMod.DataType.Default

# Multi-level qualified type
multiLevelQualified : ModuleA.ModuleB.TypeC
multiLevelQualified = TypeC.new

# Using qualified type with generics
resultType : Result.Result(I32, Str)
resultType = Result.Ok(42)

# Function returning qualified type
getColor : {} -> Color.RGB
getColor = |_| Color.RGB({ r: 0, g: 255, b: 0 })

# Function accepting qualified type
processColor : Color.RGB -> Str
processColor = |color|
    "Color processed"

# Multiple qualified types in a function signature
transform : Result.Result(Color.RGB, ExtMod.Error) -> ModuleA.ModuleB.TypeC
transform = |result|
    match result {
        Result.Ok(rgb) => TypeC.fromColor(rgb)
        Result.Err(err) => TypeC.default
    }
~~~
# EXPECTED
PARSE ERROR - qualified_type_canonicalization.md:8:1:8:7
PARSE ERROR - qualified_type_canonicalization.md:8:14:8:21
MODULE NOT FOUND - qualified_type_canonicalization.md:9:1:9:13
MODULE NOT FOUND - qualified_type_canonicalization.md:10:1:10:40
MODULE NOT FOUND - qualified_type_canonicalization.md:11:1:11:32
TYPE NOT EXPOSED - qualified_type_canonicalization.md:14:24:14:28
UNDECLARED TYPE - qualified_type_canonicalization.md:15:19:15:24
TYPE NOT EXPOSED - qualified_type_canonicalization.md:18:26:18:35
UNDECLARED TYPE - qualified_type_canonicalization.md:19:26:19:35
UNDECLARED TYPE - qualified_type_canonicalization.md:22:38:22:44
UNDEFINED VARIABLE - qualified_type_canonicalization.md:23:23:23:32
TYPE NOT EXPOSED - qualified_type_canonicalization.md:26:20:26:27
TYPE NOT EXPOSED - qualified_type_canonicalization.md:30:23:30:27
UNDECLARED TYPE - qualified_type_canonicalization.md:31:16:31:21
TYPE NOT EXPOSED - qualified_type_canonicalization.md:34:21:34:25
UNUSED VARIABLE - qualified_type_canonicalization.md:35:17:35:22
TYPE NOT EXPOSED - qualified_type_canonicalization.md:39:19:39:26
TYPE NOT EXPOSED - qualified_type_canonicalization.md:39:32:39:36
TYPE NOT EXPOSED - qualified_type_canonicalization.md:39:44:39:50
UNDECLARED TYPE - qualified_type_canonicalization.md:39:70:39:76
UNDEFINED VARIABLE - qualified_type_canonicalization.md:42:27:42:42
UNDEFINED VARIABLE - qualified_type_canonicalization.md:43:28:43:41
UNUSED VARIABLE - qualified_type_canonicalization.md:43:20:43:23
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `import_exposing_no_close`
This is an unexpected parsing error. Please check your syntax.

**qualified_type_canonicalization.md:8:1:8:7:**
```roc
import Basics.Result
```
^^^^^^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Result(a, Str)`
    `Maybe(List(U64))`

**qualified_type_canonicalization.md:8:14:8:21:**
```roc
import Basics.Result
```
             ^^^^^^^


**MODULE NOT FOUND**
The module `Color` was not found in this Roc project.

You're attempting to use this module here:
**qualified_type_canonicalization.md:9:1:9:13:**
```roc
import Color
```
^^^^^^^^^^^^


**MODULE NOT FOUND**
The module `ModuleB` was not found in this Roc project.

You're attempting to use this module here:
**qualified_type_canonicalization.md:10:1:10:40:**
```roc
import ModuleA.ModuleB exposing [TypeC]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**MODULE NOT FOUND**
The module `ExternalModule` was not found in this Roc project.

You're attempting to use this module here:
**qualified_type_canonicalization.md:11:1:11:32:**
```roc
import ExternalModule as ExtMod
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**TYPE NOT EXPOSED**
The `Color` module does not expose anything named _RGB_.

You're attempting to use this type here:
**qualified_type_canonicalization.md:14:24:14:28:**
```roc
simpleQualified : Color.RGB
```
                       ^^^^

Make sure the module exports this type, or use a type that is exposed.


**UNDECLARED TYPE**
The type _Color_ is not declared in this scope.

This type is referenced here:
**qualified_type_canonicalization.md:15:19:15:24:**
```roc
simpleQualified = Color.RGB({ r: 255, g: 0, b: 0 })
```
                  ^^^^^


**TYPE NOT EXPOSED**
The `ExternalModule` module does not expose anything named _DataType_.

You're attempting to use this type here:
**qualified_type_canonicalization.md:18:26:18:35:**
```roc
aliasedQualified : ExtMod.DataType
```
                         ^^^^^^^^^

Make sure the module exports this type, or use a type that is exposed.


**UNDECLARED TYPE**
Cannot resolve qualified type _ExtMod.DataType_.

This type is referenced here:
**qualified_type_canonicalization.md:19:26:19:35:**
```roc
aliasedQualified = ExtMod.DataType.Default
```
                         ^^^^^^^^^


**UNDECLARED TYPE**
Cannot resolve qualified type _ModuleA.ModuleB.TypeC_.

This type is referenced here:
**qualified_type_canonicalization.md:22:38:22:44:**
```roc
multiLevelQualified : ModuleA.ModuleB.TypeC
```
                                     ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `new` in this scope.
Is there an `import` or `exposing` missing up-top?

**qualified_type_canonicalization.md:23:23:23:32:**
```roc
multiLevelQualified = TypeC.new
```
                      ^^^^^^^^^


**TYPE NOT EXPOSED**
There is no _Result.Result_ type.

You're attempting to use this type here:
**qualified_type_canonicalization.md:26:20:26:27:**
```roc
resultType : Result.Result(I32, Str)
```
                   ^^^^^^^

There is a `Result` module, but it does not have a `Result` type nested inside it.


**TYPE NOT EXPOSED**
The `Color` module does not expose anything named _RGB_.

You're attempting to use this type here:
**qualified_type_canonicalization.md:30:23:30:27:**
```roc
getColor : {} -> Color.RGB
```
                      ^^^^

Make sure the module exports this type, or use a type that is exposed.


**UNDECLARED TYPE**
The type _Color_ is not declared in this scope.

This type is referenced here:
**qualified_type_canonicalization.md:31:16:31:21:**
```roc
getColor = |_| Color.RGB({ r: 0, g: 255, b: 0 })
```
               ^^^^^


**TYPE NOT EXPOSED**
The `Color` module does not expose anything named _RGB_.

You're attempting to use this type here:
**qualified_type_canonicalization.md:34:21:34:25:**
```roc
processColor : Color.RGB -> Str
```
                    ^^^^

Make sure the module exports this type, or use a type that is exposed.


**UNUSED VARIABLE**
Variable `color` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_color` to suppress this warning.
The unused variable is declared here:
**qualified_type_canonicalization.md:35:17:35:22:**
```roc
processColor = |color|
```
                ^^^^^


**TYPE NOT EXPOSED**
There is no _Result.Result_ type.

You're attempting to use this type here:
**qualified_type_canonicalization.md:39:19:39:26:**
```roc
transform : Result.Result(Color.RGB, ExtMod.Error) -> ModuleA.ModuleB.TypeC
```
                  ^^^^^^^

There is a `Result` module, but it does not have a `Result` type nested inside it.


**TYPE NOT EXPOSED**
The `Color` module does not expose anything named _RGB_.

You're attempting to use this type here:
**qualified_type_canonicalization.md:39:32:39:36:**
```roc
transform : Result.Result(Color.RGB, ExtMod.Error) -> ModuleA.ModuleB.TypeC
```
                               ^^^^

Make sure the module exports this type, or use a type that is exposed.


**TYPE NOT EXPOSED**
The `ExternalModule` module does not expose anything named _Error_.

You're attempting to use this type here:
**qualified_type_canonicalization.md:39:44:39:50:**
```roc
transform : Result.Result(Color.RGB, ExtMod.Error) -> ModuleA.ModuleB.TypeC
```
                                           ^^^^^^

Make sure the module exports this type, or use a type that is exposed.


**UNDECLARED TYPE**
Cannot resolve qualified type _ModuleA.ModuleB.TypeC_.

This type is referenced here:
**qualified_type_canonicalization.md:39:70:39:76:**
```roc
transform : Result.Result(Color.RGB, ExtMod.Error) -> ModuleA.ModuleB.TypeC
```
                                                                     ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `fromColor` in this scope.
Is there an `import` or `exposing` missing up-top?

**qualified_type_canonicalization.md:42:27:42:42:**
```roc
        Result.Ok(rgb) => TypeC.fromColor(rgb)
```
                          ^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `default` in this scope.
Is there an `import` or `exposing` missing up-top?

**qualified_type_canonicalization.md:43:28:43:41:**
```roc
        Result.Err(err) => TypeC.default
```
                           ^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable `err` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_err` to suppress this warning.
The unused variable is declared here:
**qualified_type_canonicalization.md:43:20:43:23:**
```roc
        Result.Err(err) => TypeC.default
```
                   ^^^


# TOKENS
~~~zig
KwModule,OpenSquare,
UpperIdent,Comma,
UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,Comma,
UpperIdent,Comma,
UpperIdent,Comma,
CloseSquare,
KwImport,UpperIdent,NoSpaceDotUpperIdent,
KwImport,UpperIdent,
KwImport,UpperIdent,NoSpaceDotUpperIdent,KwExposing,OpenSquare,UpperIdent,CloseSquare,
KwImport,UpperIdent,KwAs,UpperIdent,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,UpperIdent,Comma,UpperIdent,CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpColon,OpenCurly,CloseCurly,OpArrow,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,
StringStart,StringPart,StringEnd,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotUpperIdent,Comma,UpperIdent,NoSpaceDotUpperIdent,CloseRound,OpArrow,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,UpperIdent,NoSpaceDotLowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(malformed-header (tag "import_exposing_no_close"))
	(statements
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-import (raw "Color"))
		(s-import (raw ".ModuleB")
			(exposing
				(exposed-upper-ident (text "TypeC"))))
		(s-import (raw "ExternalModule") (alias "ExtMod"))
		(s-type-anno (name "simpleQualified")
			(ty (name "Color.RGB")))
		(s-decl
			(p-ident (raw "simpleQualified"))
			(e-apply
				(e-tag (raw "Color.RGB"))
				(e-record
					(field (field "r")
						(e-int (raw "255")))
					(field (field "g")
						(e-int (raw "0")))
					(field (field "b")
						(e-int (raw "0"))))))
		(s-type-anno (name "aliasedQualified")
			(ty (name "ExtMod.DataType")))
		(s-decl
			(p-ident (raw "aliasedQualified"))
			(e-tag (raw "ExtMod.DataType.Default")))
		(s-type-anno (name "multiLevelQualified")
			(ty (name "ModuleA.ModuleB.TypeC")))
		(s-decl
			(p-ident (raw "multiLevelQualified"))
			(e-ident (raw "TypeC.new")))
		(s-type-anno (name "resultType")
			(ty-apply
				(ty (name "Result.Result"))
				(ty (name "I32"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "resultType"))
			(e-apply
				(e-tag (raw "Result.Ok"))
				(e-int (raw "42"))))
		(s-type-anno (name "getColor")
			(ty-fn
				(ty-record)
				(ty (name "Color.RGB"))))
		(s-decl
			(p-ident (raw "getColor"))
			(e-lambda
				(args
					(p-underscore))
				(e-apply
					(e-tag (raw "Color.RGB"))
					(e-record
						(field (field "r")
							(e-int (raw "0")))
						(field (field "g")
							(e-int (raw "255")))
						(field (field "b")
							(e-int (raw "0")))))))
		(s-type-anno (name "processColor")
			(ty-fn
				(ty (name "Color.RGB"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "processColor"))
			(e-lambda
				(args
					(p-ident (raw "color")))
				(e-string
					(e-string-part (raw "Color processed")))))
		(s-type-anno (name "transform")
			(ty-fn
				(ty-apply
					(ty (name "Result.Result"))
					(ty (name "Color.RGB"))
					(ty (name "ExtMod.Error")))
				(ty (name "ModuleA.ModuleB.TypeC"))))
		(s-decl
			(p-ident (raw "transform"))
			(e-lambda
				(args
					(p-ident (raw "result")))
				(e-match
					(e-ident (raw "result"))
					(branches
						(branch
							(p-tag (raw ".Ok")
								(p-ident (raw "rgb")))
							(e-apply
								(e-ident (raw "TypeC.fromColor"))
								(e-ident (raw "rgb"))))
						(branch
							(p-tag (raw ".Err")
								(p-ident (raw "err")))
							(e-ident (raw "TypeC.default")))))))))
~~~
# FORMATTED
~~~roc

import Color
import ModuleB exposing [TypeC]
import ExternalModule as ExtMod

# Simple qualified type
simpleQualified : Color.RGB
simpleQualified = Color.RGB({ r: 255, g: 0, b: 0 })

# Aliased qualified type
aliasedQualified : ExtMod.DataType
aliasedQualified = ExtMod.DataType.Default

# Multi-level qualified type
multiLevelQualified : ModuleA.ModuleB.TypeC
multiLevelQualified = TypeC.new

# Using qualified type with generics
resultType : Result.Result(I32, Str)
resultType = Result.Ok(42)

# Function returning qualified type
getColor : {} -> Color.RGB
getColor = |_| Color.RGB({ r: 0, g: 255, b: 0 })

# Function accepting qualified type
processColor : Color.RGB -> Str
processColor = |color|
	"Color processed"

# Multiple qualified types in a function signature
transform : Result.Result(Color.RGB, ExtMod.Error) -> ModuleA.ModuleB.TypeC
transform = |result|
	match result {
		Result.Ok(rgb) => TypeC.fromColor(rgb)
		Result.Err(err) => TypeC.default
	}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "simpleQualified"))
		(e-runtime-error (tag "undeclared_type"))
		(annotation
			(declared-type
				(ty-malformed))))
	(d-let
		(p-assign (ident "aliasedQualified"))
		(e-runtime-error (tag "undeclared_type"))
		(annotation
			(declared-type
				(ty-malformed))))
	(d-let
		(p-assign (ident "multiLevelQualified"))
		(e-runtime-error (tag "ident_not_in_scope"))
		(annotation
			(declared-type
				(ty-malformed))))
	(d-let
		(p-assign (ident "resultType"))
		(e-nominal-external
			(external-module "Result")
			(e-tag (name "Ok")
				(args
					(e-num (value "42")))))
		(annotation
			(declared-type
				(ty-malformed))))
	(d-let
		(p-assign (ident "getColor"))
		(e-lambda
			(args
				(p-underscore))
			(e-runtime-error (tag "undeclared_type")))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-record)
					(ty-malformed)))))
	(d-let
		(p-assign (ident "processColor"))
		(e-lambda
			(args
				(p-assign (ident "color")))
			(e-string
				(e-literal (string "Color processed"))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-malformed)
					(ty-lookup (name "Str") (builtin))))))
	(d-let
		(p-assign (ident "transform"))
		(e-closure
			(captures
				(capture (ident "rgb")))
			(e-lambda
				(args
					(p-assign (ident "result")))
				(e-match
					(match
						(cond
							(e-lookup-local
								(p-assign (ident "result"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-nominal-external (external-module "Result")
											(p-applied-tag))))
								(value
									(e-call
										(e-runtime-error (tag "ident_not_in_scope"))
										(e-lookup-local
											(p-assign (ident "rgb"))))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-nominal-external (external-module "Result")
											(p-applied-tag))))
								(value
									(e-runtime-error (tag "ident_not_in_scope")))))))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-malformed)
					(ty-malformed)))))
	(s-import (module "Color")
		(exposes))
	(s-import (module "ModuleB")
		(exposes
			(exposed (name "TypeC") (wildcard false))))
	(s-import (module "ExternalModule")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "{  } -> Error"))
		(patt (type "Error -> Str"))
		(patt (type "Error -> Error")))
	(expressions
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "{  } -> Error"))
		(expr (type "Error -> Str"))
		(expr (type "Error -> Error"))))
~~~
