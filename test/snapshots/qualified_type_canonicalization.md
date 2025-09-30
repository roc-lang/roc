# META
~~~ini
description=Qualified type canonicalization
type=file
~~~
# SOURCE
~~~roc
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
PARSE ERROR - qualified_type_canonicalization.md:1:14:1:21
PARSE ERROR - qualified_type_canonicalization.md:3:15:3:23
PARSE ERROR - qualified_type_canonicalization.md:3:24:3:32
PARSE ERROR - qualified_type_canonicalization.md:3:33:3:34
PARSE ERROR - qualified_type_canonicalization.md:3:39:3:40
MISSING MAIN! FUNCTION - qualified_type_canonicalization.md:1:1:37:6
MODULE NOT FOUND - qualified_type_canonicalization.md:1:1:1:14
MODULE NOT FOUND - qualified_type_canonicalization.md:2:1:2:13
MODULE NOT FOUND - qualified_type_canonicalization.md:3:1:3:15
MODULE NOT FOUND - qualified_type_canonicalization.md:4:1:4:32
UNDECLARED TYPE - qualified_type_canonicalization.md:8:19:8:24
MODULE NOT IMPORTED - qualified_type_canonicalization.md:15:23:15:44
UNDEFINED VARIABLE - qualified_type_canonicalization.md:16:23:16:32
MODULE NOT IMPORTED - qualified_type_canonicalization.md:19:14:19:27
UNDECLARED TYPE - qualified_type_canonicalization.md:24:16:24:21
UNUSED VARIABLE - qualified_type_canonicalization.md:28:17:28:22
MODULE NOT IMPORTED - qualified_type_canonicalization.md:32:13:32:26
MODULE NOT IMPORTED - qualified_type_canonicalization.md:32:55:32:76
UNDEFINED VARIABLE - qualified_type_canonicalization.md:35:27:35:42
UNDEFINED VARIABLE - qualified_type_canonicalization.md:36:28:36:41
UNUSED VARIABLE - qualified_type_canonicalization.md:36:20:36:23
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**qualified_type_canonicalization.md:1:14:1:21:**
```roc
import Basics.Result
```
             ^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**qualified_type_canonicalization.md:3:15:3:23:**
```roc
import ModuleA.ModuleB exposing [TypeC]
```
              ^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**qualified_type_canonicalization.md:3:24:3:32:**
```roc
import ModuleA.ModuleB exposing [TypeC]
```
                       ^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**qualified_type_canonicalization.md:3:33:3:34:**
```roc
import ModuleA.ModuleB exposing [TypeC]
```
                                ^


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

**qualified_type_canonicalization.md:3:39:3:40:**
```roc
import ModuleA.ModuleB exposing [TypeC]
```
                                      ^


**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**qualified_type_canonicalization.md:1:1:37:6:**
```roc
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
```


**MODULE NOT FOUND**
The module `Basics` was not found in this Roc project.

You're attempting to use this module here:
**qualified_type_canonicalization.md:1:1:1:14:**
```roc
import Basics.Result
```
^^^^^^^^^^^^^


**MODULE NOT FOUND**
The module `Color` was not found in this Roc project.

You're attempting to use this module here:
**qualified_type_canonicalization.md:2:1:2:13:**
```roc
import Color
```
^^^^^^^^^^^^


**MODULE NOT FOUND**
The module `ModuleA` was not found in this Roc project.

You're attempting to use this module here:
**qualified_type_canonicalization.md:3:1:3:15:**
```roc
import ModuleA.ModuleB exposing [TypeC]
```
^^^^^^^^^^^^^^


**MODULE NOT FOUND**
The module `ExternalModule` was not found in this Roc project.

You're attempting to use this module here:
**qualified_type_canonicalization.md:4:1:4:32:**
```roc
import ExternalModule as ExtMod
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNDECLARED TYPE**
The type _Color_ is not declared in this scope.

This type is referenced here:
**qualified_type_canonicalization.md:8:19:8:24:**
```roc
simpleQualified = Color.RGB({ r: 255, g: 0, b: 0 })
```
                  ^^^^^


**MODULE NOT IMPORTED**
There is no module with the name `import Basics.Result
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
multiLevelQualified : ModuleA.ModuleB` imported into this Roc file.

You're attempting to use this module here:
**qualified_type_canonicalization.md:15:23:15:44:**
```roc
multiLevelQualified : ModuleA.ModuleB.TypeC
```
                      ^^^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `new` in this scope.
Is there an `import` or `exposing` missing up-top?

**qualified_type_canonicalization.md:16:23:16:32:**
```roc
multiLevelQualified = TypeC.new
```
                      ^^^^^^^^^


**MODULE NOT IMPORTED**
There is no module with the name `Result` imported into this Roc file.

You're attempting to use this module here:
**qualified_type_canonicalization.md:19:14:19:27:**
```roc
resultType : Result.Result(I32, Str)
```
             ^^^^^^^^^^^^^


**UNDECLARED TYPE**
The type _Color_ is not declared in this scope.

This type is referenced here:
**qualified_type_canonicalization.md:24:16:24:21:**
```roc
getColor = |_| Color.RGB({ r: 0, g: 255, b: 0 })
```
               ^^^^^


**UNUSED VARIABLE**
Variable `color` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_color` to suppress this warning.
The unused variable is declared here:
**qualified_type_canonicalization.md:28:17:28:22:**
```roc
processColor = |color|
```
                ^^^^^


**MODULE NOT IMPORTED**
There is no module with the name `Result` imported into this Roc file.

You're attempting to use this module here:
**qualified_type_canonicalization.md:32:13:32:26:**
```roc
transform : Result.Result(Color.RGB, ExtMod.Error) -> ModuleA.ModuleB.TypeC
```
            ^^^^^^^^^^^^^


**MODULE NOT IMPORTED**
There is no module with the name `import Basics.Result
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
transform : Result.Result(Color.RGB, ExtMod.Error) -> ModuleA.ModuleB` imported into this Roc file.

You're attempting to use this module here:
**qualified_type_canonicalization.md:32:55:32:76:**
```roc
transform : Result.Result(Color.RGB, ExtMod.Error) -> ModuleA.ModuleB.TypeC
```
                                                      ^^^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `fromColor` in this scope.
Is there an `import` or `exposing` missing up-top?

**qualified_type_canonicalization.md:35:27:35:42:**
```roc
        Result.Ok(rgb) => TypeC.fromColor(rgb)
```
                          ^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `default` in this scope.
Is there an `import` or `exposing` missing up-top?

**qualified_type_canonicalization.md:36:28:36:41:**
```roc
        Result.Err(err) => TypeC.default
```
                           ^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable `err` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_err` to suppress this warning.
The unused variable is declared here:
**qualified_type_canonicalization.md:36:20:36:23:**
```roc
        Result.Err(err) => TypeC.default
```
                   ^^^


# TOKENS
~~~zig
KwImport(1:1-1:7),UpperIdent(1:8-1:14),NoSpaceDotUpperIdent(1:14-1:21),
KwImport(2:1-2:7),UpperIdent(2:8-2:13),
KwImport(3:1-3:7),UpperIdent(3:8-3:15),NoSpaceDotUpperIdent(3:15-3:23),KwExposing(3:24-3:32),OpenSquare(3:33-3:34),UpperIdent(3:34-3:39),CloseSquare(3:39-3:40),
KwImport(4:1-4:7),UpperIdent(4:8-4:22),KwAs(4:23-4:25),UpperIdent(4:26-4:32),
LowerIdent(7:1-7:16),OpColon(7:17-7:18),UpperIdent(7:19-7:24),NoSpaceDotUpperIdent(7:24-7:28),
LowerIdent(8:1-8:16),OpAssign(8:17-8:18),UpperIdent(8:19-8:24),NoSpaceDotUpperIdent(8:24-8:28),NoSpaceOpenRound(8:28-8:29),OpenCurly(8:29-8:30),LowerIdent(8:31-8:32),OpColon(8:32-8:33),Int(8:34-8:37),Comma(8:37-8:38),LowerIdent(8:39-8:40),OpColon(8:40-8:41),Int(8:42-8:43),Comma(8:43-8:44),LowerIdent(8:45-8:46),OpColon(8:46-8:47),Int(8:48-8:49),CloseCurly(8:50-8:51),CloseRound(8:51-8:52),
LowerIdent(11:1-11:17),OpColon(11:18-11:19),UpperIdent(11:20-11:26),NoSpaceDotUpperIdent(11:26-11:35),
LowerIdent(12:1-12:17),OpAssign(12:18-12:19),UpperIdent(12:20-12:26),NoSpaceDotUpperIdent(12:26-12:35),NoSpaceDotUpperIdent(12:35-12:43),
LowerIdent(15:1-15:20),OpColon(15:21-15:22),UpperIdent(15:23-15:30),NoSpaceDotUpperIdent(15:30-15:38),NoSpaceDotUpperIdent(15:38-15:44),
LowerIdent(16:1-16:20),OpAssign(16:21-16:22),UpperIdent(16:23-16:28),NoSpaceDotLowerIdent(16:28-16:32),
LowerIdent(19:1-19:11),OpColon(19:12-19:13),UpperIdent(19:14-19:20),NoSpaceDotUpperIdent(19:20-19:27),NoSpaceOpenRound(19:27-19:28),UpperIdent(19:28-19:31),Comma(19:31-19:32),UpperIdent(19:33-19:36),CloseRound(19:36-19:37),
LowerIdent(20:1-20:11),OpAssign(20:12-20:13),UpperIdent(20:14-20:20),NoSpaceDotUpperIdent(20:20-20:23),NoSpaceOpenRound(20:23-20:24),Int(20:24-20:26),CloseRound(20:26-20:27),
LowerIdent(23:1-23:9),OpColon(23:10-23:11),OpenCurly(23:12-23:13),CloseCurly(23:13-23:14),OpArrow(23:15-23:17),UpperIdent(23:18-23:23),NoSpaceDotUpperIdent(23:23-23:27),
LowerIdent(24:1-24:9),OpAssign(24:10-24:11),OpBar(24:12-24:13),Underscore(24:13-24:14),OpBar(24:14-24:15),UpperIdent(24:16-24:21),NoSpaceDotUpperIdent(24:21-24:25),NoSpaceOpenRound(24:25-24:26),OpenCurly(24:26-24:27),LowerIdent(24:28-24:29),OpColon(24:29-24:30),Int(24:31-24:32),Comma(24:32-24:33),LowerIdent(24:34-24:35),OpColon(24:35-24:36),Int(24:37-24:40),Comma(24:40-24:41),LowerIdent(24:42-24:43),OpColon(24:43-24:44),Int(24:45-24:46),CloseCurly(24:47-24:48),CloseRound(24:48-24:49),
LowerIdent(27:1-27:13),OpColon(27:14-27:15),UpperIdent(27:16-27:21),NoSpaceDotUpperIdent(27:21-27:25),OpArrow(27:26-27:28),UpperIdent(27:29-27:32),
LowerIdent(28:1-28:13),OpAssign(28:14-28:15),OpBar(28:16-28:17),LowerIdent(28:17-28:22),OpBar(28:22-28:23),
StringStart(29:5-29:6),StringPart(29:6-29:21),StringEnd(29:21-29:22),
LowerIdent(32:1-32:10),OpColon(32:11-32:12),UpperIdent(32:13-32:19),NoSpaceDotUpperIdent(32:19-32:26),NoSpaceOpenRound(32:26-32:27),UpperIdent(32:27-32:32),NoSpaceDotUpperIdent(32:32-32:36),Comma(32:36-32:37),UpperIdent(32:38-32:44),NoSpaceDotUpperIdent(32:44-32:50),CloseRound(32:50-32:51),OpArrow(32:52-32:54),UpperIdent(32:55-32:62),NoSpaceDotUpperIdent(32:62-32:70),NoSpaceDotUpperIdent(32:70-32:76),
LowerIdent(33:1-33:10),OpAssign(33:11-33:12),OpBar(33:13-33:14),LowerIdent(33:14-33:20),OpBar(33:20-33:21),
KwMatch(34:5-34:10),LowerIdent(34:11-34:17),OpenCurly(34:18-34:19),
UpperIdent(35:9-35:15),NoSpaceDotUpperIdent(35:15-35:18),NoSpaceOpenRound(35:18-35:19),LowerIdent(35:19-35:22),CloseRound(35:22-35:23),OpFatArrow(35:24-35:26),UpperIdent(35:27-35:32),NoSpaceDotLowerIdent(35:32-35:42),NoSpaceOpenRound(35:42-35:43),LowerIdent(35:43-35:46),CloseRound(35:46-35:47),
UpperIdent(36:9-36:15),NoSpaceDotUpperIdent(36:15-36:19),NoSpaceOpenRound(36:19-36:20),LowerIdent(36:20-36:23),CloseRound(36:23-36:24),OpFatArrow(36:25-36:27),UpperIdent(36:28-36:33),NoSpaceDotLowerIdent(36:33-36:41),
CloseCurly(37:5-37:6),
EndOfFile(38:1-38:1),
~~~
# PARSE
~~~clojure
(file @1.1-37.6
	(type-module @1.1-1.7)
	(statements
		(s-import @1.1-1.14 (raw "Basics"))
		(s-malformed @1.14-1.21 (tag "statement_unexpected_token"))
		(s-import @2.1-2.13 (raw "Color"))
		(s-import @3.1-3.15 (raw "ModuleA"))
		(s-malformed @3.15-3.23 (tag "statement_unexpected_token"))
		(s-malformed @3.24-3.32 (tag "statement_unexpected_token"))
		(s-malformed @3.33-3.34 (tag "statement_unexpected_token"))
		(s-malformed @3.39-3.40 (tag "expected_colon_after_type_annotation"))
		(s-import @4.1-4.32 (raw "ExternalModule") (alias "ExtMod"))
		(s-type-anno @7.1-7.28 (name "simpleQualified")
			(ty @7.19-7.28 (name "Color.RGB")))
		(s-decl @8.1-8.52
			(p-ident @8.1-8.16 (raw "simpleQualified"))
			(e-apply @8.19-8.52
				(e-tag @8.19-8.28 (raw "Color.RGB"))
				(e-record @8.29-8.51
					(field (field "r")
						(e-int @8.34-8.37 (raw "255")))
					(field (field "g")
						(e-int @8.42-8.43 (raw "0")))
					(field (field "b")
						(e-int @8.48-8.49 (raw "0"))))))
		(s-type-anno @11.1-11.35 (name "aliasedQualified")
			(ty @11.20-11.35 (name "ExtMod.DataType")))
		(s-decl @12.1-12.43
			(p-ident @12.1-12.17 (raw "aliasedQualified"))
			(e-tag @12.20-12.43 (raw "ExtMod.DataType.Default")))
		(s-type-anno @15.1-15.44 (name "multiLevelQualified")
			(ty @15.23-15.44 (name "ModuleA.ModuleB.TypeC")))
		(s-decl @16.1-16.32
			(p-ident @16.1-16.20 (raw "multiLevelQualified"))
			(e-ident @16.23-16.32 (raw "TypeC.new")))
		(s-type-anno @19.1-19.37 (name "resultType")
			(ty-apply @19.14-19.37
				(ty @19.14-19.27 (name "Result.Result"))
				(ty @19.28-19.31 (name "I32"))
				(ty @19.33-19.36 (name "Str"))))
		(s-decl @20.1-20.27
			(p-ident @20.1-20.11 (raw "resultType"))
			(e-apply @20.14-20.27
				(e-tag @20.14-20.23 (raw "Result.Ok"))
				(e-int @20.24-20.26 (raw "42"))))
		(s-type-anno @23.1-23.27 (name "getColor")
			(ty-fn @23.12-23.27
				(ty-record @23.12-23.14)
				(ty @23.18-23.27 (name "Color.RGB"))))
		(s-decl @24.1-24.49
			(p-ident @24.1-24.9 (raw "getColor"))
			(e-lambda @24.12-24.49
				(args
					(p-underscore))
				(e-apply @24.16-24.49
					(e-tag @24.16-24.25 (raw "Color.RGB"))
					(e-record @24.26-24.48
						(field (field "r")
							(e-int @24.31-24.32 (raw "0")))
						(field (field "g")
							(e-int @24.37-24.40 (raw "255")))
						(field (field "b")
							(e-int @24.45-24.46 (raw "0")))))))
		(s-type-anno @27.1-27.32 (name "processColor")
			(ty-fn @27.16-27.32
				(ty @27.16-27.25 (name "Color.RGB"))
				(ty @27.29-27.32 (name "Str"))))
		(s-decl @28.1-29.22
			(p-ident @28.1-28.13 (raw "processColor"))
			(e-lambda @28.16-29.22
				(args
					(p-ident @28.17-28.22 (raw "color")))
				(e-string @29.5-29.22
					(e-string-part @29.6-29.21 (raw "Color processed")))))
		(s-type-anno @32.1-32.76 (name "transform")
			(ty-fn @32.13-32.76
				(ty-apply @32.13-32.51
					(ty @32.13-32.26 (name "Result.Result"))
					(ty @32.27-32.36 (name "Color.RGB"))
					(ty @32.38-32.50 (name "ExtMod.Error")))
				(ty @32.55-32.76 (name "ModuleA.ModuleB.TypeC"))))
		(s-decl @33.1-37.6
			(p-ident @33.1-33.10 (raw "transform"))
			(e-lambda @33.13-37.6
				(args
					(p-ident @33.14-33.20 (raw "result")))
				(e-match
					(e-ident @34.11-34.17 (raw "result"))
					(branches
						(branch @35.9-35.47
							(p-tag @35.9-35.23 (raw ".Ok")
								(p-ident @35.19-35.22 (raw "rgb")))
							(e-apply @35.27-35.47
								(e-ident @35.27-35.42 (raw "TypeC.fromColor"))
								(e-ident @35.43-35.46 (raw "rgb"))))
						(branch @36.9-36.41
							(p-tag @36.9-36.24 (raw ".Err")
								(p-ident @36.20-36.23 (raw "err")))
							(e-ident @36.28-36.41 (raw "TypeC.default")))))))))
~~~
# FORMATTED
~~~roc
import Basics

import Color
import ModuleA

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
		(p-assign @8.1-8.16 (ident "simpleQualified"))
		(e-runtime-error (tag "undeclared_type"))
		(annotation @8.1-8.16
			(declared-type
				(ty-lookup-external @7.19-7.28
					(module-idx "1")
					(target-node-idx "0")))))
	(d-let
		(p-assign @12.1-12.17 (ident "aliasedQualified"))
		(e-nominal-external @12.20-12.43
			(module-idx "3")
			(target-node-idx "0")
			(e-tag @12.20-12.43 (name "Default")))
		(annotation @12.1-12.17
			(declared-type
				(ty-lookup-external @11.20-11.35
					(module-idx "3")
					(target-node-idx "0")))))
	(d-let
		(p-assign @16.1-16.20 (ident "multiLevelQualified"))
		(e-runtime-error (tag "ident_not_in_scope"))
		(annotation @16.1-16.20
			(declared-type
				(ty-malformed @15.23-15.44))))
	(d-let
		(p-assign @20.1-20.11 (ident "resultType"))
		(e-nominal @20.14-20.27 (nominal "Result")
			(e-tag @20.14-20.27 (name "Ok")
				(args
					(e-int @20.24-20.26 (value "42")))))
		(annotation @20.1-20.11
			(declared-type
				(ty-malformed @19.14-19.27))))
	(d-let
		(p-assign @24.1-24.9 (ident "getColor"))
		(e-lambda @24.12-24.49
			(args
				(p-underscore @24.13-24.14))
			(e-runtime-error (tag "undeclared_type")))
		(annotation @24.1-24.9
			(declared-type
				(ty-fn @23.12-23.27 (effectful false)
					(ty-record @23.12-23.14)
					(ty-lookup-external @23.18-23.27
						(module-idx "1")
						(target-node-idx "0"))))))
	(d-let
		(p-assign @28.1-28.13 (ident "processColor"))
		(e-lambda @28.16-29.22
			(args
				(p-assign @28.17-28.22 (ident "color")))
			(e-string @29.5-29.22
				(e-literal @29.6-29.21 (string "Color processed"))))
		(annotation @28.1-28.13
			(declared-type
				(ty-fn @27.16-27.32 (effectful false)
					(ty-lookup-external @27.16-27.25
						(module-idx "1")
						(target-node-idx "0"))
					(ty @27.29-27.32 (name "Str"))))))
	(d-let
		(p-assign @33.1-33.10 (ident "transform"))
		(e-closure @33.13-37.6
			(captures
				(capture @35.19-35.22 (ident "rgb")))
			(e-lambda @33.13-37.6
				(args
					(p-assign @33.14-33.20 (ident "result")))
				(e-match @34.5-37.6
					(match @34.5-37.6
						(cond
							(e-lookup-local @34.11-34.17
								(p-assign @33.14-33.20 (ident "result"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-nominal @35.9-35.23
											(p-applied-tag @35.9-35.23))))
								(value
									(e-call @35.27-35.47
										(e-runtime-error (tag "ident_not_in_scope"))
										(e-lookup-local @35.43-35.46
											(p-assign @35.19-35.22 (ident "rgb"))))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-nominal @36.9-36.24
											(p-applied-tag @36.9-36.24))))
								(value
									(e-runtime-error (tag "ident_not_in_scope")))))))))
		(annotation @33.1-33.10
			(declared-type
				(ty-fn @32.13-32.76 (effectful false)
					(ty-malformed @32.13-32.26)
					(ty-malformed @32.55-32.76)))))
	(s-import @1.1-1.14 (module "Basics")
		(exposes))
	(s-import @2.1-2.13 (module "Color")
		(exposes))
	(s-import @3.1-3.15 (module "ModuleA")
		(exposes))
	(s-import @4.1-4.32 (module "ExternalModule") (alias "ExtMod")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @8.1-8.16 (type "Error"))
		(patt @12.1-12.17 (type "Error"))
		(patt @16.1-16.20 (type "Error"))
		(patt @20.1-20.11 (type "Error"))
		(patt @24.1-24.9 (type "{  } -> Error"))
		(patt @28.1-28.13 (type "Error -> Str"))
		(patt @33.1-33.10 (type "Error -> Error")))
	(expressions
		(expr @8.19-8.24 (type "Error"))
		(expr @12.20-12.43 (type "Error"))
		(expr @16.23-16.32 (type "Error"))
		(expr @20.14-20.27 (type "Error"))
		(expr @24.12-24.49 (type "{  } -> Error"))
		(expr @28.16-29.22 (type "Error -> Str"))
		(expr @33.13-37.6 (type "Error -> Error"))))
~~~
