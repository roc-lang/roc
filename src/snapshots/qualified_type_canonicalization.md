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
UNEXPECTED TOKEN IN EXPRESSION - qualified_type_canonicalization.md:10:15:10:23
UNEXPECTED TOKEN IN EXPRESSION - qualified_type_canonicalization.md:10:24:10:32
INVALID STATEMENT - qualified_type_canonicalization.md:10:15:10:23
INVALID STATEMENT - qualified_type_canonicalization.md:10:24:10:32
INVALID STATEMENT - qualified_type_canonicalization.md:10:33:10:40
UNDEFINED VARIABLE - qualified_type_canonicalization.md:15:19:15:24
UNDEFINED VARIABLE - qualified_type_canonicalization.md:19:26:19:35
UNDEFINED VARIABLE - qualified_type_canonicalization.md:23:23:23:32
UNDEFINED VARIABLE - qualified_type_canonicalization.md:31:16:31:21
UNUSED VARIABLE - qualified_type_canonicalization.md:35:17:35:22
UNDEFINED VARIABLE - qualified_type_canonicalization.md:42:27:42:42
UNDEFINED VARIABLE - qualified_type_canonicalization.md:43:28:43:41
UNUSED VARIABLE - qualified_type_canonicalization.md:43:20:43:23
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `import_exposing_no_close`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
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

Here is the problematic code:
**qualified_type_canonicalization.md:8:14:8:21:**
```roc
import Basics.Result
```
             ^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.ModuleB** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**qualified_type_canonicalization.md:10:15:10:23:**
```roc
import ModuleA.ModuleB exposing [TypeC]
```
              ^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **exposing** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**qualified_type_canonicalization.md:10:24:10:32:**
```roc
import ModuleA.ModuleB exposing [TypeC]
```
                       ^^^^^^^^


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**qualified_type_canonicalization.md:10:15:10:23:**
```roc
import ModuleA.ModuleB exposing [TypeC]
```
              ^^^^^^^^


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**qualified_type_canonicalization.md:10:24:10:32:**
```roc
import ModuleA.ModuleB exposing [TypeC]
```
                       ^^^^^^^^


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**qualified_type_canonicalization.md:10:33:10:40:**
```roc
import ModuleA.ModuleB exposing [TypeC]
```
                                ^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `Color` in this scope.
Is there an `import` or `exposing` missing up-top?

**qualified_type_canonicalization.md:15:19:15:24:**
```roc
simpleQualified = Color.RGB({ r: 255, g: 0, b: 0 })
```
                  ^^^^^


**UNDEFINED VARIABLE**
Nothing is named `DataType` in this scope.
Is there an `import` or `exposing` missing up-top?

**qualified_type_canonicalization.md:19:26:19:35:**
```roc
aliasedQualified = ExtMod.DataType.Default
```
                         ^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `new` in this scope.
Is there an `import` or `exposing` missing up-top?

**qualified_type_canonicalization.md:23:23:23:32:**
```roc
multiLevelQualified = TypeC.new
```
                      ^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `Color` in this scope.
Is there an `import` or `exposing` missing up-top?

**qualified_type_canonicalization.md:31:16:31:21:**
```roc
getColor = |_| Color.RGB({ r: 0, g: 255, b: 0 })
```
               ^^^^^


**UNUSED VARIABLE**
Variable `color` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_color` to suppress this warning.
The unused variable is declared here:
**qualified_type_canonicalization.md:35:17:35:22:**
```roc
processColor = |color|
```
                ^^^^^


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
KwModule(1:1-1:7),OpenSquare(1:8-1:9),
UpperIdent(2:5-2:10),Comma(2:10-2:11),
UpperIdent(3:5-3:12),NoSpaceDotUpperIdent(3:12-3:20),NoSpaceDotUpperIdent(3:20-3:26),Comma(3:26-3:27),
UpperIdent(4:5-4:11),Comma(4:11-4:12),
UpperIdent(5:5-5:19),Comma(5:19-5:20),
CloseSquare(6:1-6:2),
KwImport(8:1-8:7),UpperIdent(8:8-8:14),NoSpaceDotUpperIdent(8:14-8:21),
KwImport(9:1-9:7),UpperIdent(9:8-9:13),
KwImport(10:1-10:7),UpperIdent(10:8-10:15),NoSpaceDotUpperIdent(10:15-10:23),KwExposing(10:24-10:32),OpenSquare(10:33-10:34),UpperIdent(10:34-10:39),CloseSquare(10:39-10:40),
KwImport(11:1-11:7),UpperIdent(11:8-11:22),KwAs(11:23-11:25),UpperIdent(11:26-11:32),
LowerIdent(14:1-14:16),OpColon(14:17-14:18),UpperIdent(14:19-14:24),NoSpaceDotUpperIdent(14:24-14:28),
LowerIdent(15:1-15:16),OpAssign(15:17-15:18),UpperIdent(15:19-15:24),NoSpaceDotUpperIdent(15:24-15:28),NoSpaceOpenRound(15:28-15:29),OpenCurly(15:29-15:30),LowerIdent(15:31-15:32),OpColon(15:32-15:33),Int(15:34-15:37),Comma(15:37-15:38),LowerIdent(15:39-15:40),OpColon(15:40-15:41),Int(15:42-15:43),Comma(15:43-15:44),LowerIdent(15:45-15:46),OpColon(15:46-15:47),Int(15:48-15:49),CloseCurly(15:50-15:51),CloseRound(15:51-15:52),
LowerIdent(18:1-18:17),OpColon(18:18-18:19),UpperIdent(18:20-18:26),NoSpaceDotUpperIdent(18:26-18:35),
LowerIdent(19:1-19:17),OpAssign(19:18-19:19),UpperIdent(19:20-19:26),NoSpaceDotUpperIdent(19:26-19:35),NoSpaceDotUpperIdent(19:35-19:43),
LowerIdent(22:1-22:20),OpColon(22:21-22:22),UpperIdent(22:23-22:30),NoSpaceDotUpperIdent(22:30-22:38),NoSpaceDotUpperIdent(22:38-22:44),
LowerIdent(23:1-23:20),OpAssign(23:21-23:22),UpperIdent(23:23-23:28),NoSpaceDotLowerIdent(23:28-23:32),
LowerIdent(26:1-26:11),OpColon(26:12-26:13),UpperIdent(26:14-26:20),NoSpaceDotUpperIdent(26:20-26:27),NoSpaceOpenRound(26:27-26:28),UpperIdent(26:28-26:31),Comma(26:31-26:32),UpperIdent(26:33-26:36),CloseRound(26:36-26:37),
LowerIdent(27:1-27:11),OpAssign(27:12-27:13),UpperIdent(27:14-27:20),NoSpaceDotUpperIdent(27:20-27:23),NoSpaceOpenRound(27:23-27:24),Int(27:24-27:26),CloseRound(27:26-27:27),
LowerIdent(30:1-30:9),OpColon(30:10-30:11),OpenCurly(30:12-30:13),CloseCurly(30:13-30:14),OpArrow(30:15-30:17),UpperIdent(30:18-30:23),NoSpaceDotUpperIdent(30:23-30:27),
LowerIdent(31:1-31:9),OpAssign(31:10-31:11),OpBar(31:12-31:13),Underscore(31:13-31:14),OpBar(31:14-31:15),UpperIdent(31:16-31:21),NoSpaceDotUpperIdent(31:21-31:25),NoSpaceOpenRound(31:25-31:26),OpenCurly(31:26-31:27),LowerIdent(31:28-31:29),OpColon(31:29-31:30),Int(31:31-31:32),Comma(31:32-31:33),LowerIdent(31:34-31:35),OpColon(31:35-31:36),Int(31:37-31:40),Comma(31:40-31:41),LowerIdent(31:42-31:43),OpColon(31:43-31:44),Int(31:45-31:46),CloseCurly(31:47-31:48),CloseRound(31:48-31:49),
LowerIdent(34:1-34:13),OpColon(34:14-34:15),UpperIdent(34:16-34:21),NoSpaceDotUpperIdent(34:21-34:25),OpArrow(34:26-34:28),UpperIdent(34:29-34:32),
LowerIdent(35:1-35:13),OpAssign(35:14-35:15),OpBar(35:16-35:17),LowerIdent(35:17-35:22),OpBar(35:22-35:23),
StringStart(36:5-36:6),StringPart(36:6-36:21),StringEnd(36:21-36:22),
LowerIdent(39:1-39:10),OpColon(39:11-39:12),UpperIdent(39:13-39:19),NoSpaceDotUpperIdent(39:19-39:26),NoSpaceOpenRound(39:26-39:27),UpperIdent(39:27-39:32),NoSpaceDotUpperIdent(39:32-39:36),Comma(39:36-39:37),UpperIdent(39:38-39:44),NoSpaceDotUpperIdent(39:44-39:50),CloseRound(39:50-39:51),OpArrow(39:52-39:54),UpperIdent(39:55-39:62),NoSpaceDotUpperIdent(39:62-39:70),NoSpaceDotUpperIdent(39:70-39:76),
LowerIdent(40:1-40:10),OpAssign(40:11-40:12),OpBar(40:13-40:14),LowerIdent(40:14-40:20),OpBar(40:20-40:21),
KwMatch(41:5-41:10),LowerIdent(41:11-41:17),OpenCurly(41:18-41:19),
UpperIdent(42:9-42:15),NoSpaceDotUpperIdent(42:15-42:18),NoSpaceOpenRound(42:18-42:19),LowerIdent(42:19-42:22),CloseRound(42:22-42:23),OpFatArrow(42:24-42:26),UpperIdent(42:27-42:32),NoSpaceDotLowerIdent(42:32-42:42),NoSpaceOpenRound(42:42-42:43),LowerIdent(42:43-42:46),CloseRound(42:46-42:47),
UpperIdent(43:9-43:15),NoSpaceDotUpperIdent(43:15-43:19),NoSpaceOpenRound(43:19-43:20),LowerIdent(43:20-43:23),CloseRound(43:23-43:24),OpFatArrow(43:25-43:27),UpperIdent(43:28-43:33),NoSpaceDotLowerIdent(43:33-43:41),
CloseCurly(44:5-44:6),EndOfFile(44:6-44:6),
~~~
# PARSE
~~~clojure
(file @1.1-44.6
	(malformed-header @8.1-8.7 (tag "import_exposing_no_close"))
	(statements
		(s-malformed @8.8-8.21 (tag "expected_colon_after_type_annotation"))
		(s-import @9.1-9.13 (raw "Color"))
		(s-import @10.1-10.15 (raw "ModuleA"))
		(e-malformed @10.15-10.23 (reason "expr_unexpected_token"))
		(e-malformed @10.24-10.32 (reason "expr_unexpected_token"))
		(e-list @10.33-10.40
			(e-tag @10.34-10.39 (raw "TypeC")))
		(s-import @11.1-11.32 (raw "ExternalModule") (alias "ExtMod"))
		(s-type-anno @14.1-14.28 (name "simpleQualified")
			(ty @14.19-14.28 (name "Color.RGB")))
		(s-decl @15.1-15.52
			(p-ident @15.1-15.16 (raw "simpleQualified"))
			(e-apply @15.19-15.52
				(e-tag @15.19-15.28 (raw "Color.RGB"))
				(e-record @15.29-15.51
					(field (field "r")
						(e-int @15.34-15.37 (raw "255")))
					(field (field "g")
						(e-int @15.42-15.43 (raw "0")))
					(field (field "b")
						(e-int @15.48-15.49 (raw "0"))))))
		(s-type-anno @18.1-18.35 (name "aliasedQualified")
			(ty @18.20-18.35 (name "ExtMod.DataType")))
		(s-decl @19.1-19.43
			(p-ident @19.1-19.17 (raw "aliasedQualified"))
			(e-tag @19.20-19.43 (raw "ExtMod.DataType.Default")))
		(s-type-anno @22.1-22.44 (name "multiLevelQualified")
			(ty @22.23-22.44 (name "ModuleA.ModuleB.TypeC")))
		(s-decl @23.1-23.32
			(p-ident @23.1-23.20 (raw "multiLevelQualified"))
			(e-ident @23.23-23.32 (raw "TypeC.new")))
		(s-type-anno @26.1-26.37 (name "resultType")
			(ty-apply @26.14-26.37
				(ty @26.14-26.27 (name "Result.Result"))
				(ty @26.28-26.31 (name "I32"))
				(ty @26.33-26.36 (name "Str"))))
		(s-decl @27.1-27.27
			(p-ident @27.1-27.11 (raw "resultType"))
			(e-apply @27.14-27.27
				(e-tag @27.14-27.23 (raw "Result.Ok"))
				(e-int @27.24-27.26 (raw "42"))))
		(s-type-anno @30.1-30.27 (name "getColor")
			(ty-fn @30.12-30.27
				(ty-record @30.12-30.14)
				(ty @30.18-30.27 (name "Color.RGB"))))
		(s-decl @31.1-31.49
			(p-ident @31.1-31.9 (raw "getColor"))
			(e-lambda @31.12-31.49
				(args
					(p-underscore))
				(e-apply @31.16-31.49
					(e-tag @31.16-31.25 (raw "Color.RGB"))
					(e-record @31.26-31.48
						(field (field "r")
							(e-int @31.31-31.32 (raw "0")))
						(field (field "g")
							(e-int @31.37-31.40 (raw "255")))
						(field (field "b")
							(e-int @31.45-31.46 (raw "0")))))))
		(s-type-anno @34.1-34.32 (name "processColor")
			(ty-fn @34.16-34.32
				(ty @34.16-34.25 (name "Color.RGB"))
				(ty @34.29-34.32 (name "Str"))))
		(s-decl @35.1-36.22
			(p-ident @35.1-35.13 (raw "processColor"))
			(e-lambda @35.16-36.22
				(args
					(p-ident @35.17-35.22 (raw "color")))
				(e-string @36.5-36.22
					(e-string-part @36.6-36.21 (raw "Color processed")))))
		(s-type-anno @39.1-39.76 (name "transform")
			(ty-fn @39.13-39.76
				(ty-apply @39.13-39.51
					(ty @39.13-39.26 (name "Result.Result"))
					(ty @39.27-39.36 (name "Color.RGB"))
					(ty @39.38-39.50 (name "ExtMod.Error")))
				(ty @39.55-39.76 (name "ModuleA.ModuleB.TypeC"))))
		(s-decl @40.1-44.6
			(p-ident @40.1-40.10 (raw "transform"))
			(e-lambda @40.13-44.6
				(args
					(p-ident @40.14-40.20 (raw "result")))
				(e-match
					(e-ident @41.11-41.17 (raw "result"))
					(branches
						(branch @42.9-42.47
							(p-tag @42.9-42.23 (raw ".Ok")
								(p-ident @42.19-42.22 (raw "rgb")))
							(e-apply @42.27-42.47
								(e-ident @42.27-42.42 (raw "TypeC.fromColor"))
								(e-ident @42.43-42.46 (raw "rgb"))))
						(branch @43.9-43.41
							(p-tag @43.9-43.24 (raw ".Err")
								(p-ident @43.20-43.23 (raw "err")))
							(e-ident @43.28-43.41 (raw "TypeC.default")))))))))
~~~
# FORMATTED
~~~roc



import Color
import ModuleA
[TypeC]
import ExternalModule as ExtMod

# Simple qualified type
simpleQualified : Color.RGB
simpleQualified = Color.RGB({r: 255, g: 0, b: 0})

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
getColor = |_| Color.RGB({r: 0, g: 255, b: 0})

# Function accepting qualified type
processColor : Color.RGB -> Str
processColor = |color|
	"Color processed"

# Multiple qualified types in a function signature
transform : Result.Result(Color.RGB, ExtMod.Error) -> ModuleA.ModuleB.TypeC
transform = |result|
	match result {
		Ok(rgb) => TypeC.fromColor(rgb)
		Err(err) => TypeC.default
	}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @15.1-15.16 (ident "simpleQualified"))
		(e-runtime-error (tag "ident_not_in_scope"))
		(annotation @15.1-15.16
			(declared-type
				(ty-lookup-external @14.19-14.28
					(ext-decl @14.19-14.28 (ident "Color.RGB") (kind "type"))))))
	(d-let
		(p-assign @19.1-19.17 (ident "aliasedQualified"))
		(e-runtime-error (tag "ident_not_in_scope"))
		(annotation @19.1-19.17
			(declared-type
				(ty-lookup-external @18.20-18.35
					(ext-decl @18.20-18.35 (ident "ExtMod.DataType") (kind "type"))))))
	(d-let
		(p-assign @23.1-23.20 (ident "multiLevelQualified"))
		(e-runtime-error (tag "ident_not_in_scope"))
		(annotation @23.1-23.20
			(declared-type
				(ty-lookup-external @22.23-22.44
					(ext-decl @22.23-22.44 (ident "ModuleA.ModuleB.TypeC") (kind "type"))))))
	(d-let
		(p-assign @27.1-27.11 (ident "resultType"))
		(e-nominal @27.14-27.20 (nominal "<malformed>")
			(e-tag @27.14-27.23 (name "Ok")
				(args
					(e-int @27.24-27.26 (value "42")))))
		(annotation @27.1-27.11
			(declared-type
				(ty-apply @26.14-26.37 (symbol "Result.Result")
					(ty @26.28-26.31 (name "I32"))
					(ty @26.33-26.36 (name "Str"))))))
	(d-let
		(p-assign @31.1-31.9 (ident "getColor"))
		(e-lambda @31.12-31.49
			(args
				(p-underscore @31.13-31.14))
			(e-runtime-error (tag "ident_not_in_scope")))
		(annotation @31.1-31.9
			(declared-type
				(ty-fn @30.12-30.27 (effectful false)
					(ty-record @30.12-30.14)
					(ty-lookup-external @30.18-30.27
						(ext-decl @30.18-30.27 (ident "Color.RGB") (kind "type")))))))
	(d-let
		(p-assign @35.1-35.13 (ident "processColor"))
		(e-lambda @35.16-36.22
			(args
				(p-assign @35.17-35.22 (ident "color")))
			(e-string @36.5-36.22
				(e-literal @36.6-36.21 (string "Color processed"))))
		(annotation @35.1-35.13
			(declared-type
				(ty-fn @34.16-34.32 (effectful false)
					(ty-lookup-external @34.16-34.25
						(ext-decl @34.16-34.25 (ident "Color.RGB") (kind "type")))
					(ty @34.29-34.32 (name "Str"))))))
	(d-let
		(p-assign @40.1-40.10 (ident "transform"))
		(e-lambda @40.13-44.6
			(args
				(p-assign @40.14-40.20 (ident "result")))
			(e-match @41.5-44.6
				(match @41.5-44.6
					(cond
						(e-lookup-local @41.11-41.17
							(p-assign @40.14-40.20 (ident "result"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag @42.9-42.23)))
							(value
								(e-call @42.27-42.47
									(e-runtime-error (tag "ident_not_in_scope"))
									(e-lookup-local @42.43-42.46
										(p-assign @42.19-42.22 (ident "rgb"))))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag @43.9-43.24)))
							(value
								(e-runtime-error (tag "ident_not_in_scope"))))))))
		(annotation @40.1-40.10
			(declared-type
				(ty-fn @39.13-39.76 (effectful false)
					(ty-apply @39.13-39.51 (symbol "Result.Result")
						(ty-lookup-external @39.27-39.36
							(ext-decl @39.27-39.36 (ident "Color.RGB") (kind "type")))
						(ty-lookup-external @39.38-39.50
							(ext-decl @39.38-39.50 (ident "ExtMod.Error") (kind "type"))))
					(ty-lookup-external @39.55-39.76
						(ext-decl @39.55-39.76 (ident "ModuleA.ModuleB.TypeC") (kind "type")))))))
	(s-import @9.1-9.13 (module "Color")
		(exposes))
	(s-import @10.1-10.15 (module "ModuleA")
		(exposes))
	(s-import @11.1-11.32 (module "ExternalModule") (alias "ExtMod")
		(exposes))
	(ext-decl @14.19-14.28 (ident "Color.RGB") (kind "type"))
	(ext-decl @18.20-18.35 (ident "ExtMod.DataType") (kind "type"))
	(ext-decl @22.23-22.44 (ident "ModuleA.ModuleB.TypeC") (kind "type"))
	(ext-decl @26.14-26.27 (ident "Result.Result") (kind "type"))
	(ext-decl @30.18-30.27 (ident "Color.RGB") (kind "type"))
	(ext-decl @34.16-34.25 (ident "Color.RGB") (kind "type"))
	(ext-decl @39.13-39.26 (ident "Result.Result") (kind "type"))
	(ext-decl @39.27-39.36 (ident "Color.RGB") (kind "type"))
	(ext-decl @39.38-39.50 (ident "ExtMod.Error") (kind "type"))
	(ext-decl @39.55-39.76 (ident "ModuleA.ModuleB.TypeC") (kind "type")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @15.1-15.16 (type "Error"))
		(patt @19.1-19.17 (type "Error"))
		(patt @23.1-23.20 (type "Error"))
		(patt @27.1-27.11 (type "Error"))
		(patt @31.1-31.9 (type "{  } -> Error"))
		(patt @35.1-35.13 (type "Color.RGB -> Str"))
		(patt @40.1-40.10 (type "Error -> Error")))
	(expressions
		(expr @15.19-15.24 (type "Error"))
		(expr @19.26-19.35 (type "Error"))
		(expr @23.23-23.32 (type "Error"))
		(expr @27.14-27.20 (type "Error"))
		(expr @31.12-31.49 (type "{  } -> Error"))
		(expr @35.16-36.22 (type "Color.RGB -> Str"))
		(expr @40.13-44.6 (type "Error -> Error"))))
~~~
