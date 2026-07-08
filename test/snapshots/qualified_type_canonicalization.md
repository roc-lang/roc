# META
~~~ini
description=Qualified type canonicalization
type=file
~~~
# SOURCE
~~~roc
import Basics.Try
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
resultType : Try.Try(I32, Str)
resultType = Try.Ok(42)

# Function returning qualified type
getColor : {} -> Color.RGB
getColor = |_| Color.RGB({ r: 0, g: 255, b: 0 })

# Function accepting qualified type
processColor : Color.RGB -> Str
processColor = |color|
    "Color processed"

# Multiple qualified types in a function signature
transform : Try.Try(Color.RGB, ExtMod.Error) -> ModuleA.ModuleB.TypeC
transform = |result|
    match result {
        Try.Ok(rgb) => TypeC.fromColor(rgb)
        Try.Err(err) => TypeC.default
    }
~~~
# EXPECTED
DUPLICATE DEFINITION - qualified_type_canonicalization.md:1:1:1:18
MODULE NOT FOUND - qualified_type_canonicalization.md:1:1:1:18
MODULE NOT FOUND - qualified_type_canonicalization.md:2:1:2:13
MODULE NOT FOUND - qualified_type_canonicalization.md:3:1:3:40
MODULE NOT FOUND - qualified_type_canonicalization.md:4:1:4:32
MODULE NOT FOUND - qualified_type_canonicalization.md:7:24:7:28
MODULE NOT FOUND - qualified_type_canonicalization.md:8:19:8:24
MODULE NOT FOUND - qualified_type_canonicalization.md:11:26:11:35
MODULE NOT FOUND - qualified_type_canonicalization.md:12:26:12:35
MODULE NOT FOUND - qualified_type_canonicalization.md:15:38:15:44
DOES NOT EXIST - qualified_type_canonicalization.md:16:23:16:32
MISSING NESTED TYPE - qualified_type_canonicalization.md:19:14:19:21
MODULE NOT FOUND - qualified_type_canonicalization.md:23:23:23:27
MODULE NOT FOUND - qualified_type_canonicalization.md:24:16:24:21
MODULE NOT FOUND - qualified_type_canonicalization.md:27:21:27:25
UNUSED VARIABLE - qualified_type_canonicalization.md:28:17:28:22
MISSING NESTED TYPE - qualified_type_canonicalization.md:32:13:32:20
MODULE NOT FOUND - qualified_type_canonicalization.md:32:26:32:30
MODULE NOT FOUND - qualified_type_canonicalization.md:32:38:32:44
MODULE NOT FOUND - qualified_type_canonicalization.md:32:64:32:70
DOES NOT EXIST - qualified_type_canonicalization.md:35:24:35:39
DOES NOT EXIST - qualified_type_canonicalization.md:36:25:36:38
UNUSED VARIABLE - qualified_type_canonicalization.md:36:17:36:20
# PROBLEMS

┌──────────────────────┐
│ DUPLICATE DEFINITION ├─ The name `Try` is being redeclared here. ───────────┐
└┬─────────────────────┘                                                      │
 │                                                                            │
 │  import Basics.Try                                                         │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                         │
 └──────────────────────────────────── qualified_type_canonicalization.md:1:1 ┘

    In this scope, `Try` was already defined here:
      ┌───────────────────────────────────────────────────────────────────────┐
    1 │  import Basics.Try                                                    │
      │  ‾                                                                    │
      └─────────────────────────────── qualified_type_canonicalization.md:1:1 ┘


┌──────────────────┐
│ MODULE NOT FOUND ├─ The module `Basics` was not found in this Roc project. ─┐
└┬─────────────────┘                                                          │
 │                                                                            │
 │  import Basics.Try                                                         │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                         │
 └──────────────────────────────────── qualified_type_canonicalization.md:1:1 ┘



┌──────────────────┐
│ MODULE NOT FOUND ├─ The module `Color` was not found in this Roc project. ──┐
└┬─────────────────┘                                                          │
 │                                                                            │
 │  import Color                                                              │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾                                                              │
 └──────────────────────────────────── qualified_type_canonicalization.md:2:1 ┘



┌──────────────────┐
│ MODULE NOT FOUND ├─ The module `ModuleA` was not found in this Roc ─────────┐
└┬─────────────────┘  project.                                                │
 │                                                                            │
 │  import ModuleA.ModuleB exposing [TypeC]                                   │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                   │
 └──────────────────────────────────── qualified_type_canonicalization.md:3:1 ┘



┌──────────────────┐
│ MODULE NOT FOUND ├─ The module `ExternalModule` was not found in this Roc ──┐
└┬─────────────────┘  project.                                                │
 │                                                                            │
 │  import ExternalModule as ExtMod                                           │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                           │
 └──────────────────────────────────── qualified_type_canonicalization.md:4:1 ┘



┌──────────────────┐
│ MODULE NOT FOUND ├─ This `RGB` type is declared to be in `Color`, which ────┐
└┬─────────────────┘  does not exist.                                         │
 │                                                                            │
 │  simpleQualified : Color.RGB                                               │
 │                         ‾‾‾‾                                               │
 └─────────────────────────────────── qualified_type_canonicalization.md:7:24 ┘



┌──────────────────┐
│ MODULE NOT FOUND ├─ This `RGB` type is declared to be in `Color`, which ────┐
└┬─────────────────┘  does not exist.                                         │
 │                                                                            │
 │  simpleQualified = Color.RGB({ r: 255, g: 0, b: 0 })                       │
 │                    ‾‾‾‾‾                                                   │
 └─────────────────────────────────── qualified_type_canonicalization.md:8:19 ┘



┌──────────────────┐
│ MODULE NOT FOUND ├─ This `DataType` type is declared to be in ──────────────┐
└┬─────────────────┘  `ExternalModule`, which does not exist.                 │
 │                                                                            │
 │  aliasedQualified : ExtMod.DataType                                        │
 │                           ‾‾‾‾‾‾‾‾‾                                        │
 └────────────────────────────────── qualified_type_canonicalization.md:11:26 ┘



┌──────────────────┐
│ MODULE NOT FOUND ├─ This `DataType` type is declared to be in ──────────────┐
└┬─────────────────┘  `ExternalModule`, which does not exist.                 │
 │                                                                            │
 │  aliasedQualified = ExtMod.DataType.Default                                │
 │                           ‾‾‾‾‾‾‾‾‾                                        │
 └────────────────────────────────── qualified_type_canonicalization.md:12:26 ┘



┌──────────────────┐
│ MODULE NOT FOUND ├─ This `ModuleB.TypeC` type is declared to be in ─────────┐
└┬─────────────────┘  `ModuleA`, which does not exist.                        │
 │                                                                            │
 │  multiLevelQualified : ModuleA.ModuleB.TypeC                               │
 │                                       ‾‾‾‾‾‾                               │
 └────────────────────────────────── qualified_type_canonicalization.md:15:38 ┘



┌────────────────┐
│ DOES NOT EXIST ├─ `TypeC.new` does not exist. ──────────────────────────────┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  multiLevelQualified = TypeC.new                                           │
 │                        ‾‾‾‾‾‾‾‾‾                                           │
 └────────────────────────────────── qualified_type_canonicalization.md:16:23 ┘



┌─────────────────────┐
│ MISSING NESTED TYPE ├─ `Try` is in scope, but it doesn't have a nested ─────┐
└┬────────────────────┘  type that's also named `Try`.                        │
 │                                                                            │
 │  resultType : Try.Try(I32, Str)                                            │
 │               ‾‾‾‾‾‾‾                                                      │
 └────────────────────────────────── qualified_type_canonicalization.md:19:14 ┘



┌──────────────────┐
│ MODULE NOT FOUND ├─ This `RGB` type is declared to be in `Color`, which ────┐
└┬─────────────────┘  does not exist.                                         │
 │                                                                            │
 │  getColor : {} -> Color.RGB                                                │
 │                        ‾‾‾‾                                                │
 └────────────────────────────────── qualified_type_canonicalization.md:23:23 ┘



┌──────────────────┐
│ MODULE NOT FOUND ├─ This `RGB` type is declared to be in `Color`, which ────┐
└┬─────────────────┘  does not exist.                                         │
 │                                                                            │
 │  getColor = |_| Color.RGB({ r: 0, g: 255, b: 0 })                          │
 │                 ‾‾‾‾‾                                                      │
 └────────────────────────────────── qualified_type_canonicalization.md:24:16 ┘



┌──────────────────┐
│ MODULE NOT FOUND ├─ This `RGB` type is declared to be in `Color`, which ────┐
└┬─────────────────┘  does not exist.                                         │
 │                                                                            │
 │  processColor : Color.RGB -> Str                                           │
 │                      ‾‾‾‾                                                  │
 └────────────────────────────────── qualified_type_canonicalization.md:27:21 ┘



┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `color` is defined here and then never used. ───┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  processColor = |color|                                                    │
 │                  ‾‾‾‾‾                                                     │
 └────────────────────────────────── qualified_type_canonicalization.md:28:17 ┘

    If you don't need this variable, prefix it with an underscore like `_color`
    to suppress this warning.


┌─────────────────────┐
│ MISSING NESTED TYPE ├─ `Try` is in scope, but it doesn't have a nested ─────┐
└┬────────────────────┘  type that's also named `Try`.                        │
 │                                                                            │
 │  transform : Try.Try(Color.RGB, ExtMod.Error) -> ModuleA.ModuleB.TypeC     │
 │              ‾‾‾‾‾‾‾                                                       │
 └────────────────────────────────── qualified_type_canonicalization.md:32:13 ┘



┌──────────────────┐
│ MODULE NOT FOUND ├─ This `RGB` type is declared to be in `Color`, which ────┐
└┬─────────────────┘  does not exist.                                         │
 │                                                                            │
 │  transform : Try.Try(Color.RGB, ExtMod.Error) -> ModuleA.ModuleB.TypeC     │
 │                           ‾‾‾‾                                             │
 └────────────────────────────────── qualified_type_canonicalization.md:32:26 ┘



┌──────────────────┐
│ MODULE NOT FOUND ├─ This `Error` type is declared to be in ─────────────────┐
└┬─────────────────┘  `ExternalModule`, which does not exist.                 │
 │                                                                            │
 │  transform : Try.Try(Color.RGB, ExtMod.Error) -> ModuleA.ModuleB.TypeC     │
 │                                       ‾‾‾‾‾‾                               │
 └────────────────────────────────── qualified_type_canonicalization.md:32:38 ┘



┌──────────────────┐
│ MODULE NOT FOUND ├─ This `ModuleB.TypeC` type is declared to be in ─────────┐
└┬─────────────────┘  `ModuleA`, which does not exist.                        │
 │                                                                            │
 │  transform : Try.Try(Color.RGB, ExtMod.Error) -> ModuleA.ModuleB.TypeC     │
 │                                                                 ‾‾‾‾‾‾     │
 └────────────────────────────────── qualified_type_canonicalization.md:32:64 ┘



┌────────────────┐
│ DOES NOT EXIST ├─ `TypeC.fromColor` does not exist. ────────────────────────┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  Try.Ok(rgb) => TypeC.fromColor(rgb)                                       │
 │                 ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                            │
 └────────────────────────────────── qualified_type_canonicalization.md:35:24 ┘



┌────────────────┐
│ DOES NOT EXIST ├─ `TypeC.default` does not exist. ──────────────────────────┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  Try.Err(err) => TypeC.default                                             │
 │                  ‾‾‾‾‾‾‾‾‾‾‾‾‾                                             │
 └────────────────────────────────── qualified_type_canonicalization.md:36:25 ┘



┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `err` is defined here and then never used. ─────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  Try.Err(err) => TypeC.default                                             │
 │          ‾‾‾                                                               │
 └────────────────────────────────── qualified_type_canonicalization.md:36:17 ┘

    If you don't need this variable, prefix it with an underscore like `_err`
    to suppress this warning.

# TOKENS
~~~zig
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
	(type-module)
	(statements
		(s-import (raw "Basics")
			(exposing
				(exposed-upper-ident (text "Try"))))
		(s-import (raw "Color"))
		(s-import (raw "ModuleA.ModuleB")
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
				(ty (name "Try.Try"))
				(ty (name "I32"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "resultType"))
			(e-apply
				(e-tag (raw "Try.Ok"))
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
					(ty (name "Try.Try"))
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
import Basics exposing [Try]
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
resultType : Try.Try(I32, Str)
resultType = Try.Ok(42)

# Function returning qualified type
getColor : {} -> Color.RGB
getColor = |_| Color.RGB({ r: 0, g: 255, b: 0 })

# Function accepting qualified type
processColor : Color.RGB -> Str
processColor = |color|
	"Color processed"

# Multiple qualified types in a function signature
transform : Try.Try(Color.RGB, ExtMod.Error) -> ModuleA.ModuleB.TypeC
transform = |result|
	match result {
		Try.Ok(rgb) => TypeC.fromColor(rgb)
		Try.Err(err) => TypeC.default
	}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "simpleQualified"))
		(e-runtime-error (tag "type_from_missing_module"))
		(annotation
			(ty-malformed)))
	(d-let
		(p-assign (ident "aliasedQualified"))
		(e-runtime-error (tag "type_from_missing_module"))
		(annotation
			(ty-malformed)))
	(d-let
		(p-assign (ident "multiLevelQualified"))
		(e-runtime-error (tag "qualified_ident_does_not_exist"))
		(annotation
			(ty-malformed)))
	(d-let
		(p-assign (ident "resultType"))
		(e-nominal-external
			(builtin)
			(e-tag (name "Ok")
				(args
					(e-num (value "42")))))
		(annotation
			(ty-malformed)))
	(d-let
		(p-assign (ident "getColor"))
		(e-lambda
			(args
				(p-underscore))
			(e-runtime-error (tag "type_from_missing_module")))
		(annotation
			(ty-fn (effectful false)
				(ty-record)
				(ty-malformed))))
	(d-let
		(p-assign (ident "processColor"))
		(e-lambda
			(args
				(p-assign (ident "color")))
			(e-string
				(e-literal (string "Color processed"))))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "transform"))
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
									(p-nominal-external (builtin)
										(p-applied-tag))))
							(value
								(e-call
									(e-runtime-error (tag "qualified_ident_does_not_exist"))
									(e-lookup-local
										(p-assign (ident "rgb"))))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-nominal-external (builtin)
										(p-applied-tag))))
							(value
								(e-runtime-error (tag "qualified_ident_does_not_exist"))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-malformed))))
	(s-import (module "Basics")
		(exposes
			(exposed (name "Try") (wildcard false))))
	(s-import (module "Color")
		(exposes))
	(s-import (module "ModuleA")
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
		(patt (type "{} -> Error"))
		(patt (type "Error -> Str"))
		(patt (type "Error -> Error")))
	(expressions
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "{} -> Error"))
		(expr (type "Error -> Str"))
		(expr (type "Error -> Error"))))
~~~
