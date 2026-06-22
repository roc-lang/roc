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
    Try,
    ExternalModule,
]

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
MODULE HEADER DEPRECATED - qualified_type_canonicalization.md:1:1:6:2
DUPLICATE DEFINITION - qualified_type_canonicalization.md:8:1:8:18
MODULE NOT FOUND - qualified_type_canonicalization.md:8:1:8:18
MODULE NOT FOUND - qualified_type_canonicalization.md:9:1:9:13
MODULE NOT FOUND - qualified_type_canonicalization.md:10:1:10:40
MODULE NOT FOUND - qualified_type_canonicalization.md:11:1:11:32
MODULE NOT FOUND - qualified_type_canonicalization.md:14:24:14:28
MODULE NOT FOUND - qualified_type_canonicalization.md:15:19:15:24
MODULE NOT FOUND - qualified_type_canonicalization.md:18:26:18:35
MODULE NOT FOUND - qualified_type_canonicalization.md:19:26:19:35
MODULE NOT FOUND - qualified_type_canonicalization.md:22:38:22:44
DOES NOT EXIST - qualified_type_canonicalization.md:23:23:23:32
MISSING NESTED TYPE - qualified_type_canonicalization.md:26:14:26:21
MODULE NOT FOUND - qualified_type_canonicalization.md:30:23:30:27
MODULE NOT FOUND - qualified_type_canonicalization.md:31:16:31:21
MODULE NOT FOUND - qualified_type_canonicalization.md:34:21:34:25
UNUSED VARIABLE - qualified_type_canonicalization.md:35:17:35:22
MISSING NESTED TYPE - qualified_type_canonicalization.md:39:13:39:20
MODULE NOT FOUND - qualified_type_canonicalization.md:39:26:39:30
MODULE NOT FOUND - qualified_type_canonicalization.md:39:38:39:44
MODULE NOT FOUND - qualified_type_canonicalization.md:39:64:39:70
UNDECLARED TYPE - qualified_type_canonicalization.md:42:9:42:12
DOES NOT EXIST - qualified_type_canonicalization.md:42:24:42:39
UNDECLARED TYPE - qualified_type_canonicalization.md:43:9:43:12
DOES NOT EXIST - qualified_type_canonicalization.md:43:25:43:38
UNUSED VARIABLE - qualified_type_canonicalization.md:43:17:43:20
EXPOSED BUT NOT DEFINED - qualified_type_canonicalization.md:3:5:3:26
EXPOSED BUT NOT DEFINED - qualified_type_canonicalization.md:4:5:4:8
# PROBLEMS
                                                    ┌──────────────────────────┐
┌─ The module header is deprecated. ────────────────┤ MODULE HEADER DEPRECATED │
│                                                   └─────────────────────────┬┘
│                                                                             │
│  module [                                                                   │
│      Color,                                                                 │
│      ModuleA.ModuleB.TypeC,                                                 │
│      Try,                                                                   │
│      ExternalModule,                                                        │
│  ]                                                                          │
└─────────────────────────────────────────────────────────────────────────────┘
    qualified_type_canonicalization.md:1:1

    Type modules (headerless files with a top-level type matching the filename)
    are now the preferred way to define modules.

    Remove the module header and ensure your file defines a type that matches
    the filename.
                                                        ┌──────────────────────┐
┌─ The name Try is being redeclared in this scope. ─────┤ DUPLICATE DEFINITION │
│                                                       └─────────────────────┬┘
│                                                                             │
│  import Basics.Try                                                          │
│  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                          │
└─────────────────────────────────────────────────────────────────────────────┘
    qualified_type_canonicalization.md:8:1

    The redeclaration is here:

    But Try was already defined here:
      ┌──────────────────────────────────────
      qualified_type_canonicalization.md:1:1
      │
    1 │ module [
      │ ^
                                                            ┌──────────────────┐
┌─ The module Basics was not found in this Roc project. ────┤ MODULE NOT FOUND │
│                                                           └─────────────────┬┘
│                                                                             │
│  import Basics.Try                                                          │
│  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                          │
└─────────────────────────────────────────────────────────────────────────────┘
    qualified_type_canonicalization.md:8:1

    You're attempting to use this module here:
                                                            ┌──────────────────┐
┌─ The module Color was not found in this Roc project. ─────┤ MODULE NOT FOUND │
│                                                           └─────────────────┬┘
│                                                                             │
│  import Color                                                               │
│  ‾‾‾‾‾‾‾‾‾‾‾‾                                                               │
└─────────────────────────────────────────────────────────────────────────────┘
    qualified_type_canonicalization.md:9:1

    You're attempting to use this module here:
                                                            ┌──────────────────┐
┌─ The module ModuleA was not found in this Roc project. ───┤ MODULE NOT FOUND │
│                                                           └─────────────────┬┘
│                                                                             │
│  import ModuleA.ModuleB exposing [TypeC]                                    │
│  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                    │
└─────────────────────────────────────────────────────────────────────────────┘
    qualified_type_canonicalization.md:10:1

    You're attempting to use this module here:
                                                            ┌──────────────────┐
┌─ The module ExternalModule was not found in this Roc ─────┤ MODULE NOT FOUND │
│  project.                                                 └─────────────────┬┘
│                                                                             │
│  import ExternalModule as ExtMod                                            │
│  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                            │
└─────────────────────────────────────────────────────────────────────────────┘
    qualified_type_canonicalization.md:11:1

    You're attempting to use this module here:
                                                            ┌──────────────────┐
┌─ The type RGB is qualified by the module Color, but that ─┤ MODULE NOT FOUND │
│  module was not found in this Roc project.                └─────────────────┬┘
│                                                                             │
│  simpleQualified : Color.RGB                                                │
│                         ‾‾‾‾                                                │
└─────────────────────────────────────────────────────────────────────────────┘
    qualified_type_canonicalization.md:14:24

    You're attempting to use this type here:
                                                            ┌──────────────────┐
┌─ The type RGB is qualified by the module Color, but that ─┤ MODULE NOT FOUND │
│  module was not found in this Roc project.                └─────────────────┬┘
│                                                                             │
│  simpleQualified = Color.RGB({ r: 255, g: 0, b: 0 })                        │
│                    ‾‾‾‾‾                                                    │
└─────────────────────────────────────────────────────────────────────────────┘
    qualified_type_canonicalization.md:15:19

    You're attempting to use this type here:
                                                            ┌──────────────────┐
┌─ The type DataType is qualified by the module ────────────┤ MODULE NOT FOUND │
│  ExternalModule, but that module was not found in this    └─────────────────┬┘
│  Roc project.                                                               │
│                                                                             │
│  aliasedQualified : ExtMod.DataType                                         │
│                           ‾‾‾‾‾‾‾‾‾                                         │
└─────────────────────────────────────────────────────────────────────────────┘
    qualified_type_canonicalization.md:18:26

    You're attempting to use this type here:
                                                            ┌──────────────────┐
┌─ The type DataType is qualified by the module ────────────┤ MODULE NOT FOUND │
│  ExternalModule, but that module was not found in this    └─────────────────┬┘
│  Roc project.                                                               │
│                                                                             │
│  aliasedQualified = ExtMod.DataType.Default                                 │
│                           ‾‾‾‾‾‾‾‾‾                                         │
└─────────────────────────────────────────────────────────────────────────────┘
    qualified_type_canonicalization.md:19:26

    You're attempting to use this type here:
                                                            ┌──────────────────┐
┌─ The type ModuleB.TypeC is qualified by the module ───────┤ MODULE NOT FOUND │
│  ModuleA, but that module was not found in this Roc       └─────────────────┬┘
│  project.                                                                   │
│                                                                             │
│  multiLevelQualified : ModuleA.ModuleB.TypeC                                │
│                                       ‾‾‾‾‾‾                                │
└─────────────────────────────────────────────────────────────────────────────┘
    qualified_type_canonicalization.md:22:38

    You're attempting to use this type here:
                                                              ┌────────────────┐
┌─ TypeC.new does not exist. ─────────────────────────────────┤ DOES NOT EXIST │
│                                                             └───────────────┬┘
│                                                                             │
│  multiLevelQualified = TypeC.new                                            │
│                        ‾‾‾‾‾‾‾‾‾                                            │
└─────────────────────────────────────────────────────────────────────────────┘
    qualified_type_canonicalization.md:23:23

                                                         ┌─────────────────────┐
┌─ Try is in scope, but it doesn't have a nested type ───┤ MISSING NESTED TYPE │
│  that's also named Try.                                └────────────────────┬┘
│                                                                             │
│  resultType : Try.Try(I32, Str)                                             │
│               ‾‾‾‾‾‾‾                                                       │
└─────────────────────────────────────────────────────────────────────────────┘
    qualified_type_canonicalization.md:26:14

    It's referenced here:
                                                            ┌──────────────────┐
┌─ The type RGB is qualified by the module Color, but that ─┤ MODULE NOT FOUND │
│  module was not found in this Roc project.                └─────────────────┬┘
│                                                                             │
│  getColor : {} -> Color.RGB                                                 │
│                        ‾‾‾‾                                                 │
└─────────────────────────────────────────────────────────────────────────────┘
    qualified_type_canonicalization.md:30:23

    You're attempting to use this type here:
                                                            ┌──────────────────┐
┌─ The type RGB is qualified by the module Color, but that ─┤ MODULE NOT FOUND │
│  module was not found in this Roc project.                └─────────────────┬┘
│                                                                             │
│  getColor = |_| Color.RGB({ r: 0, g: 255, b: 0 })                           │
│                 ‾‾‾‾‾                                                       │
└─────────────────────────────────────────────────────────────────────────────┘
    qualified_type_canonicalization.md:31:16

    You're attempting to use this type here:
                                                            ┌──────────────────┐
┌─ The type RGB is qualified by the module Color, but that ─┤ MODULE NOT FOUND │
│  module was not found in this Roc project.                └─────────────────┬┘
│                                                                             │
│  processColor : Color.RGB -> Str                                            │
│                      ‾‾‾‾                                                   │
└─────────────────────────────────────────────────────────────────────────────┘
    qualified_type_canonicalization.md:34:21

    You're attempting to use this type here:
                                                             ┌─────────────────┐
┌─ Variable color is defined here and then never used: ──────┤ UNUSED VARIABLE │
│                                                            └────────────────┬┘
│                                                                             │
│  processColor = |color|                                                     │
│                  ‾‾‾‾‾                                                      │
└─────────────────────────────────────────────────────────────────────────────┘
    qualified_type_canonicalization.md:35:17

    If you don't need this variable, prefix it with an underscore like _color
    to suppress this warning.
                                                         ┌─────────────────────┐
┌─ Try is in scope, but it doesn't have a nested type ───┤ MISSING NESTED TYPE │
│  that's also named Try.                                └────────────────────┬┘
│                                                                             │
│  transform : Try.Try(Color.RGB, ExtMod.Error) -> ModuleA.ModuleB.TypeC      │
│              ‾‾‾‾‾‾‾                                                        │
└─────────────────────────────────────────────────────────────────────────────┘
    qualified_type_canonicalization.md:39:13

    It's referenced here:
                                                            ┌──────────────────┐
┌─ The type RGB is qualified by the module Color, but that ─┤ MODULE NOT FOUND │
│  module was not found in this Roc project.                └─────────────────┬┘
│                                                                             │
│  transform : Try.Try(Color.RGB, ExtMod.Error) -> ModuleA.ModuleB.TypeC      │
│                           ‾‾‾‾                                              │
└─────────────────────────────────────────────────────────────────────────────┘
    qualified_type_canonicalization.md:39:26

    You're attempting to use this type here:
                                                            ┌──────────────────┐
┌─ The type Error is qualified by the module ───────────────┤ MODULE NOT FOUND │
│  ExternalModule, but that module was not found in this    └─────────────────┬┘
│  Roc project.                                                               │
│                                                                             │
│  transform : Try.Try(Color.RGB, ExtMod.Error) -> ModuleA.ModuleB.TypeC      │
│                                       ‾‾‾‾‾‾                                │
└─────────────────────────────────────────────────────────────────────────────┘
    qualified_type_canonicalization.md:39:38

    You're attempting to use this type here:
                                                            ┌──────────────────┐
┌─ The type ModuleB.TypeC is qualified by the module ───────┤ MODULE NOT FOUND │
│  ModuleA, but that module was not found in this Roc       └─────────────────┬┘
│  project.                                                                   │
│                                                                             │
│  transform : Try.Try(Color.RGB, ExtMod.Error) -> ModuleA.ModuleB.TypeC      │
│                                                                 ‾‾‾‾‾‾      │
└─────────────────────────────────────────────────────────────────────────────┘
    qualified_type_canonicalization.md:39:64

    You're attempting to use this type here:
                                                             ┌─────────────────┐
┌─ The type Try is not declared in this scope. ──────────────┤ UNDECLARED TYPE │
│                                                            └────────────────┬┘
│                                                                             │
│          Try.Ok(rgb) => TypeC.fromColor(rgb)                                │
│          ‾‾‾                                                                │
└─────────────────────────────────────────────────────────────────────────────┘
    qualified_type_canonicalization.md:42:9

    This type is referenced here:
                                                              ┌────────────────┐
┌─ TypeC.fromColor does not exist. ───────────────────────────┤ DOES NOT EXIST │
│                                                             └───────────────┬┘
│                                                                             │
│          Try.Ok(rgb) => TypeC.fromColor(rgb)                                │
│                         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                     │
└─────────────────────────────────────────────────────────────────────────────┘
    qualified_type_canonicalization.md:42:24

                                                             ┌─────────────────┐
┌─ The type Try is not declared in this scope. ──────────────┤ UNDECLARED TYPE │
│                                                            └────────────────┬┘
│                                                                             │
│          Try.Err(err) => TypeC.default                                      │
│          ‾‾‾                                                                │
└─────────────────────────────────────────────────────────────────────────────┘
    qualified_type_canonicalization.md:43:9

    This type is referenced here:
                                                              ┌────────────────┐
┌─ TypeC.default does not exist. ─────────────────────────────┤ DOES NOT EXIST │
│                                                             └───────────────┬┘
│                                                                             │
│          Try.Err(err) => TypeC.default                                      │
│                          ‾‾‾‾‾‾‾‾‾‾‾‾‾                                      │
└─────────────────────────────────────────────────────────────────────────────┘
    qualified_type_canonicalization.md:43:25

                                                             ┌─────────────────┐
┌─ Variable err is defined here and then never used: ────────┤ UNUSED VARIABLE │
│                                                            └────────────────┬┘
│                                                                             │
│          Try.Err(err) => TypeC.default                                      │
│                  ‾‾‾                                                        │
└─────────────────────────────────────────────────────────────────────────────┘
    qualified_type_canonicalization.md:43:17

    If you don't need this variable, prefix it with an underscore like _err to
    suppress this warning.
                                                     ┌─────────────────────────┐
┌─ The module header says that .TypeC is exposed, ───┤ EXPOSED BUT NOT DEFINED │
│  but it is not defined anywhere in this module.    └────────────────────────┬┘
│                                                                             │
│      ModuleA.ModuleB.TypeC,                                                 │
│      ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                  │
└─────────────────────────────────────────────────────────────────────────────┘
    qualified_type_canonicalization.md:3:5

    You can fix this by either defining .TypeC in this module, or by removing
    it from the list of exposed values.
                                                     ┌─────────────────────────┐
┌─ The module header says that Try is exposed, but ──┤ EXPOSED BUT NOT DEFINED │
│  it is not defined anywhere in this module.        └────────────────────────┬┘
│                                                                             │
│      Try,                                                                   │
│      ‾‾‾                                                                    │
└─────────────────────────────────────────────────────────────────────────────┘
    qualified_type_canonicalization.md:4:5

    You can fix this by either defining Try in this module, or by removing it
    from the list of exposed values.
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
	(module
		(exposes
			(exposed-upper-ident (text "Color"))
			(exposed-upper-ident (text "TypeC"))
			(exposed-upper-ident (text "Try"))
			(exposed-upper-ident (text "ExternalModule"))))
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
module [
	Color,
	TypeC,
	Try,
	ExternalModule,
]

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
										(p-runtime-error (tag "undeclared_type"))))
								(value
									(e-call
										(e-runtime-error (tag "qualified_ident_does_not_exist"))
										(e-lookup-local
											(p-assign (ident "rgb"))))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-runtime-error (tag "undeclared_type"))))
								(value
									(e-runtime-error (tag "qualified_ident_does_not_exist")))))))))
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
