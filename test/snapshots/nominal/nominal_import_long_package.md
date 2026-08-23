# META
~~~ini
description=Example of importing a nominal tag union from a mod within a package, and renaming it using `as`
type=snippet
~~~
# SOURCE
~~~roc
import design.Styles.Color exposing [Encoder as CE]

red : CE
red = ... # not implemented
~~~
# EXPECTED
MOD NOT FOUND - nominal_import_long_package.md:3:7:3:9
# PROBLEMS
── ✗ mod not found ─────────────────────── nominal_import_long_package.md:3:7

This CE type is declared to be in design.Styles, which does not exist.

red : CE
      ^^

# TOKENS
~~~zig
KwImport,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,KwExposing,OpenSquare,UpperIdent,KwAs,UpperIdent,CloseSquare,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,TripleDot,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-import (raw "design.Styles.Color")
			(exposing
				(exposed-upper-ident (text "Encoder") (as "CE"))))
		(s-type-anno (name "red")
			(ty (name "CE")))
		(s-decl
			(p-ident (raw "red"))
			(e-ellipsis))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "red"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-malformed)))
	(s-import (mod "design.Styles")
		(exposes
			(exposed (name "Color") (alias "Color") (wildcard false))
			(exposed (name "Color.Encoder") (alias "CE") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
