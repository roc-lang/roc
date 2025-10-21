# META
~~~ini
description=Example of a nominal tag union import from a package
type=snippet
~~~
# SOURCE
~~~roc
# import the Color module from styles package as CC
import styles.Color as CC

# instantiating an RGB nominal tab union from the styles.Color module
blue : CC.Color
blue = CC.Color.RGB(0,0,255)
~~~
# EXPECTED
MODULE NOT FOUND - nominal_tag_package_import.md:2:1:2:26
# PROBLEMS
**MODULE NOT FOUND**
The module `styles.Color` was not found in this Roc project.

You're attempting to use this module here:
**nominal_tag_package_import.md:2:1:2:26:**
```roc
import styles.Color as CC
```
^^^^^^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwImport,LowerIdent,NoSpaceDotUpperIdent,KwAs,UpperIdent,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,Int,Comma,Int,Comma,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-import (raw "styles.Color") (alias "CC"))
		(s-type-anno (name "blue")
			(ty (name "CC.Color")))
		(s-decl
			(p-ident (raw "blue"))
			(e-apply
				(e-tag (raw "CC.Color.RGB"))
				(e-int (raw "0"))
				(e-int (raw "0"))
				(e-int (raw "255"))))))
~~~
# FORMATTED
~~~roc
# import the Color module from styles package as CC
import styles.Color as CC

# instantiating an RGB nominal tab union from the styles.Color module
blue : CC.Color
blue = CC.Color.RGB(0, 0, 255)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "blue"))
		(e-nominal-external
			(module-idx "4")
			(target-node-idx "0")
			(e-tag (name "RGB")
				(args
					(e-num (value "0"))
					(e-num (value "0"))
					(e-num (value "255")))))
		(annotation
			(declared-type
				(ty-lookup (name "Color") (external (module-idx "4") (target-node-idx "0"))))))
	(s-import (module "styles.Color")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
