# META
~~~ini
description=Comprehensive test of type variable underscore conventions
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# Test 1: UNUSED TYPE VARIABLE NAME - single-use variable should start with underscore
single_use : List(elem) -> Str
single_use = |x| "hello"

# Test 2: TYPE VAR ENDING IN UNDERSCORE - variables should never end with underscore
ending_underscore : List(elem_) -> elem_
ending_underscore = |list| "default"

# Test 3: COMBINATION - single-use ending in underscore (both errors)
combo_single : List(bad_) -> Str
combo_single = |x| "combo"

# Test 4: VALID CASES - these should not generate warnings
valid_single : List(_elem) -> Str
valid_single = |x| "valid"

valid_multi : elem -> List(elem)
valid_multi = |x| [x]

main = |x| "done"
~~~
# EXPECTED
UNUSED VARIABLE - type_var_underscore_conventions.md:5:15:5:16
UNUSED VARIABLE - type_var_underscore_conventions.md:9:22:9:26
UNUSED VARIABLE - type_var_underscore_conventions.md:13:17:13:18
UNUSED VARIABLE - type_var_underscore_conventions.md:17:17:17:18
UNUSED VARIABLE - type_var_underscore_conventions.md:22:9:22:10
TYPE MISMATCH - type_var_underscore_conventions.md:9:28:9:37
# PROBLEMS
**UNUSED VARIABLE**
Variable `x` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:
**type_var_underscore_conventions.md:5:15:5:16:**
```roc
single_use = |x| "hello"
```
              ^


**UNUSED VARIABLE**
Variable `list` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_list` to suppress this warning.
The unused variable is declared here:
**type_var_underscore_conventions.md:9:22:9:26:**
```roc
ending_underscore = |list| "default"
```
                     ^^^^


**UNUSED VARIABLE**
Variable `x` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:
**type_var_underscore_conventions.md:13:17:13:18:**
```roc
combo_single = |x| "combo"
```
                ^


**UNUSED VARIABLE**
Variable `x` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:
**type_var_underscore_conventions.md:17:17:17:18:**
```roc
valid_single = |x| "valid"
```
                ^


**UNUSED VARIABLE**
Variable `x` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:
**type_var_underscore_conventions.md:22:9:22:10:**
```roc
main = |x| "done"
```
        ^


**TYPE MISMATCH**
This expression is used in an unexpected way:
**type_var_underscore_conventions.md:9:28:9:37:**
```roc
ending_underscore = |list| "default"
```
                           ^^^^^^^^^

It has the type:
    _Str_

But the type annotation says it should have the type:
    _elem__

# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,StringStart,StringPart,StringEnd,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpArrow,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,StringStart,StringPart,StringEnd,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,StringStart,StringPart,StringEnd,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,NamedUnderscore,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,StringStart,StringPart,StringEnd,
LowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenSquare,LowerIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides
			(exposed-lower-ident
				(text "main")))
		(record-field (name "pf")
			(e-string
				(e-string-part (raw "../basic-cli/platform.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-type-anno (name "single_use")
			(ty-fn
				(ty-apply
					(ty (name "List"))
					(ty-var (raw "elem")))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "single_use"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-string
					(e-string-part (raw "hello")))))
		(s-type-anno (name "ending_underscore")
			(ty-fn
				(ty-apply
					(ty (name "List"))
					(ty-var (raw "elem_")))
				(ty-var (raw "elem_"))))
		(s-decl
			(p-ident (raw "ending_underscore"))
			(e-lambda
				(args
					(p-ident (raw "list")))
				(e-string
					(e-string-part (raw "default")))))
		(s-type-anno (name "combo_single")
			(ty-fn
				(ty-apply
					(ty (name "List"))
					(ty-var (raw "bad_")))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "combo_single"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-string
					(e-string-part (raw "combo")))))
		(s-type-anno (name "valid_single")
			(ty-fn
				(ty-apply
					(ty (name "List"))
					(underscore-ty-var (raw "_elem")))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "valid_single"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-string
					(e-string-part (raw "valid")))))
		(s-type-anno (name "valid_multi")
			(ty-fn
				(ty-var (raw "elem"))
				(ty-apply
					(ty (name "List"))
					(ty-var (raw "elem")))))
		(s-decl
			(p-ident (raw "valid_multi"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-list
					(e-ident (raw "x")))))
		(s-decl
			(p-ident (raw "main"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-string
					(e-string-part (raw "done")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "single_use"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-string
				(e-literal (string "hello"))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-apply (name "List") (builtin)
						(ty-rigid-var (name "elem")))
					(ty-lookup (name "Str") (builtin))))))
	(d-let
		(p-assign (ident "ending_underscore"))
		(e-lambda
			(args
				(p-assign (ident "list")))
			(e-string
				(e-literal (string "default"))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-apply (name "List") (builtin)
						(ty-rigid-var (name "elem_")))
					(ty-rigid-var-lookup (ty-rigid-var (name "elem_")))))))
	(d-let
		(p-assign (ident "combo_single"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-string
				(e-literal (string "combo"))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-apply (name "List") (builtin)
						(ty-rigid-var (name "bad_")))
					(ty-lookup (name "Str") (builtin))))))
	(d-let
		(p-assign (ident "valid_single"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-string
				(e-literal (string "valid"))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-apply (name "List") (builtin)
						(ty-rigid-var (name "_elem")))
					(ty-lookup (name "Str") (builtin))))))
	(d-let
		(p-assign (ident "valid_multi"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-list
				(elems
					(e-lookup-local
						(p-assign (ident "x"))))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-rigid-var (name "elem"))
					(ty-apply (name "List") (builtin)
						(ty-rigid-var-lookup (ty-rigid-var (name "elem"))))))))
	(d-let
		(p-assign (ident "main"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-string
				(e-literal (string "done"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(elem) -> Str"))
		(patt (type "List(elem_) -> Error"))
		(patt (type "List(bad_) -> Str"))
		(patt (type "List(_elem) -> Str"))
		(patt (type "elem -> List(elem)"))
		(patt (type "_arg -> Str")))
	(expressions
		(expr (type "List(elem) -> Str"))
		(expr (type "List(elem_) -> Error"))
		(expr (type "List(bad_) -> Str"))
		(expr (type "List(_elem) -> Str"))
		(expr (type "elem -> List(elem)"))
		(expr (type "_arg -> Str"))))
~~~
