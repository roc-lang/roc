# META
~~~ini
description=Example of mixed local and external nominal types in same scope
type=file
~~~
# SOURCE
~~~roc
module [LocalStatus, processColor]

LocalStatus := [Pending, Complete]

processColor : _ -> LocalStatus
processColor = |color| {

    # bring RGB into scope
    import Color.RGB

    match color {
        RGB.Red => LocalStatus.Pending
        RGB.Green => LocalStatus.Complete
        RGB.Blue => LocalStatus.Pending
    }
}
~~~
# PROBLEMS
**IMPORT MUST BE TOP LEVEL**
Import statements must appear at the top level of a module.
Move this import to the top of the file, after the module header but before any definitions.

Here is the problematic code:
**nominal_mixed_scope.md:9:5:9:17:**
```roc
    import Color.RGB
```
    ^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_mixed_scope.md:9:17:9:17:**
```roc
    import Color.RGB
```
                


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.Red =>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_mixed_scope.md:12:12:12:19:**
```roc
        RGB.Red => LocalStatus.Pending
```
           ^^^^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **=> LocalStatus** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**nominal_mixed_scope.md:12:17:12:31:**
```roc
        RGB.Red => LocalStatus.Pending
```
                ^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token  is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**nominal_mixed_scope.md:12:31:12:31:**
```roc
        RGB.Red => LocalStatus.Pending
```
                              


**UNEXPECTED TOKEN IN EXPRESSION**
The token **module [LocalStatus, processColor]

LocalStatus := [Pending, Complete]

processColor : _ -> LocalStatus
processColor = |color| {

    # bring RGB into scope
    import Color.RGB

    match color {
        RGB.Red => LocalStatus.Pending
        RGB** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_mixed_scope.md:1:1:13:12:**
```roc
module [LocalStatus, processColor]

LocalStatus := [Pending, Complete]

processColor : _ -> LocalStatus
processColor = |color| {

    # bring RGB into scope
    import Color.RGB

    match color {
        RGB.Red => LocalStatus.Pending
        RGB.Green => LocalStatus.Complete
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.Green =>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_mixed_scope.md:13:12:13:21:**
```roc
        RGB.Green => LocalStatus.Complete
```
           ^^^^^^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **=> LocalStatus** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**nominal_mixed_scope.md:13:19:13:33:**
```roc
        RGB.Green => LocalStatus.Complete
```
                  ^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token  is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**nominal_mixed_scope.md:13:33:13:33:**
```roc
        RGB.Green => LocalStatus.Complete
```
                                


**UNEXPECTED TOKEN IN EXPRESSION**
The token **module [LocalStatus, processColor]

LocalStatus := [Pending, Complete]

processColor : _ -> LocalStatus
processColor = |color| {

    # bring RGB into scope
    import Color.RGB

    match color {
        RGB.Red => LocalStatus.Pending
        RGB.Green => LocalStatus.Complete
        RGB** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_mixed_scope.md:1:1:14:12:**
```roc
module [LocalStatus, processColor]

LocalStatus := [Pending, Complete]

processColor : _ -> LocalStatus
processColor = |color| {

    # bring RGB into scope
    import Color.RGB

    match color {
        RGB.Red => LocalStatus.Pending
        RGB.Green => LocalStatus.Complete
        RGB.Blue => LocalStatus.Pending
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.Blue =>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_mixed_scope.md:14:12:14:20:**
```roc
        RGB.Blue => LocalStatus.Pending
```
           ^^^^^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **=> LocalStatus** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**nominal_mixed_scope.md:14:18:14:32:**
```roc
        RGB.Blue => LocalStatus.Pending
```
                 ^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token  is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**nominal_mixed_scope.md:14:32:14:32:**
```roc
        RGB.Blue => LocalStatus.Pending
```
                               


**UNEXPECTED TOKEN IN EXPRESSION**
The token **module [LocalStatus, processColor]

LocalStatus := [Pending, Complete]

processColor : _ -> LocalStatus
processColor = |color| {

    # bring RGB into scope
    import Color.RGB

    match color {
        RGB.Red => LocalStatus.Pending
        RGB.Green => LocalStatus.Complete
        RGB.Blue => LocalStatus.Pending
    }** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_mixed_scope.md:1:1:15:6:**
```roc
module [LocalStatus, processColor]

LocalStatus := [Pending, Complete]

processColor : _ -> LocalStatus
processColor = |color| {

    # bring RGB into scope
    import Color.RGB

    match color {
        RGB.Red => LocalStatus.Pending
        RGB.Green => LocalStatus.Complete
        RGB.Blue => LocalStatus.Pending
    }
```


**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: statement type in block
Let us know if you want to help!

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:20),Comma(1:20-1:21),LowerIdent(1:22-1:34),CloseSquare(1:34-1:35),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(3:1-3:12),OpColonEqual(3:13-3:15),OpenSquare(3:16-3:17),UpperIdent(3:17-3:24),Comma(3:24-3:25),UpperIdent(3:26-3:34),CloseSquare(3:34-3:35),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(5:1-5:13),OpColon(5:14-5:15),Underscore(5:16-5:17),OpArrow(5:18-5:20),UpperIdent(5:21-5:32),Newline(1:1-1:1),
LowerIdent(6:1-6:13),OpAssign(6:14-6:15),OpBar(6:16-6:17),LowerIdent(6:17-6:22),OpBar(6:22-6:23),OpenCurly(6:24-6:25),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(8:6-8:27),
KwImport(9:5-9:11),UpperIdent(9:12-9:17),NoSpaceDotUpperIdent(9:17-9:21),Newline(1:1-1:1),
Newline(1:1-1:1),
KwMatch(11:5-11:10),LowerIdent(11:11-11:16),OpenCurly(11:17-11:18),Newline(1:1-1:1),
UpperIdent(12:9-12:12),NoSpaceDotUpperIdent(12:12-12:16),OpFatArrow(12:17-12:19),UpperIdent(12:20-12:31),NoSpaceDotUpperIdent(12:31-12:39),Newline(1:1-1:1),
UpperIdent(13:9-13:12),NoSpaceDotUpperIdent(13:12-13:18),OpFatArrow(13:19-13:21),UpperIdent(13:22-13:33),NoSpaceDotUpperIdent(13:33-13:42),Newline(1:1-1:1),
UpperIdent(14:9-14:12),NoSpaceDotUpperIdent(14:12-14:17),OpFatArrow(14:18-14:20),UpperIdent(14:21-14:32),NoSpaceDotUpperIdent(14:32-14:40),Newline(1:1-1:1),
CloseCurly(15:5-15:6),Newline(1:1-1:1),
CloseCurly(16:1-16:2),EndOfFile(16:2-16:2),
~~~
# PARSE
~~~clojure
(file @1.1-16.2
	(module @1.1-1.35
		(exposes @1.8-1.35
			(exposed-upper-ident (text "LocalStatus"))
			(exposed-lower-ident (text "processColor"))))
	(statements
		(s-type-decl @3.1-5.13
			(header @3.1-3.12 (name "LocalStatus")
				(args))
			(ty-tag-union @3.16-3.35
				(tags
					(ty (name "Pending"))
					(ty (name "Complete")))))
		(s-type-anno @5.1-6.13 (name "processColor")
			(ty-fn @5.16-5.32
				(_)
				(ty (name "LocalStatus"))))
		(s-decl @6.1-16.2
			(p-ident @6.1-6.13 (raw "processColor"))
			(e-lambda @6.16-16.2
				(args
					(p-ident @6.17-6.22 (raw "color")))
				(e-block @6.24-16.2
					(statements
						(s-malformed @9.5-9.17 (tag "import_must_be_top_level"))
						(e-tag @9.12-9.17 (raw "Color"))
						(e-malformed @1.1-1.1 (reason "expr_unexpected_token"))
						(e-match
							(e-ident @11.11-11.16 (qaul "") (raw "color"))
							(branches
								(branch @12.9-12.19
									(p-tag @12.9-12.12 (raw "RGB"))
									(e-malformed @12.12-12.19 (reason "expr_unexpected_token")))
								(branch @12.17-12.39
									(p-malformed @12.17-12.31 (tag "pattern_unexpected_token"))
									(e-tag @12.20-12.31 (raw "LocalStatus")))
								(branch @12.31-13.12
									(p-malformed @1.1-1.1 (tag "pattern_unexpected_token"))
									(e-malformed @1.1-13.12 (reason "expr_unexpected_token")))
								(branch @13.9-13.21
									(p-tag @13.9-13.12 (raw "RGB"))
									(e-malformed @13.12-13.21 (reason "expr_unexpected_token")))
								(branch @13.19-13.42
									(p-malformed @13.19-13.33 (tag "pattern_unexpected_token"))
									(e-tag @13.22-13.33 (raw "LocalStatus")))
								(branch @13.33-14.12
									(p-malformed @1.1-1.1 (tag "pattern_unexpected_token"))
									(e-malformed @1.1-14.12 (reason "expr_unexpected_token")))
								(branch @14.9-14.20
									(p-tag @14.9-14.12 (raw "RGB"))
									(e-malformed @14.12-14.20 (reason "expr_unexpected_token")))
								(branch @14.18-14.40
									(p-malformed @14.18-14.32 (tag "pattern_unexpected_token"))
									(e-tag @14.21-14.32 (raw "LocalStatus")))
								(branch @14.32-15.6
									(p-malformed @1.1-1.1 (tag "pattern_unexpected_token"))
									(e-malformed @1.1-15.6 (reason "expr_unexpected_token")))))))))))
~~~
# FORMATTED
~~~roc
module [LocalStatus, processColor]

LocalStatus : [Pending, Complete]

processColor : _ -> LocalStatus
processColor = |color| {

	# bring RGB into scope
	
	Color
	

	match color {
		RGB => 		 => LocalStatus		 => 
		RGB => 		 => LocalStatus		 => 
		RGB => 		 => LocalStatus		 => 
	}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.13 (ident "processColor"))
		(e-lambda @6.16-16.2
			(args
				(p-assign @6.17-6.22 (ident "color")))
			(e-block @6.24-16.2
				(s-expr @9.12-9.21
					(e-tag @9.12-9.17 (name "Color") (args "TODO")))
				(e-match @11.5-16.2
					(match @11.5-16.2
						(cond
							(e-lookup-local @11.11-11.16
								(pattern @6.17-6.22)))
						(branches
							(branch
								(patterns
									(p-applied-tag @12.9-12.12 (degenerate false)))
								(value
									(e-runtime-error (tag "expr_not_canonicalized"))))
							(branch
								(patterns
									(p-runtime-error @12.17-12.31 (tag "pattern_not_canonicalized") (degenerate false)))
								(value
									(e-tag @12.20-12.31 (name "LocalStatus") (args "TODO"))))
							(branch
								(patterns
									(p-runtime-error @1.1-1.1 (tag "pattern_not_canonicalized") (degenerate false)))
								(value
									(e-runtime-error (tag "expr_not_canonicalized"))))
							(branch
								(patterns
									(p-applied-tag @13.9-13.12 (degenerate false)))
								(value
									(e-runtime-error (tag "expr_not_canonicalized"))))
							(branch
								(patterns
									(p-runtime-error @13.19-13.33 (tag "pattern_not_canonicalized") (degenerate false)))
								(value
									(e-tag @13.22-13.33 (name "LocalStatus") (args "TODO"))))
							(branch
								(patterns
									(p-runtime-error @1.1-1.1 (tag "pattern_not_canonicalized") (degenerate false)))
								(value
									(e-runtime-error (tag "expr_not_canonicalized"))))
							(branch
								(patterns
									(p-applied-tag @14.9-14.12 (degenerate false)))
								(value
									(e-runtime-error (tag "expr_not_canonicalized"))))
							(branch
								(patterns
									(p-runtime-error @14.18-14.32 (tag "pattern_not_canonicalized") (degenerate false)))
								(value
									(e-tag @14.21-14.32 (name "LocalStatus") (args "TODO"))))
							(branch
								(patterns
									(p-runtime-error @1.1-1.1 (tag "pattern_not_canonicalized") (degenerate false)))
								(value
									(e-runtime-error (tag "expr_not_canonicalized")))))))))
		(annotation @6.1-6.13
			(declared-type
				(ty-fn @5.16-5.32 (effectful false)
					(ty-underscore @5.16-5.17)
					(ty @5.21-5.32 (name "LocalStatus"))))))
	(s-type-decl @3.1-5.13
		(ty-header @3.1-3.12 (name "LocalStatus"))
		(ty-tag-union @3.16-3.35
			(ty @3.17-3.24 (name "Pending"))
			(ty @3.26-3.34 (name "Complete")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.13 (type "Error -> Error")))
	(expressions
		(expr @6.16-16.2 (type "Error -> Error"))))
~~~
