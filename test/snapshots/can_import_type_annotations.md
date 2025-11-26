# META
~~~ini
description=Import types and use in type annotations
type=snippet
~~~
# SOURCE
~~~roc
import http.Client as Http exposing [Request, Response]
import json.Json
import utils.Try exposing [Try]

processRequest : Request -> Response
processRequest = |req| Http.defaultResponse

parseJson : Str -> Json.Value
parseJson = |input| Json.parse(input)

handleApi : Http.Request -> Try(Http.Response, Json.Error)
handleApi = |request| {
    result = Json.decode(request.body)
    match result {
        Ok(data) => Ok(Http.success(data))
        Err(err) => Err(err)
    }
}

config : Json.Config
config = Json.defaultConfig

# Test nested type qualification
advancedParser : Json.Parser.Config, Str -> Try(Json.Value, Json.Parser.Error)
advancedParser = |parserConfig, input| Json.Parser.parseWith(parserConfig, input)

# Test function with multiple type parameters
combineTrys : Try(a, err), Try(b, err) -> Try((a, b), err)
combineTrys = |result1, result2|
    match result1 {
        Ok(value1) =>
            match(result2) {
                Ok(value2) => Ok((value1, value2))
                Err(err) => Err(err)
            }
        Err(err) => Err(err)
    }
~~~
# EXPECTED
DUPLICATE DEFINITION - can_import_type_annotations.md:3:1:3:32
UNDECLARED TYPE - can_import_type_annotations.md:5:18:5:25
UNDECLARED TYPE - can_import_type_annotations.md:5:29:5:37
UNDEFINED VARIABLE - can_import_type_annotations.md:6:24:6:44
UNUSED VARIABLE - can_import_type_annotations.md:6:19:6:22
UNDEFINED VARIABLE - can_import_type_annotations.md:9:21:9:31
UNDEFINED VARIABLE - can_import_type_annotations.md:13:14:13:25
UNDEFINED VARIABLE - can_import_type_annotations.md:15:24:15:36
UNDEFINED VARIABLE - can_import_type_annotations.md:21:10:21:28
MODULE NOT IMPORTED - can_import_type_annotations.md:24:18:24:36
MODULE NOT IMPORTED - can_import_type_annotations.md:24:61:24:78
UNDEFINED VARIABLE - can_import_type_annotations.md:25:40:25:61
# PROBLEMS
**DUPLICATE DEFINITION**
The name `Try` is being redeclared in this scope.

The redeclaration is here:
**can_import_type_annotations.md:3:1:3:32:**
```roc
import utils.Try exposing [Try]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

But `Try` was already defined here:
**can_import_type_annotations.md:1:1:1:1:**
```roc
import http.Client as Http exposing [Request, Response]
```
^


**UNDECLARED TYPE**
The type _Request_ is not declared in this scope.

This type is referenced here:
**can_import_type_annotations.md:5:18:5:25:**
```roc
processRequest : Request -> Response
```
                 ^^^^^^^


**UNDECLARED TYPE**
The type _Response_ is not declared in this scope.

This type is referenced here:
**can_import_type_annotations.md:5:29:5:37:**
```roc
processRequest : Request -> Response
```
                            ^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `defaultResponse` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_import_type_annotations.md:6:24:6:44:**
```roc
processRequest = |req| Http.defaultResponse
```
                       ^^^^^^^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable `req` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_req` to suppress this warning.
The unused variable is declared here:
**can_import_type_annotations.md:6:19:6:22:**
```roc
processRequest = |req| Http.defaultResponse
```
                  ^^^


**UNDEFINED VARIABLE**
Nothing is named `parse` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_import_type_annotations.md:9:21:9:31:**
```roc
parseJson = |input| Json.parse(input)
```
                    ^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `decode` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_import_type_annotations.md:13:14:13:25:**
```roc
    result = Json.decode(request.body)
```
             ^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `success` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_import_type_annotations.md:15:24:15:36:**
```roc
        Ok(data) => Ok(Http.success(data))
```
                       ^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `defaultConfig` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_import_type_annotations.md:21:10:21:28:**
```roc
config = Json.defaultConfig
```
         ^^^^^^^^^^^^^^^^^^


**MODULE NOT IMPORTED**
There is no module with the name `Json.Parser` imported into this Roc file.

You're attempting to use this module here:
**can_import_type_annotations.md:24:18:24:36:**
```roc
advancedParser : Json.Parser.Config, Str -> Try(Json.Value, Json.Parser.Error)
```
                 ^^^^^^^^^^^^^^^^^^


**MODULE NOT IMPORTED**
There is no module with the name `Json.Parser` imported into this Roc file.

You're attempting to use this module here:
**can_import_type_annotations.md:24:61:24:78:**
```roc
advancedParser : Json.Parser.Config, Str -> Try(Json.Value, Json.Parser.Error)
```
                                                            ^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `parseWith` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_import_type_annotations.md:25:40:25:61:**
```roc
advancedParser = |parserConfig, input| Json.Parser.parseWith(parserConfig, input)
```
                                       ^^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwImport,LowerIdent,NoSpaceDotUpperIdent,KwAs,UpperIdent,KwExposing,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
KwImport,LowerIdent,NoSpaceDotUpperIdent,
KwImport,LowerIdent,NoSpaceDotUpperIdent,KwExposing,OpenSquare,UpperIdent,CloseSquare,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotUpperIdent,Comma,UpperIdent,NoSpaceDotUpperIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceDotLowerIdent,CloseRound,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotUpperIdent,Comma,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,
KwMatch,NoSpaceOpenRound,LowerIdent,CloseRound,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,UpperIdent,NoSpaceOpenRound,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,CloseRound,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-import (raw "http.Client") (alias "Http")
			(exposing
				(exposed-upper-ident (text "Request"))
				(exposed-upper-ident (text "Response"))))
		(s-import (raw "json.Json"))
		(s-import (raw "utils.Try")
			(exposing
				(exposed-upper-ident (text "Try"))))
		(s-type-anno (name "processRequest")
			(ty-fn
				(ty (name "Request"))
				(ty (name "Response"))))
		(s-decl
			(p-ident (raw "processRequest"))
			(e-lambda
				(args
					(p-ident (raw "req")))
				(e-ident (raw "Http.defaultResponse"))))
		(s-type-anno (name "parseJson")
			(ty-fn
				(ty (name "Str"))
				(ty (name "Json.Value"))))
		(s-decl
			(p-ident (raw "parseJson"))
			(e-lambda
				(args
					(p-ident (raw "input")))
				(e-apply
					(e-ident (raw "Json.parse"))
					(e-ident (raw "input")))))
		(s-type-anno (name "handleApi")
			(ty-fn
				(ty (name "Http.Request"))
				(ty-apply
					(ty (name "Try"))
					(ty (name "Http.Response"))
					(ty (name "Json.Error")))))
		(s-decl
			(p-ident (raw "handleApi"))
			(e-lambda
				(args
					(p-ident (raw "request")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "result"))
							(e-apply
								(e-ident (raw "Json.decode"))
								(e-field-access
									(e-ident (raw "request"))
									(e-ident (raw "body")))))
						(e-match
							(e-ident (raw "result"))
							(branches
								(branch
									(p-tag (raw "Ok")
										(p-ident (raw "data")))
									(e-apply
										(e-tag (raw "Ok"))
										(e-apply
											(e-ident (raw "Http.success"))
											(e-ident (raw "data")))))
								(branch
									(p-tag (raw "Err")
										(p-ident (raw "err")))
									(e-apply
										(e-tag (raw "Err"))
										(e-ident (raw "err"))))))))))
		(s-type-anno (name "config")
			(ty (name "Json.Config")))
		(s-decl
			(p-ident (raw "config"))
			(e-ident (raw "Json.defaultConfig")))
		(s-type-anno (name "advancedParser")
			(ty-fn
				(ty (name "Json.Parser.Config"))
				(ty (name "Str"))
				(ty-apply
					(ty (name "Try"))
					(ty (name "Json.Value"))
					(ty (name "Json.Parser.Error")))))
		(s-decl
			(p-ident (raw "advancedParser"))
			(e-lambda
				(args
					(p-ident (raw "parserConfig"))
					(p-ident (raw "input")))
				(e-apply
					(e-ident (raw "Json.Parser.parseWith"))
					(e-ident (raw "parserConfig"))
					(e-ident (raw "input")))))
		(s-type-anno (name "combineTrys")
			(ty-fn
				(ty-apply
					(ty (name "Try"))
					(ty-var (raw "a"))
					(ty-var (raw "err")))
				(ty-apply
					(ty (name "Try"))
					(ty-var (raw "b"))
					(ty-var (raw "err")))
				(ty-apply
					(ty (name "Try"))
					(ty-tuple
						(ty-var (raw "a"))
						(ty-var (raw "b")))
					(ty-var (raw "err")))))
		(s-decl
			(p-ident (raw "combineTrys"))
			(e-lambda
				(args
					(p-ident (raw "result1"))
					(p-ident (raw "result2")))
				(e-match
					(e-ident (raw "result1"))
					(branches
						(branch
							(p-tag (raw "Ok")
								(p-ident (raw "value1")))
							(e-match
								(e-tuple
									(e-ident (raw "result2")))
								(branches
									(branch
										(p-tag (raw "Ok")
											(p-ident (raw "value2")))
										(e-apply
											(e-tag (raw "Ok"))
											(e-tuple
												(e-ident (raw "value1"))
												(e-ident (raw "value2")))))
									(branch
										(p-tag (raw "Err")
											(p-ident (raw "err")))
										(e-apply
											(e-tag (raw "Err"))
											(e-ident (raw "err")))))))
						(branch
							(p-tag (raw "Err")
								(p-ident (raw "err")))
							(e-apply
								(e-tag (raw "Err"))
								(e-ident (raw "err"))))))))))
~~~
# FORMATTED
~~~roc
import http.Client as Http exposing [Request, Response]
import json.Json
import utils.Try exposing [Try]

processRequest : Request -> Response
processRequest = |req| Http.defaultResponse

parseJson : Str -> Json.Value
parseJson = |input| Json.parse(input)

handleApi : Http.Request -> Try(Http.Response, Json.Error)
handleApi = |request| {
	result = Json.decode(request.body)
	match result {
		Ok(data) => Ok(Http.success(data))
		Err(err) => Err(err)
	}
}

config : Json.Config
config = Json.defaultConfig

# Test nested type qualification
advancedParser : Json.Parser.Config, Str -> Try(Json.Value, Json.Parser.Error)
advancedParser = |parserConfig, input| Json.Parser.parseWith(parserConfig, input)

# Test function with multiple type parameters
combineTrys : Try(a, err), Try(b, err) -> Try((a, b), err)
combineTrys = |result1, result2|
	match result1 {
		Ok(value1) =>
			match (result2) {
				Ok(value2) => Ok((value1, value2))
				Err(err) => Err(err)
			}
		Err(err) => Err(err)
	}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "processRequest"))
		(e-lambda
			(args
				(p-assign (ident "req")))
			(e-runtime-error (tag "ident_not_in_scope")))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-malformed))))
	(d-let
		(p-assign (ident "parseJson"))
		(e-lambda
			(args
				(p-assign (ident "input")))
			(e-call
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-lookup-local
					(p-assign (ident "input")))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-lookup (name "Value") (external-module "json.Json")))))
	(d-let
		(p-assign (ident "handleApi"))
		(e-closure
			(captures
				(capture (ident "data"))
				(capture (ident "err")))
			(e-lambda
				(args
					(p-assign (ident "request")))
				(e-block
					(s-let
						(p-assign (ident "result"))
						(e-call
							(e-runtime-error (tag "ident_not_in_scope"))
							(e-dot-access (field "body")
								(receiver
									(e-lookup-local
										(p-assign (ident "request")))))))
					(e-match
						(match
							(cond
								(e-lookup-local
									(p-assign (ident "result"))))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag)))
									(value
										(e-tag (name "Ok")
											(args
												(e-call
													(e-runtime-error (tag "ident_not_in_scope"))
													(e-lookup-local
														(p-assign (ident "data"))))))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag)))
									(value
										(e-tag (name "Err")
											(args
												(e-lookup-local
													(p-assign (ident "err")))))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Request") (external-module "http.Client"))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "Response") (external-module "http.Client"))
					(ty-lookup (name "Error") (external-module "json.Json"))))))
	(d-let
		(p-assign (ident "config"))
		(e-runtime-error (tag "ident_not_in_scope"))
		(annotation
			(ty-lookup (name "Config") (external-module "json.Json"))))
	(d-let
		(p-assign (ident "advancedParser"))
		(e-lambda
			(args
				(p-assign (ident "parserConfig"))
				(p-assign (ident "input")))
			(e-call
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-lookup-local
					(p-assign (ident "parserConfig")))
				(e-lookup-local
					(p-assign (ident "input")))))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "Value") (external-module "json.Json"))
					(ty-malformed)))))
	(d-let
		(p-assign (ident "combineTrys"))
		(e-closure
			(captures
				(capture (ident "value1"))
				(capture (ident "value2"))
				(capture (ident "err"))
				(capture (ident "err")))
			(e-lambda
				(args
					(p-assign (ident "result1"))
					(p-assign (ident "result2")))
				(e-match
					(match
						(cond
							(e-lookup-local
								(p-assign (ident "result1"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-match
										(match
											(cond
												(e-lookup-local
													(p-assign (ident "result2"))))
											(branches
												(branch
													(patterns
														(pattern (degenerate false)
															(p-applied-tag)))
													(value
														(e-tag (name "Ok")
															(args
																(e-tuple
																	(elems
																		(e-lookup-local
																			(p-assign (ident "value1")))
																		(e-lookup-local
																			(p-assign (ident "value2")))))))))
												(branch
													(patterns
														(pattern (degenerate false)
															(p-applied-tag)))
													(value
														(e-tag (name "Err")
															(args
																(e-lookup-local
																	(p-assign (ident "err"))))))))))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-tag (name "Err")
										(args
											(e-lookup-local
												(p-assign (ident "err"))))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Try") (builtin)
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "err")))
				(ty-apply (name "Try") (builtin)
					(ty-rigid-var (name "b"))
					(ty-rigid-var-lookup (ty-rigid-var (name "err"))))
				(ty-apply (name "Try") (builtin)
					(ty-tuple
						(ty-rigid-var-lookup (ty-rigid-var (name "a")))
						(ty-rigid-var-lookup (ty-rigid-var (name "b"))))
					(ty-rigid-var-lookup (ty-rigid-var (name "err")))))))
	(s-import (module "http.Client")
		(exposes
			(exposed (name "Request") (wildcard false))
			(exposed (name "Response") (wildcard false))))
	(s-import (module "json.Json")
		(exposes))
	(s-import (module "utils.Try")
		(exposes
			(exposed (name "Try") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error -> Error"))
		(patt (type "Str -> Error"))
		(patt (type "Error -> Try(Error, Error)"))
		(patt (type "Error"))
		(patt (type "Error, Str -> Error"))
		(patt (type "Try(a, err), Try(b, err) -> Try((a, b), err)")))
	(expressions
		(expr (type "Error -> Error"))
		(expr (type "Str -> Error"))
		(expr (type "Error -> Try(Error, Error)"))
		(expr (type "Error"))
		(expr (type "Error, Str -> Error"))
		(expr (type "Try(a, err), Try(b, err) -> Try((a, b), err)"))))
~~~
