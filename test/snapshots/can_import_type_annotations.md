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
combine_tries : Try(a, err), Try(b, err) -> Try((a, b), err)
combine_tries = |result1, result2|
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
RESERVED TYPE NAME - can_import_type_annotations.md:2:12:2:17
RESERVED TYPE NAME - can_import_type_annotations.md:3:13:3:17
UNUSED VARIABLE - can_import_type_annotations.md:6:19:6:22
MOD NOT FOUND - can_import_type_annotations.md:5:18:5:25
MOD NOT FOUND - can_import_type_annotations.md:5:29:5:37
DOES NOT EXIST - can_import_type_annotations.md:6:24:6:44
MOD NOT FOUND - can_import_type_annotations.md:11:17:11:25
MOD NOT FOUND - can_import_type_annotations.md:11:37:11:46
DOES NOT EXIST - can_import_type_annotations.md:15:24:15:36
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Reserved Type Name")
		(region (start 2 12) (end 2 17))
		(headline
			(text "This import cannot bind ")
			(annotated symbol-unqualified "Json")
			(text " because that name is reserved for a builtin type."))
		(document
			(reflow "Use `as` to give the imported type a different name.")
			(line-break)
			(source-region (file "can_import_type_annotations.md") (start 2 12) (end 2 17) (annotation error) (line-text "import json.Json"))))
	(report
		(severity runtime_error)
		(title "Reserved Type Name")
		(region (start 3 13) (end 3 17))
		(headline
			(text "This import cannot bind ")
			(annotated symbol-unqualified "Try")
			(text " because that name is reserved for a builtin type."))
		(document
			(reflow "Use `as` to give the imported type a different name.")
			(line-break)
			(source-region (file "can_import_type_annotations.md") (start 3 13) (end 3 17) (annotation error) (line-text "import utils.Try exposing [Try]"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 6 19) (end 6 22))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "req")
			(reflow " is defined here and then never used."))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_req")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "can_import_type_annotations.md") (start 6 19) (end 6 22) (annotation error) (line-text "processRequest = |req| Http.defaultResponse"))))
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 5 18) (end 5 25))
		(headline
			(text "This ")
			(annotated code "Request")
			(reflow " type is declared to be in ")
			(annotated code "http.Client")
			(reflow ", which does not exist."))
		(document
			(source-region (file "can_import_type_annotations.md") (start 5 18) (end 5 25) (annotation error) (line-text "processRequest : Request -> Response"))))
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 5 29) (end 5 37))
		(headline
			(text "This ")
			(annotated code "Response")
			(reflow " type is declared to be in ")
			(annotated code "http.Client")
			(reflow ", which does not exist."))
		(document
			(source-region (file "can_import_type_annotations.md") (start 5 29) (end 5 37) (annotation error) (line-text "processRequest : Request -> Response"))))
	(report
		(severity runtime_error)
		(title "Does Not Exist")
		(region (start 6 24) (end 6 44))
		(headline
			(annotated symbol-unqualified "Http.defaultResponse")
			(reflow " does not exist."))
		(document
			(source-region (file "can_import_type_annotations.md") (start 6 24) (end 6 44) (annotation error) (line-text "processRequest = |req| Http.defaultResponse"))))
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 11 17) (end 11 25))
		(headline
			(text "This ")
			(annotated code "Request")
			(reflow " type is declared to be in ")
			(annotated code "http.Client")
			(reflow ", which does not exist."))
		(document
			(source-region (file "can_import_type_annotations.md") (start 11 17) (end 11 25) (annotation error) (line-text "handleApi : Http.Request -> Try(Http.Response, Json.Error)"))))
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 11 37) (end 11 46))
		(headline
			(text "This ")
			(annotated code "Response")
			(reflow " type is declared to be in ")
			(annotated code "http.Client")
			(reflow ", which does not exist."))
		(document
			(source-region (file "can_import_type_annotations.md") (start 11 37) (end 11 46) (annotation error) (line-text "handleApi : Http.Request -> Try(Http.Response, Json.Error)"))))
	(report
		(severity runtime_error)
		(title "Does Not Exist")
		(region (start 15 24) (end 15 36))
		(headline
			(annotated symbol-unqualified "Http.success")
			(reflow " does not exist."))
		(document
			(source-region (file "can_import_type_annotations.md") (start 15 24) (end 15 36) (annotation error) (line-text "        Ok(data) => Ok(Http.success(data))")))))
~~~
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
	(type-mod)
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
									(receiver
										(e-ident (raw "request")))
									(segment (mode "required") (field "body")))))
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
		(s-type-anno (name "combine_tries")
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
			(p-ident (raw "combine_tries"))
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
combine_tries : Try(a, err), Try(b, err) -> Try((a, b), err)
combine_tries = |result1, result2|
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
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-malformed))))
	(d-let
		(p-assign (ident "parseJson"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-malformed))))
	(d-let
		(p-assign (ident "handleApi"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-malformed))))
	(d-let
		(p-assign (ident "config"))
		(e-runtime-error (tag "builtin_import_conflict"))
		(annotation
			(ty-malformed)))
	(d-let
		(p-assign (ident "advancedParser"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-lookup (name "Str") (builtin))
				(ty-malformed))))
	(d-let
		(p-assign (ident "combine_tries"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-malformed)
				(ty-malformed))))
	(s-import (mod "http.Client")
		(exposes
			(exposed (name "Request") (wildcard false))
			(exposed (name "Response") (wildcard false))))
	(s-import (mod "json.Json")
		(exposes))
	(s-import (mod "utils.Try")
		(exposes
			(exposed (name "Try") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error -> Error"))
		(patt (type "Str -> Error"))
		(patt (type "Error -> Error"))
		(patt (type "Error"))
		(patt (type "Error, Str -> Error"))
		(patt (type "Error, Error -> Error")))
	(expressions
		(expr (type "Error -> Error"))
		(expr (type "Str -> Error"))
		(expr (type "Error -> Error"))
		(expr (type "Error"))
		(expr (type "Error, Str -> Error"))
		(expr (type "Error, Error -> Error"))))
~~~
