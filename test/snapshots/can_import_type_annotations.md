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
DUPLICATE DEFINITION - can_import_type_annotations.md:2:1:2:17
UNDECLARED TYPE - can_import_type_annotations.md:5:18:5:25
UNDECLARED TYPE - can_import_type_annotations.md:5:29:5:37
NAME NOT IN SCOPE - can_import_type_annotations.md:6:24:6:44
UNUSED VARIABLE - can_import_type_annotations.md:6:19:6:22
MOD NOT FOUND - can_import_type_annotations.md:8:24:8:30
NAME NOT IN SCOPE - can_import_type_annotations.md:9:21:9:31
MOD NOT FOUND - can_import_type_annotations.md:11:17:11:25
MOD NOT FOUND - can_import_type_annotations.md:11:37:11:46
MOD NOT FOUND - can_import_type_annotations.md:11:52:11:58
NAME NOT IN SCOPE - can_import_type_annotations.md:13:14:13:25
NAME NOT IN SCOPE - can_import_type_annotations.md:15:24:15:36
MOD NOT FOUND - can_import_type_annotations.md:20:14:20:21
NAME NOT IN SCOPE - can_import_type_annotations.md:21:10:21:28
MOD NOT FOUND - can_import_type_annotations.md:24:29:24:36
MOD NOT FOUND - can_import_type_annotations.md:24:53:24:59
MOD NOT FOUND - can_import_type_annotations.md:24:72:24:78
NAME NOT IN SCOPE - can_import_type_annotations.md:25:40:25:61
TYPE MISMATCH - can_import_type_annotations.md:15:21:15:43
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Duplicate Definition")
		(region (start 2 1) (end 2 17))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "Json")
			(reflow " is being redeclared here:"))
		(document
			(source-region (file "can_import_type_annotations.md") (start 2 1) (end 2 17) (annotation error) (line-text "import json.Json"))
			(line-break)
			(reflow "In this scope, ")
			(annotated symbol-unqualified "Json")
			(reflow " was already defined in ")
			(source-location
				(file "can_import_type_annotations.md")
				(line 1)
				(column 1))
			(reflow ":")
			(line-break)
			(source-region (file "can_import_type_annotations.md") (start 1 1) (end 1 1) (annotation dim) (line-text "import http.Client as Http exposing [Request, Response]"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 5 18) (end 5 25))
		(headline
			(reflow "The type ")
			(annotated code "Request")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "can_import_type_annotations.md") (start 5 18) (end 5 25) (annotation error) (line-text "processRequest : Request -> Response"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 5 29) (end 5 37))
		(headline
			(reflow "The type ")
			(annotated code "Response")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "can_import_type_annotations.md") (start 5 29) (end 5 37) (annotation error) (line-text "processRequest : Request -> Response"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 6 24) (end 6 44))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "defaultResponse")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "can_import_type_annotations.md") (start 6 24) (end 6 44) (annotation error) (line-text "processRequest = |req| Http.defaultResponse"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 6 19) (end 6 22))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "req")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_req")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "can_import_type_annotations.md") (start 6 19) (end 6 22) (annotation error) (line-text "processRequest = |req| Http.defaultResponse"))))
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 8 24) (end 8 30))
		(headline
			(text "This ")
			(annotated code "Value")
			(reflow " type is declared to be in ")
			(annotated code "json.Json")
			(reflow ", which does not exist."))
		(document
			(source-region (file "can_import_type_annotations.md") (start 8 24) (end 8 30) (annotation error) (line-text "parseJson : Str -> Json.Value"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 9 21) (end 9 31))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "parse")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "can_import_type_annotations.md") (start 9 21) (end 9 31) (annotation error) (line-text "parseJson = |input| Json.parse(input)"))))
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
		(title "Mod Not Found")
		(region (start 11 52) (end 11 58))
		(headline
			(text "This ")
			(annotated code "Error")
			(reflow " type is declared to be in ")
			(annotated code "json.Json")
			(reflow ", which does not exist."))
		(document
			(source-region (file "can_import_type_annotations.md") (start 11 52) (end 11 58) (annotation error) (line-text "handleApi : Http.Request -> Try(Http.Response, Json.Error)"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 13 14) (end 13 25))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "decode")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "can_import_type_annotations.md") (start 13 14) (end 13 25) (annotation error) (line-text "    result = Json.decode(request.body)"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 15 24) (end 15 36))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "success")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "can_import_type_annotations.md") (start 15 24) (end 15 36) (annotation error) (line-text "        Ok(data) => Ok(Http.success(data))"))))
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 20 14) (end 20 21))
		(headline
			(text "This ")
			(annotated code "Config")
			(reflow " type is declared to be in ")
			(annotated code "json.Json")
			(reflow ", which does not exist."))
		(document
			(source-region (file "can_import_type_annotations.md") (start 20 14) (end 20 21) (annotation error) (line-text "config : Json.Config"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 21 10) (end 21 28))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "defaultConfig")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "can_import_type_annotations.md") (start 21 10) (end 21 28) (annotation error) (line-text "config = Json.defaultConfig"))))
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 24 29) (end 24 36))
		(headline
			(text "This ")
			(annotated code "Parser.Config")
			(reflow " type is declared to be in ")
			(annotated code "json.Json")
			(reflow ", which does not exist."))
		(document
			(source-region (file "can_import_type_annotations.md") (start 24 29) (end 24 36) (annotation error) (line-text "advancedParser : Json.Parser.Config, Str -> Try(Json.Value, Json.Parser.Error)"))))
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 24 53) (end 24 59))
		(headline
			(text "This ")
			(annotated code "Value")
			(reflow " type is declared to be in ")
			(annotated code "json.Json")
			(reflow ", which does not exist."))
		(document
			(source-region (file "can_import_type_annotations.md") (start 24 53) (end 24 59) (annotation error) (line-text "advancedParser : Json.Parser.Config, Str -> Try(Json.Value, Json.Parser.Error)"))))
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 24 72) (end 24 78))
		(headline
			(text "This ")
			(annotated code "Parser.Error")
			(reflow " type is declared to be in ")
			(annotated code "json.Json")
			(reflow ", which does not exist."))
		(document
			(source-region (file "can_import_type_annotations.md") (start 24 72) (end 24 78) (annotation error) (line-text "advancedParser : Json.Parser.Config, Str -> Try(Json.Value, Json.Parser.Error)"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 25 40) (end 25 61))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "parseWith")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "can_import_type_annotations.md") (start 25 40) (end 25 61) (annotation error) (line-text "advancedParser = |parserConfig, input| Json.Parser.parseWith(parserConfig, input)"))))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 15 21) (end 15 43))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "branch of this")
			(reflow " ")
			(annotated code "match")
			(reflow " ")
			(reflow "does not match the previous")
			(reflow " ")
			(reflow "branch")
			(reflow " ")
			(reflow "."))
		(document
			(source-region (file "can_import_type_annotations.md") (start 15 21) (end 15 43) (annotation error) (line-text "        Ok(data) => Ok(Http.success(data))"))
			(line-break)
			(reflow "The")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "branch is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Ok(Error), ..]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But the previous")
			(reflow " ")
			(reflow "branch results")
			(reflow " ")
			(reflow "in:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Try(Error, Error)")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "All branches in a")
			(reflow " ")
			(annotated code "match")
			(reflow " ")
			(reflow "must have compatible types.")
			(line-break)
			(annotated underline "Note:")
			(reflow " ")
			(reflow "You can wrap branches values in a tag to make them compatible.")
			(line-break)
			(reflow "To learn about tags, see")
			(reflow " ")
			(link "https://www.roc-lang.org/tutorial#tags"))))
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
				(ty-apply (name "Try") (builtin)
					(ty-malformed)
					(ty-malformed)))))
	(d-let
		(p-assign (ident "config"))
		(e-runtime-error (tag "ident_not_in_scope"))
		(annotation
			(ty-malformed)))
	(d-let
		(p-assign (ident "advancedParser"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-malformed)
					(ty-malformed)))))
	(d-let
		(p-assign (ident "combineTrys"))
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
											(p-assign (ident "err")))))))))))
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
		(patt (type "Error -> Try(Error, Error)"))
		(patt (type "Error"))
		(patt (type "Error, Str -> Try(Error, Error)"))
		(patt (type "Try(a, err), Try(b, err) -> Try((a, b), err)")))
	(expressions
		(expr (type "Error -> Error"))
		(expr (type "Str -> Error"))
		(expr (type "Error -> Try(Error, Error)"))
		(expr (type "Error"))
		(expr (type "Error, Str -> Try(Error, Error)"))
		(expr (type "Try(a, err), Try(b, err) -> Try((a, b), err)"))))
~~~
