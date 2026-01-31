import FunctionInfo exposing [FunctionInfo]
import HostedFunctionInfo exposing [HostedFunctionInfo]

## Module type information
ModuleTypeInfo := {
    functions : List(FunctionInfo),
    hosted_functions : List(HostedFunctionInfo),
    main_type : Str,
    name : Str,
}.{}
