import FunctionInfo exposing [FunctionInfo]
import HostedFunctionInfo exposing [HostedFunctionInfo]

ModuleTypeInfo := {
    functions : List(FunctionInfo),
    hosted_functions : List(HostedFunctionInfo),
    main_type : Str,
    name : Str,
}
