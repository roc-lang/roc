import TypeId exposing [TypeId]
import EntryPoint exposing [EntryPoint]
import ModuleTypeInfo exposing [ModuleTypeInfo]
import FunctionInfo exposing [FunctionInfo]
import HostedFunctionInfo exposing [HostedFunctionInfo]

## Type information extracted from the platform module for glue generation
Types := {
    ## Entry points (aka platform requires)
    entrypoints : List(EntryPoint),
    modules : List(ModuleTypeInfo),
}
