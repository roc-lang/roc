# Types for glue generation
# Contains type information extracted from the platform module

import TypeId exposing [TypeId]
import EntryPoint exposing [EntryPoint]
import ModuleTypeInfo exposing [ModuleTypeInfo]
import FunctionInfo exposing [FunctionInfo]
import HostedFunctionInfo exposing [HostedFunctionInfo]

## Types structure for glue generation
Types := {
    ## Entry points (platform requires)
    entrypoints : List(EntryPoint),
    ## Module type information
    modules : List(ModuleTypeInfo),
}.{
}
