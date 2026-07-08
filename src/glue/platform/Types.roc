import TypeId exposing [TypeId]
import EntryPoint exposing [EntryPoint]
import ModuleTypeInfo exposing [ModuleTypeInfo]
import FunctionInfo exposing [FunctionInfo]
import HostedFunctionInfo exposing [HostedFunctionInfo]
import TypeInfo exposing [TypeInfo]
import ProvidesEntry exposing [ProvidesEntry]

## Type information extracted from the platform module for glue generation
Types := {
    ## Entry points (aka platform requires)
    entrypoints : List(EntryPoint),
    modules : List(ModuleTypeInfo),
    provides_entries : List(ProvidesEntry),
    types : List(TypeInfo),
}
