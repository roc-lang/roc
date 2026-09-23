import TypeId exposing [TypeId]
import ModuleTypeInfo exposing [ModuleTypeInfo]
import FunctionInfo exposing [FunctionInfo]
import HostedFunctionInfo exposing [HostedFunctionInfo]
import TypeInfo exposing [TypeInfo]
import ProvidesEntry exposing [ProvidesEntry]

## Type information extracted from the platform module for glue generation
Types := {
    modules : List(ModuleTypeInfo),
    provides_entries : List(ProvidesEntry),
    types : List(TypeInfo),
}
