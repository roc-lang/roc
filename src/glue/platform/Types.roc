import TypeId exposing [TypeId]
import EntryPoint exposing [EntryPoint]
import ModuleTypeInfo exposing [ModuleTypeInfo]
import FunctionInfo exposing [FunctionInfo]
import HostedFunctionInfo exposing [HostedFunctionInfo]
import TypeRepr exposing [TypeRepr]
import ProvidesEntry exposing [ProvidesEntry]
import TargetLayout exposing [TargetLayout]

## Type information extracted from the platform module for glue generation
Types := {
    entrypoints : List(EntryPoint),
    layouts : List(TargetLayout),
    modules : List(ModuleTypeInfo),
    provides_entries : List(ProvidesEntry),
    target_names : List(Str),
    type_table : List(TypeRepr),
}
