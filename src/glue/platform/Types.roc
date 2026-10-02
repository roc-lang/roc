import TypeId
import ModuleTypeInfo
import FunctionInfo
import HostedFunctionInfo
import TypeInfo
import ProvidesEntry

## Type information extracted from the platform module for glue generation
Types := {
    modules : List(ModuleTypeInfo),
    provides_entries : List(ProvidesEntry),
    types : List(TypeInfo),
}
