platform ""
    requires {
        make_glue : List(Types) -> Try(List(File), Str)
    }
    exposes [
        EntryPoint,
        File,
        FunctionInfo,
        FunctionRepr,
        HostedFunctionInfo,
        ModuleTypeInfo,
        ProvidesEntry,
        RocName,
        RecordField,
        RecordFieldInfo,
        RecordRepr,
        TagUnionRepr,
        TagVariant,
        TypeId,
        TypeTable,
        TypeRepr,
        Types,
    ]
    packages {}
    provides { "roc_make_glue": make_glue_for_host }
    targets: {}

import Types exposing [Types]
import File exposing [File]
import TypeId exposing [TypeId]
import EntryPoint exposing [EntryPoint]
import ModuleTypeInfo exposing [ModuleTypeInfo]
import FunctionInfo exposing [FunctionInfo]
import HostedFunctionInfo exposing [HostedFunctionInfo]
import RecordFieldInfo exposing [RecordFieldInfo]
import FunctionRepr exposing [FunctionRepr]
import RecordField exposing [RecordField]
import RecordRepr exposing [RecordRepr]
import TagUnionRepr exposing [TagUnionRepr]
import TagVariant exposing [TagVariant]
import TypeRepr exposing [TypeRepr]
import ProvidesEntry exposing [ProvidesEntry]
import TypeTable exposing [TypeTable]
import RocName exposing [RocName]

make_glue_for_host : List(Types) -> Try(List(File), Str)
make_glue_for_host = |types_list| make_glue(types_list)
