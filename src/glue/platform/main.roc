platform ""
    requires {
        make_glue : List(Types) -> Try(List(File), Str)
    }
    exposes [
        EntryPoint,
        AbiFieldLayout,
        AbiLayout,
        AbiLayoutDetails,
        AbiRecordLayout,
        AbiTagLayout,
        AbiTagUnionLayout,
        ArgShape,
        File,
        FunctionInfo,
        FunctionRepr,
        GlueInput,
        HostRcPlan,
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
        TypeInfo,
        TypeTable,
        TypeNamePlan,
        TypeRepr,
        Types,
    ]
    packages {}
    provides { "roc_make_glue": make_glue_for_host }
    targets: {}

import Types exposing [Types]
import File exposing [File]
import TypeId exposing [TypeId]
import AbiFieldLayout exposing [AbiFieldLayout]
import AbiLayout exposing [AbiLayout]
import AbiLayoutDetails exposing [AbiLayoutDetails]
import AbiRecordLayout exposing [AbiRecordLayout]
import AbiTagLayout exposing [AbiTagLayout]
import AbiTagUnionLayout exposing [AbiTagUnionLayout]
import ArgShape exposing [ArgShape]
import EntryPoint exposing [EntryPoint]
import ModuleTypeInfo exposing [ModuleTypeInfo]
import FunctionInfo exposing [FunctionInfo]
import HostedFunctionInfo exposing [HostedFunctionInfo]
import GlueInput exposing [GlueInput]
import HostRcPlan exposing [HostRcPlan]
import RecordFieldInfo exposing [RecordFieldInfo]
import FunctionRepr exposing [FunctionRepr]
import RecordField exposing [RecordField]
import RecordRepr exposing [RecordRepr]
import TagUnionRepr exposing [TagUnionRepr]
import TagVariant exposing [TagVariant]
import TypeRepr exposing [TypeRepr]
import ProvidesEntry exposing [ProvidesEntry]
import TypeInfo exposing [TypeInfo]
import TypeTable exposing [TypeTable]
import TypeNamePlan exposing [TypeNamePlan]
import RocName exposing [RocName]

make_glue_for_host : List(Types) -> Try(List(File), Str)
make_glue_for_host = |types_list| make_glue(types_list)
