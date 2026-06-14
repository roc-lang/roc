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
        RecordField,
        RecordFieldInfo,
        RecordRepr,
        TagUnionRepr,
        TagVariant,
        TypeId,
        TypeRepr,
        Types,
    ]
    packages {}
    provides { "roc_make_glue": make_glue_for_host }
    targets: {
        inputs: "targets/",
        x64mac: { inputs: ["libhost.a", app] },
        arm64mac: { inputs: ["libhost.a", app] },
        x64musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
        arm64musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
        x64win: { inputs: ["host.lib", app] },
        arm64win: { inputs: ["host.lib", app] },
    }

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

make_glue_for_host : List(Types) -> Try(List(File), Str)
make_glue_for_host = |types_list| make_glue(types_list)
