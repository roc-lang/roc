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
        TargetLayout,
        TypeId,
        TypeLayout,
        TypeRepr,
        Types,
    ]
    packages {}
    provides { make_glue_for_host: "make_glue" }
    targets: {
        files: "targets/",
        exe: {
            x64mac: ["libhost.a", app],
            arm64mac: ["libhost.a", app],
            x64musl: ["crt1.o", "libhost.a", app, "libc.a"],
            arm64musl: ["crt1.o", "libhost.a", app, "libc.a"],
            x64win: ["host.lib", app],
            arm64win: ["host.lib", app],
        }
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
import TargetLayout exposing [TargetLayout]
import TypeLayout exposing [TypeLayout]
import TypeRepr exposing [TypeRepr]
import ProvidesEntry exposing [ProvidesEntry]

make_glue_for_host : List(Types) -> Try(List(File), Str)
make_glue_for_host = make_glue
