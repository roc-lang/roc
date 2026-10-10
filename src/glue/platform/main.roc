platform ""
	requires {
		make_glue : List(Types) -> Try(List(File), Str)
	}
	exposes [
		AbiFieldLayout,
		AbiLayout,
		AbiLayoutDetails,
		AbiRecordLayout,
		AbiTagLayout,
		AbiTagUnionLayout,
		AbiWidth,
		ArgShape,
		CallableSignature,
		File,
		FunctionInfo,
		FunctionSignature,
		GlueInput,
		HostRcPlan,
		HostedFunctionInfo,
		ModuleTypeInfo,
		ProvidedExport,
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

import Types
import File
import TypeId
import AbiFieldLayout
import AbiLayout
import AbiLayoutDetails
import AbiRecordLayout
import AbiTagLayout
import AbiTagUnionLayout
import AbiWidth
import ArgShape
import CallableSignature
import ModuleTypeInfo
import FunctionInfo
import HostedFunctionInfo
import GlueInput
import HostRcPlan
import RecordFieldInfo
import FunctionSignature
import ProvidedExport
import RecordField
import RecordRepr
import TagUnionRepr
import TagVariant
import TypeRepr
import ProvidesEntry
import TypeInfo
import TypeTable
import TypeNamePlan
import RocName

make_glue_for_host : List(Types) -> Try(List(File), Str)
make_glue_for_host = |types_list| make_glue(types_list)
