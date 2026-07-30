platform ""
	requires {
		main! : () => {},
		provide_u8x16 : U8x16 -> U8x16,
		provide_i8x16 : I8x16 -> I8x16,
		provide_u16x8 : U16x8 -> U16x8,
		provide_i16x8 : I16x8 -> I16x8,
		provide_u32x4 : U32x4 -> U32x4,
		provide_i32x4 : I32x4 -> I32x4,
		provide_u64x2 : U64x2 -> U64x2,
		provide_i64x2 : I64x2 -> I64x2,
		provide_vector_record : Probe.VectorRecord -> Probe.VectorRecord,
		provide_vector_quad : Probe.VectorQuad -> Probe.VectorQuad,
		provide_vector_hva : Probe.VectorHva -> Probe.VectorHva,
		provide_vector_wrapper : Probe.VectorWrapper -> Probe.VectorWrapper,
		provide_vector_tuple : (U64, U8x16, I16x8) -> (U64, U8x16, I16x8),
		provide_vector_tag : Probe.VectorTag -> Probe.VectorTag,
		make_vector_tag : {} -> Probe.VectorTag,
		provide_exhaust_registers : I64, I64, I64, I64, I64, I64, F64, F64, F64, F64, F64, F64, F64, F64, U8x16 -> U8x16,
		provide_spill_vector_hva : U8x16, U8x16, U8x16, U8x16, U8x16, U8x16, U8x16, Probe.NestedVectorHva -> Probe.NestedVectorHva,
		provide_spill_float_hfa : F64, F64, F64, F64, F64, F64, F64, Probe.NestedFloatHfa -> Probe.NestedFloatHfa,
		provide_spill_integer_pair : I64, I64, I64, I64, I64, I64, I64, Probe.IntegerPair -> Probe.IntegerPair,
		provide_align_i128 : I64, I128 -> I128,
		provide_spill_i128 : I64, I64, I64, I64, I64, I128 -> I128,
		provide_spill_dec : I64, I64, I64, I64, I64, Dec -> Dec,
		provide_compact_stack : I64, I64, I64, I64, I64, I64, I64, I64, U8, U16, U32 -> U64
	}
	exposes [Probe]
	packages {}
	provides {
		"roc_main": main_for_host!,
		"roc_provide_u8x16": provide_u8x16_for_host,
		"roc_provide_i8x16": provide_i8x16_for_host,
		"roc_provide_u16x8": provide_u16x8_for_host,
		"roc_provide_i16x8": provide_i16x8_for_host,
		"roc_provide_u32x4": provide_u32x4_for_host,
		"roc_provide_i32x4": provide_i32x4_for_host,
		"roc_provide_u64x2": provide_u64x2_for_host,
		"roc_provide_i64x2": provide_i64x2_for_host,
		"roc_provide_vector_record": provide_vector_record_for_host,
		"roc_provide_vector_quad": provide_vector_quad_for_host,
		"roc_provide_vector_hva": provide_vector_hva_for_host,
		"roc_provide_vector_wrapper": provide_vector_wrapper_for_host,
		"roc_provide_vector_tuple": provide_vector_tuple_for_host,
		"roc_provide_vector_tag": provide_vector_tag_for_host,
		"roc_make_vector_tag": make_vector_tag_for_host,
		"roc_provide_exhaust_registers": provide_exhaust_registers_for_host,
		"roc_provide_spill_vector_hva": provide_spill_vector_hva_for_host,
		"roc_provide_spill_float_hfa": provide_spill_float_hfa_for_host,
		"roc_provide_spill_integer_pair": provide_spill_integer_pair_for_host,
		"roc_provide_align_i128": provide_align_i128_for_host,
		"roc_provide_spill_i128": provide_spill_i128_for_host,
		"roc_provide_spill_dec": provide_spill_dec_for_host,
		"roc_provide_compact_stack": provide_compact_stack_for_host,
	}
	hosted {
		"roc_probe_roundtrip": Probe.roundtrip!,
		"roc_probe_roundtrip_u8x16": Probe.roundtrip_u8x16!,
		"roc_probe_roundtrip_i8x16": Probe.roundtrip_i8x16!,
		"roc_probe_roundtrip_u16x8": Probe.roundtrip_u16x8!,
		"roc_probe_roundtrip_i16x8": Probe.roundtrip_i16x8!,
		"roc_probe_roundtrip_u32x4": Probe.roundtrip_u32x4!,
		"roc_probe_roundtrip_i32x4": Probe.roundtrip_i32x4!,
		"roc_probe_roundtrip_u64x2": Probe.roundtrip_u64x2!,
		"roc_probe_roundtrip_i64x2": Probe.roundtrip_i64x2!,
		"roc_probe_roundtrip_vector_record": Probe.roundtrip_vector_record!,
		"roc_probe_roundtrip_vector_quad": Probe.roundtrip_vector_quad!,
		"roc_probe_roundtrip_vector_hva": Probe.roundtrip_vector_hva!,
		"roc_probe_roundtrip_vector_wrapper": Probe.roundtrip_vector_wrapper!,
		"roc_probe_roundtrip_vector_tuple": Probe.roundtrip_vector_tuple!,
		"roc_probe_roundtrip_vector_tag": Probe.roundtrip_vector_tag!,
		"roc_probe_exhaust_registers": Probe.exhaust_registers!,
		"roc_probe_spill_vector_hva": Probe.spill_vector_hva!,
		"roc_probe_spill_float_hfa": Probe.spill_float_hfa!,
		"roc_probe_spill_integer_pair": Probe.spill_integer_pair!,
		"roc_probe_align_i128": Probe.align_i128!,
		"roc_probe_spill_i128": Probe.spill_i128!,
		"roc_probe_spill_dec": Probe.spill_dec!,
		"roc_probe_compact_stack": Probe.compact_stack!,
	}
	targets: {
		inputs_dir: "targets/",
		x64musl: {
			inputs: ["crt1.o", "libhost.a", app, "libc.a"],
			output: Exe,
		},
		arm64musl: {
			inputs: ["crt1.o", "libhost.a", app, "libc.a"],
			output: Exe,
		},
		x64mac: {
			inputs: ["libhost.a", app],
			output: Exe,
		},
		arm64mac: {
			inputs: ["libhost.a", app],
			output: Exe,
		},
		x64win: {
			inputs: ["host.lib", app],
			output: Exe,
		},
		arm64win: {
			inputs: ["host.lib", app],
			output: Exe,
		},
		wasm32: {
			inputs: ["host.wasm", app],
			output: Shared,
		},
	}

import Probe

main_for_host! : () => {}
main_for_host! = main!

provide_u8x16_for_host = provide_u8x16

provide_i8x16_for_host = provide_i8x16

provide_u16x8_for_host = provide_u16x8

provide_i16x8_for_host = provide_i16x8

provide_u32x4_for_host = provide_u32x4

provide_i32x4_for_host = provide_i32x4

provide_u64x2_for_host = provide_u64x2

provide_i64x2_for_host = provide_i64x2

provide_vector_record_for_host = provide_vector_record

provide_vector_quad_for_host = provide_vector_quad

provide_vector_hva_for_host = provide_vector_hva

provide_vector_wrapper_for_host = provide_vector_wrapper

provide_vector_tuple_for_host = provide_vector_tuple

provide_vector_tag_for_host = provide_vector_tag

make_vector_tag_for_host = make_vector_tag

provide_exhaust_registers_for_host = provide_exhaust_registers

provide_spill_vector_hva_for_host = provide_spill_vector_hva

provide_spill_float_hfa_for_host = provide_spill_float_hfa

provide_spill_integer_pair_for_host = provide_spill_integer_pair

provide_align_i128_for_host = provide_align_i128

provide_spill_i128_for_host = provide_spill_i128

provide_spill_dec_for_host = provide_spill_dec

provide_compact_stack_for_host = provide_compact_stack
