#include "roc_platform_abi.h"

RocU8x16 roc_c_lock_u8x16(RocU8x16 value) { return value; }
RocI8x16 roc_c_lock_i8x16(RocI8x16 value) { return value; }
RocU16x8 roc_c_lock_u16x8(RocU16x8 value) { return value; }
RocI16x8 roc_c_lock_i16x8(RocI16x8 value) { return value; }
RocU32x4 roc_c_lock_u32x4(RocU32x4 value) { return value; }
RocI32x4 roc_c_lock_i32x4(RocI32x4 value) { return value; }
RocU64x2 roc_c_lock_u64x2(RocU64x2 value) { return value; }
RocI64x2 roc_c_lock_i64x2(RocI64x2 value) { return value; }

ProbeVectorWrapper roc_c_lock_wrapper(ProbeVectorWrapper value) { return value; }
ProbeVectorRecord roc_c_lock_record(ProbeVectorRecord value) { return value; }
ProbeVectorQuad roc_c_lock_quad(ProbeVectorQuad value) { return value; }
ProbeVectorHva roc_c_lock_hva(ProbeVectorHva value) { return value; }
ProbeVectorTag roc_c_lock_tag(ProbeVectorTag value) { return value; }
AnonStructFbe9eaebfd8c38fd roc_c_lock_tuple(ProbeRoundtripVectorTupleArgs value) {
    AnonStructFbe9eaebfd8c38fd result = {._1 = value._1, ._2 = value._2, ._0 = value._0};
    return result;
}
