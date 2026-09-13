//! Issue 11196: keyword tag declarations and accessor bodies must compile.
const abi = @import("abi");

comptime {
    if (@intFromEnum(abi.TooLargeOrUnreachable.too_large) != 0 or
        @intFromEnum(abi.TooLargeOrUnreachable.@"unreachable") != 1)
        @compileError("pure enum discriminants changed");

    if (!@hasField(abi.DeferOrOtherPayload, "defer") or
        !@hasField(abi.DeferOrOtherPayload, "other") or
        !@hasField(abi.DeferOrExportOrStructPayload, "defer") or
        !@hasField(abi.DeferOrExportOrStructPayload, "export") or
        !@hasField(abi.DeferOrExportOrStructPayload, "struct"))
        @compileError("payload field names changed");
}

/// Force analysis of a keyword field access with a single payload.
export fn keywordSinglePayload(value: *const abi.DeferOrExportOrStruct) u64 {
    return value.payload_export();
}

/// Force analysis of a keyword field access with multiple payloads.
export fn keywordTuplePayload(value: *const abi.DeferOrExportOrStruct) u64 {
    const payload = value.payload_struct();
    return payload._0 + payload._1;
}

/// Ordinary field and accessor names retain their identity.
export fn ordinaryPayload(value: *const abi.DeferOrOther) abi.RocStr {
    return value.payload_other();
}
