## Universal value type for crossing the Roc/Host boundary.
## All signal and event values are encoded to this format.
NodeValue := [
    NvI64(I64),
    NvStr(Str),
    NvBool(Bool),
    NvUnit,
    NvF64(F64),
    NvList(List(NodeValue)),
].{
    ## Format instance - used as the format argument for encode/decode
    format : NodeValue
    format = NvUnit

    ## Encode methods for where clause constraints
    ## Pattern: encode_TYPE : NodeValue, TYPE -> Try(NodeValue, [])
    encode_i64 : NodeValue, I64 -> Try(NodeValue, [])
    encode_i64 = |_fmt, n| Ok(NvI64(n))

    encode_str : NodeValue, Str -> Try(NodeValue, [])
    encode_str = |_fmt, s| Ok(NvStr(s))

    encode_bool : NodeValue, Bool -> Try(NodeValue, [])
    encode_bool = |_fmt, b| Ok(NvBool(b))

    encode_unit : NodeValue, {} -> Try(NodeValue, [])
    encode_unit = |_fmt, {}| Ok(NvUnit)

    ## Decode methods for where clause constraints
    ## Pattern: decode_TYPE : NodeValue, NodeValue -> (Try(TYPE, [TypeMismatch]), NodeValue)
    decode_i64 : NodeValue, NodeValue -> (Try(I64, [TypeMismatch]), NodeValue)
    decode_i64 = |_fmt, nv| {
        match nv {
            NvI64(n) => (Ok(n), nv)
            _ => (Err(TypeMismatch), nv)
        }
    }

    decode_str : NodeValue, NodeValue -> (Try(Str, [TypeMismatch]), NodeValue)
    decode_str = |_fmt, nv| {
        match nv {
            NvStr(s) => (Ok(s), nv)
            _ => (Err(TypeMismatch), nv)
        }
    }

    decode_bool : NodeValue, NodeValue -> (Try(Bool, [TypeMismatch]), NodeValue)
    decode_bool = |_fmt, nv| {
        match nv {
            NvBool(b) => (Ok(b), nv)
            _ => (Err(TypeMismatch), nv)
        }
    }

    decode_unit : NodeValue, NodeValue -> (Try({}, [TypeMismatch]), NodeValue)
    decode_unit = |_fmt, nv| {
        match nv {
            NvUnit => (Ok({}), nv)
            _ => (Err(TypeMismatch), nv)
        }
    }

    ## Helper: Create NodeValue from I64
    from_i64 : I64 -> NodeValue
    from_i64 = |n| NvI64(n)

    ## Helper: Create NodeValue from Str
    from_str : Str -> NodeValue
    from_str = |s| NvStr(s)

    ## Helper: Create NodeValue from Bool
    from_bool : Bool -> NodeValue
    from_bool = |b| NvBool(b)

    ## Helper: Create unit NodeValue
    unit : NodeValue
    unit = NvUnit

    ## Helper: Extract I64 from NodeValue (crashes on type mismatch)
    to_i64 : NodeValue -> I64
    to_i64 = |nv| {
        match nv {
            NvI64(n) => n
            _ => ...
        }
    }

    ## Helper: Extract Str from NodeValue (crashes on type mismatch)
    to_str : NodeValue -> Str
    to_str = |nv| {
        match nv {
            NvStr(s) => s
            _ => ...
        }
    }
}
