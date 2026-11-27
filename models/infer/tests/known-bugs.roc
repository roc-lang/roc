# Programs on which the compiler is known to disagree with the model. Each
# section names the open issue. When an issue is fixed, selftest fails on its
# section: change it to `# expect: agree`.
H := [H].{}

# === bounded output row merges with an input rest row
# expect: disagree https://github.com/roc-lang/roc/issues/13297
forward : [B, ..] -> [B]
forward = |x| x

# === rigid variable escapes into a top-level weak value
# The model rejects `f`; the compiler accepts it and binds `k` to `b`, so a
# later `k(1.U64)` elsewhere would be rejected instead.
# expect: disagree https://github.com/roc-lang/roc/issues/13298
k = if Bool.True (|x| x) else (|x| x)

f : {} -> (b -> b)
f = |_| k

# === printer gives two type variables one name
# expect: disagree https://github.com/roc-lang/roc/issues/13300
both = |x, y| {
    v = x?
    _w = y?
    Ok(v)
}
