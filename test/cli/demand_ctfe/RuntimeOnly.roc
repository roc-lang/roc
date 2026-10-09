app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import pf.Stdout

# A reachable body with no typed-literal, dispatch, or empirical obligation.
# The runtime argument prevents the helper from becoming a CTFE root.
runtime_only : U64 -> U64
runtime_only = |input| {
    a = input + input
    b = a + input
    c = b + input
    d = c + input
    e = d + input
    f = e + input
    g = f + input
    h = g + input
    i = h + input
    j = i + input
    k = j + input
    l = k + input
    m = l + input
    n = m + input
    o = n + input
    p = o + input
    q = p + input
    r = q + input
    s = r + input
    t = s + input
    u = t + input
    v = u + input
    w = v + input
    x = w + input
    y = x + input
    z = y + input
    z
}

main! = |args| {
    input = args.len().to_u64()
    result = runtime_only(input)
    if result == input * 27 {
        Stdout.line!("runtime only correct")
    } else {
        Stdout.line!("runtime only incorrect")
    }
    Ok({})
}
