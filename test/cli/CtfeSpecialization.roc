CtfeSpecialization :: [].{
    push = |{ tag, items }, x| { tag, items: items.append(x) }

    build : U64 -> U64
    build = |n| {
        var $r = { tag: "plans", items: [] }
        for _ in List.from_iter(U64.until(0, n)) {
            $r = CtfeSpecialization.push($r, "Hello")
        }
        $r.items.len()
    }

    built : U64
    built = CtfeSpecialization.build(32)

    # Fail during check, not just roc test, if evaluation stores the wrong value.
    verified : U64
    verified = if CtfeSpecialization.built == 32 32 else crash "wrong compile-time length"
}
