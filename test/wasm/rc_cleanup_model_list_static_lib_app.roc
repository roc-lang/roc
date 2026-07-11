app [main!] { pf: platform "./static-lib-platform/main.roc" }

Plant : {
    x : I32,
    kind : U32,
}

Model : {
    tick : U64,
    plants : List(Plant),
}

step : Box(Model) -> Box(Model)
step = |boxed| {
    model = Box.unbox(boxed)
    moved = List.map(model.plants, |plant| { ..plant, x: plant.x - 1 })
    kept = List.drop_if(moved, |plant| plant.x < -2)

    plants =
        if model.tick % 3 == 0 {
            List.append(kept, { x: 3, kind: U64.to_u32_wrap(model.tick) })
        } else {
            kept
        }

    Box.box({ tick: model.tick + 1, plants })
}

run : Box(Model), U64 -> Box(Model)
run = |initial, count| {
    var $boxed = initial
    var $i = 0

    while $i < count {
        $boxed = step($boxed)
        $i = $i + 1
    }

    $boxed
}

main! = |seed| {
    count = seed + 16
    final_box = run(Box.box({ tick: 0, plants: [] }), count)
    final = Box.unbox(final_box)

    if final.tick == count and List.len(final.plants) < 4 {
        "ok"
    } else {
        "bad"
    }
}
