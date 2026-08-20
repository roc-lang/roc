app [Model, main] { pf: platform "./platform/main.roc" }

Model : { points : List(F32), cursor : U64 }

main = {
    init: |{}| { points: List.repeat(0.0.F32, 4096), cursor: 0 },
    update: |model| {
        points = match List.set(model.points, model.cursor % 4096, model.cursor.to_f32()) {
            Ok(updated) => updated
            Err(_) => model.points
        }
        { ..model, points, cursor: model.cursor + 1 }
    },
    cursor: |model| model.cursor,
}
