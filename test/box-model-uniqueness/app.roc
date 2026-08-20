app [Model, main] { pf: platform "./platform/main.roc" }

Model : { points : List(F32), cursor : U64 }

main = {
    init: |{}| { points: List.repeat(0.0.F32, 4096), cursor: 0 },
    init_append: |{}| {
        points: List.reserve(List.repeat(0.0.F32, 4096), 16),
        cursor: 0,
    },
    update: |model| {
        points = match List.set(model.points, model.cursor % 4096, model.cursor.to_f32()) {
            Ok(updated) => updated
            Err(_) => model.points
        }
        { ..model, points, cursor: model.cursor + 1 }
    },
    update_append: |model| {
        # Both checked mutations fail and return ownership of their input. The
        # following append must see that twice-restored unique list and consume
        # its reserved capacity without copying. Using replace and update also
        # proves the optimization follows the generic outcome convention, not
        # a List.set-specific rule.
        after_replace = match List.replace(model.points, 1000000, model.cursor.to_f32()) {
            Ok(updated) => updated.list
            Err(_) => model.points
        }
        points = match List.update(after_replace, 1000000, |value| value + 1) {
            Ok(updated) => updated
            Err(_) => after_replace
        }
        { ..model, points: List.append(points, model.cursor.to_f32()), cursor: model.cursor + 1 }
    },
    cursor: |model| model.cursor,
}
