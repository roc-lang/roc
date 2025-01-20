module [find_path, Model, initial_model, cheapest_open, reconstruct_path]

import Quicksort

find_path = \cost_fn, move_fn, start, end ->
    astar(cost_fn, move_fn, end, initial_model(start))

Model position : {
    evaluated : Set position,
    open_set : Set position,
    costs : Dict position F64,
    came_from : Dict position position,
} where position implements Hash & Eq

initial_model : position -> Model position where position implements Hash & Eq
initial_model = \start -> {
    evaluated: Set.empty(),
    open_set: Set.single(start),
    costs: Dict.single(start, 0),
    came_from: Dict.empty(),
}

cheapest_open : (position -> F64), Model position -> Result position {} where position implements Hash & Eq
cheapest_open = \cost_fn, model ->
    model.open_set
    |> Set.to_list
    |> List.keep_oks(
        \position ->
            when Dict.get(model.costs, position) is
                Err(_) -> Err({})
                Ok(cost) -> Ok({ cost: cost + cost_fn(position), position }),
    )
    |> Quicksort.sort_by(.cost)
    |> List.first
    |> Result.map_ok(.position)
    |> Result.map_err(\_ -> {})

reconstruct_path : Dict position position, position -> List position where position implements Hash & Eq
reconstruct_path = \came_from, goal ->
    when Dict.get(came_from, goal) is
        Err(_) -> []
        Ok(next) -> List.append(reconstruct_path(came_from, next), goal)

update_cost : position, position, Model position -> Model position where position implements Hash & Eq
update_cost = \current, neighbor, model ->
    new_came_from =
        Dict.insert(model.came_from, neighbor, current)

    new_costs =
        Dict.insert(model.costs, neighbor, distance_to)

    distance_to =
        reconstruct_path(new_came_from, neighbor)
        |> List.len
        |> Num.to_frac

    new_model =
        { model &
            costs: new_costs,
            came_from: new_came_from,
        }

    when Dict.get(model.costs, neighbor) is
        Err(_) ->
            new_model

        Ok(previous_distance) ->
            if distance_to < previous_distance then
                new_model
            else
                model

astar : (position, position -> F64), (position -> Set position), position, Model position -> Result (List position) {} where position implements Hash & Eq
astar = \cost_fn, move_fn, goal, model ->
    when cheapest_open(\source -> cost_fn(source, goal), model) is
        Err({}) -> Err({})
        Ok(current) ->
            if current == goal then
                Ok(reconstruct_path(model.came_from, goal))
            else
                model_popped =
                    { model &
                        open_set: Set.remove(model.open_set, current),
                        evaluated: Set.insert(model.evaluated, current),
                    }

                neighbors =
                    move_fn(current)

                new_neighbors =
                    Set.difference(neighbors, model_popped.evaluated)

                model_with_neighbors : Model _
                model_with_neighbors =
                    model_popped
                    |> &open_set(Set.union(model_popped.open_set, new_neighbors))

                walker : Model _, _ -> Model _
                walker = \amodel, n -> update_cost(current, n, amodel)

                model_with_costs =
                    Set.walk(new_neighbors, model_with_neighbors, walker)

                astar(cost_fn, move_fn, goal, model_with_costs)

# takeStep = \moveFn, _goal, model, current ->
#     modelPopped =
#         { model &
#             openSet: Set.remove model.openSet current,
#             evaluated: Set.insert model.evaluated current,
#         }
#
#     neighbors = moveFn current
#
#     newNeighbors = Set.difference neighbors modelPopped.evaluated
#
#     modelWithNeighbors = { modelPopped & openSet: Set.union modelPopped.openSet newNeighbors }
#
#     # a lot goes wrong here
#     modelWithCosts =
#         Set.walk newNeighbors modelWithNeighbors (\n, m -> updateCost current n m)
#
#     modelWithCosts
