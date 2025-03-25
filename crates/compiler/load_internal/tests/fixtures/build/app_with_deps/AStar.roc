module [initial_model, reconstruct_path, update_cost, cheapest_open, astar, find_path]

# a port of https://github.com/krisajenkins/elm-astar/blob/2.1.3/src/AStar/Generalised.elm

Model position : {
    evaluated : Set position,
    open_set : Set position,
    costs : Map.Map position F64,
    came_from : Map.Map position position,
}

initial_model : position -> Model position
initial_model = \start -> {
    evaluated: Set.empty({}),
    open_set: Set.single(start),
    costs: Dict.single(start, 0.0),
    came_from: Map.empty,
}

cheapest_open : (position -> F64), Model position -> Result position [KeyNotFound]*
cheapest_open = \cost_function, model ->

    folder = \res_smallest_so_far, position ->
        when Map.get(model.costs, position) is
            Err(e) ->
                Err(e)

            Ok(cost) ->
                position_cost = cost_function(position)

                when res_smallest_so_far is
                    Err(_) -> Ok({ position, cost: cost + position_cost })
                    Ok(smallest_so_far) ->
                        if position_cost + cost < smallest_so_far.cost then
                            Ok({ position, cost: cost + position_cost })
                        else
                            Ok(smallest_so_far)

    Set.walk(model.open_set, Err(KeyNotFound), folder)
    |> Result.map_ok(\x -> x.position)

reconstruct_path : Map position position, position -> List position
reconstruct_path = \came_from, goal ->
    when Map.get(came_from, goal) is
        Err(KeyNotFound) ->
            []

        Ok(next) ->
            List.append(reconstruct_path(came_from, next), goal)

update_cost : position, position, Model position -> Model position
update_cost = \current, neighbour, model ->
    new_came_from = Map.insert(model.came_from, neighbour, current)

    new_costs = Map.insert(model.costs, neighbour, distance_to)

    distance_to =
        reconstruct_path(new_came_from, neighbour)
        |> List.len
        |> Num.to_frac

    new_model = { model & costs: new_costs, came_from: new_came_from }

    when Map.get(model.costs, neighbour) is
        Err(KeyNotFound) ->
            new_model

        Ok(previous_distance) ->
            if distance_to < previous_distance then
                new_model
            else
                model

find_path : { cost_function : position, position -> F64, move_function : position -> Set position, start : position, end : position } -> Result (List position) [KeyNotFound]*
find_path = \{ cost_function, move_function, start, end } ->
    astar(cost_function, move_function, end, initial_model(start))

astar : (position, position -> F64), (position -> Set position), position, Model position -> [Err [KeyNotFound]*, Ok (List position)]*
astar = \cost_fn, move_fn, goal, model ->
    when cheapest_open(\position -> cost_fn(goal, position), model) is
        Err(_) ->
            Err(KeyNotFound)

        Ok(current) ->
            if current == goal then
                Ok(reconstruct_path(model.came_from, goal))
            else
                model_popped = { model & open_set: Set.remove(model.open_set, current), evaluated: Set.insert(model.evaluated, current) }

                neighbours = move_fn(current)

                new_neighbours = Set.difference(neighbours, model_popped.evaluated)

                model_with_neighbours = { model_popped & open_set: Set.union(model_popped.open_set, new_neighbours) }

                model_with_costs = Set.walk(new_neighbours, model_with_neighbours, \md, nb -> update_cost(current, nb, md))

                astar(cost_fn, move_fn, goal, model_with_costs)

