interface AStar
    exposes [findPath, Model, initialModel, cheapestOpen, reconstructPath]
    imports [Quicksort]

findPath = \costFn, moveFn, start, end ->
    astar costFn moveFn end (initialModel start)

Model position : {
    evaluated : Set position,
    openSet : Set position,
    costs : Dict position F64,
    cameFrom : Dict position position,
} | position has Hash & Eq

initialModel : position -> Model position | position has Hash & Eq
initialModel = \start -> {
    evaluated: Set.empty,
    openSet: Set.single start,
    costs: Dict.single start 0,
    cameFrom: Dict.empty,
}

cheapestOpen : (position -> F64), Model position -> Result position {} | position has Hash & Eq
cheapestOpen = \costFn, model ->
    model.openSet
    |> Set.toList
    |> List.keepOks
        (\position ->
            when Dict.get model.costs position is
                Err _ -> Err {}
                Ok cost -> Ok { cost: cost + costFn position, position }
        )
    |> Quicksort.sortBy .cost
    |> List.first
    |> Result.map .position
    |> Result.mapErr (\_ -> {})

reconstructPath : Dict position position, position -> List position | position has Hash & Eq
reconstructPath = \cameFrom, goal ->
    when Dict.get cameFrom goal is
        Err _ -> []
        Ok next -> List.append (reconstructPath cameFrom next) goal

updateCost : position, position, Model position -> Model position | position has Hash & Eq
updateCost = \current, neighbor, model ->
    newCameFrom =
        Dict.insert model.cameFrom neighbor current

    newCosts =
        Dict.insert model.costs neighbor distanceTo

    distanceTo =
        reconstructPath newCameFrom neighbor
        |> List.len
        |> Num.toFrac

    newModel =
        { model &
            costs: newCosts,
            cameFrom: newCameFrom,
        }

    when Dict.get model.costs neighbor is
        Err _ ->
            newModel

        Ok previousDistance ->
            if distanceTo < previousDistance then
                newModel
            else
                model

astar : (position, position -> F64), (position -> Set position), position, Model position -> Result (List position) {} | position has Hash & Eq
astar = \costFn, moveFn, goal, model ->
    when cheapestOpen (\source -> costFn source goal) model is
        Err {} -> Err {}
        Ok current ->
            if current == goal then
                Ok (reconstructPath model.cameFrom goal)
            else
                modelPopped =
                    { model &
                        openSet: Set.remove model.openSet current,
                        evaluated: Set.insert model.evaluated current,
                    }

                neighbors =
                    moveFn current

                newNeighbors =
                    Set.difference neighbors modelPopped.evaluated

                modelWithNeighbors : Model position
                modelWithNeighbors =
                    { modelPopped &
                        openSet: Set.union modelPopped.openSet newNeighbors,
                    }

                walker : Model position, position -> Model position
                walker = \amodel, n -> updateCost current n amodel

                modelWithCosts =
                    Set.walk newNeighbors modelWithNeighbors walker

                astar costFn moveFn goal modelWithCosts

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
