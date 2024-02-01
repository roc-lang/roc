interface AStar
    exposes [initialModel, reconstructPath, updateCost, cheapestOpen, astar, findPath]

# a port of https://github.com/krisajenkins/elm-astar/blob/2.1.3/src/AStar/Generalised.elm

Model position : {
    evaluated : Set position,
    openSet : Set position,
    costs : Map.Map position F64,
    cameFrom : Map.Map position position,
}

initialModel : position -> Model position
initialModel = \start -> {
    evaluated: Set.empty {},
    openSet: Set.single start,
    costs: Dict.single start 0.0,
    cameFrom: Map.empty,
}

cheapestOpen : (position -> F64), Model position -> Result position [KeyNotFound]*
cheapestOpen = \costFunction, model ->

    folder = \resSmallestSoFar, position ->
        when Map.get model.costs position is
            Err e ->
                Err e

            Ok cost ->
                positionCost = costFunction position

                when resSmallestSoFar is
                    Err _ -> Ok { position, cost: cost + positionCost }
                    Ok smallestSoFar ->
                        if positionCost + cost < smallestSoFar.cost then
                            Ok { position, cost: cost + positionCost }
                        else
                            Ok smallestSoFar

    Set.walk model.openSet (Err KeyNotFound) folder
    |> Result.map (\x -> x.position)

reconstructPath : Map position position, position -> List position
reconstructPath = \cameFrom, goal ->
    when Map.get cameFrom goal is
        Err KeyNotFound ->
            []

        Ok next ->
            List.append (reconstructPath cameFrom next) goal

updateCost : position, position, Model position -> Model position
updateCost = \current, neighbour, model ->
    newCameFrom = Map.insert model.cameFrom neighbour current

    newCosts = Map.insert model.costs neighbour distanceTo

    distanceTo =
        reconstructPath newCameFrom neighbour
        |> List.len
        |> Num.toFrac

    newModel = { model & costs: newCosts, cameFrom: newCameFrom }

    when Map.get model.costs neighbour is
        Err KeyNotFound ->
            newModel

        Ok previousDistance ->
            if distanceTo < previousDistance then
                newModel
            else
                model

findPath : { costFunction : position, position -> F64, moveFunction : position -> Set position, start : position, end : position } -> Result (List position) [KeyNotFound]*
findPath = \{ costFunction, moveFunction, start, end } ->
    astar costFunction moveFunction end (initialModel start)

astar : (position, position -> F64), (position -> Set position), position, Model position -> [Err [KeyNotFound]*, Ok (List position)]*
astar = \costFn, moveFn, goal, model ->
    when cheapestOpen (\position -> costFn goal position) model is
        Err _ ->
            Err KeyNotFound

        Ok current ->
            if current == goal then
                Ok (reconstructPath model.cameFrom goal)
            else
                modelPopped = { model & openSet: Set.remove model.openSet current, evaluated: Set.insert model.evaluated current }

                neighbours = moveFn current

                newNeighbours = Set.difference neighbours modelPopped.evaluated

                modelWithNeighbours = { modelPopped & openSet: Set.union modelPopped.openSet newNeighbours }

                modelWithCosts = Set.walk newNeighbours modelWithNeighbours (\md, nb -> updateCost current nb md)

                astar costFn moveFn goal modelWithCosts
