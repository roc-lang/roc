app "astar"
    packages { base: "platform" }
    imports [base.Task]
    provides [ main ] to base

# astar : (position, position -> F64), (position -> Set position), position, Model position -> Result (List position) {}

fromList : List a -> Set a
fromList = \list -> List.walk list (\x, a -> Set.insert a x) Set.empty

main : Task.Task {} []
main =

    step = \n ->
        when n is
            1 -> fromList [ 2, 3 ]
            2 -> fromList [ 4 ]
            3 -> fromList [ 4 ]
            4 -> fromList []
            _ -> fromList []

    cost = \_, _ -> 1

    when astar cost step 4 (initialModel 1) is
        Ok _path -> 
            Task.putLine "yay"
        Err _ -> 
            Task.putLine "nay"

Model position :
    {
        evaluated : Set position,
        openSet : Set position,
        costs : Dict position F64,
        cameFrom : Dict position position
    }

initialModel : position -> Model position
initialModel = \start ->
    {
        evaluated : Set.empty, 
        openSet : Set.singleton start,
        costs : Dict.singleton start 0, 
        cameFrom : Dict.empty
    }

sortBy : List a, (a -> b) -> List a
sortBy = \list, _toCmp -> list

filterMap : List a, (a -> Result b *) -> List b
filterMap = \list, toResult ->
    List.walk list (\element, accum ->
        when toResult element is
            Ok value ->
                List.append accum value

            Err _ ->
                accum
        )
        []

cheapestOpen : (position -> F64), Model position -> Result position {}
cheapestOpen = \costFn, model ->
    model.openSet
        |> Set.toList
        |> filterMap (\position ->
            when Dict.get model.costs position is
                Err _ ->
                    Err {}

                Ok cost ->
                    Ok { position, cost: cost + costFn position }
                    )
        |> sortBy .cost
        |> List.first
        |> Result.map .position
        |> Result.mapErr (\_ -> {})


reconstructPath : Dict position position, position -> List position
reconstructPath = \cameFrom, goal ->
    when Dict.get cameFrom goal is
        Err _ ->
            []

        Ok next ->
            List.append (reconstructPath cameFrom next) goal

# TODO shuffle things around so we get reuse
updateCost : position, position, Model position -> Model position
updateCost = \current, neighbor, model ->
    newCameFrom =
        Dict.insert model.cameFrom neighbor current

    newCosts =
        Dict.insert model.costs neighbor distanceTo

    distanceTo =
        reconstructPath newCameFrom neighbor
            |> List.len
            |> Num.toFloat

    newModel =
        { model & 
            costs: newCosts,
            cameFrom: newCameFrom
        }

    when Dict.get model.costs neighbor is
        Err _ ->
            newModel

        Ok previousDistance ->
            if distanceTo < previousDistance then
                newModel

            else
                model

astar : (position, position -> F64), (position -> Set position), position, Model position -> Result (List position) {}
astar = \costFn, moveFn, goal, model ->
    when cheapestOpen (\source -> costFn source goal) model is
        Err {} ->
            Err {}

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
                    Set.difference modelPopped.evaluated neighbors

                modelWithNeighbors =
                    { modelPopped &
                        openSet: Set.union modelPopped.openSet newNeighbors
                    }

                modelWithCosts =
                    Set.walk newNeighbors (\n, m -> updateCost current n m) modelWithNeighbors

                astar costFn moveFn goal modelWithCosts

      
