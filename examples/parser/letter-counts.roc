app "example"
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.8.1/x8URkvfyi9I0QhmVG98roKBUs_AZRkLFwFJVJ3942YA.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.5/KB-TITJ4DfunB88sFBWjCtCGV7LRRDdTH5JUXp4gIb8.tar.br",
    }
    imports [
        cli.Stdout,
        cli.Stderr,
        parser.Core.{ Parser, buildPrimitiveParser, many },
        parser.String.{ parseStr },
    ]
    provides [main] to cli

main =
    lettersInput = "AAAiBByAABBwBtCCCiAyArBBx"
    ifLetterA = \l -> l == A
    when parseStr (many letterParser) lettersInput is
        Ok letters ->
            letters
            |> List.keepIf ifLetterA
            |> List.map \_ -> 1
            |> List.sum
            |> Num.toStr
            |> \countLetterA -> Stdout.line "I counted $(countLetterA) letter A's!"

        Err _ -> Stderr.line "Ooops, something went wrong parsing letters"

Letter : [A, B, C, Other]

letterParser : Parser (List U8) Letter
letterParser =
    input <- buildPrimitiveParser

    valResult =
        when input is
            [] -> Err (ParsingFailure "Nothing to parse")
            ['A', ..] -> Ok A
            ['B', ..] -> Ok B
            ['C', ..] -> Ok C
            _ -> Ok Other

    valResult
    |> Result.map \val -> { val, input: List.dropFirst input 1 }

expect
    input = "B"
    parser = letterParser
    result = parseStr parser input
    result == Ok B

expect
    input = "BCXA"
    parser = many letterParser
    result = parseStr parser input
    result == Ok [B, C, Other, A]



List.mapSum :
    List (Num a),
    (Num a -> Num b)
    { before ? Filter (Num a), after ? Filter (Num b) },
    -> Num a

List.mapProduct :
    List (Num a),
    (Num a -> Num b)
    { before ? Filter (Num a), after ? Filter (Num b) },
    -> Num a

Filter a : [
    KeepFirst U64,
    KeepLast U64,
    KeepIf (a -> Bool),
    KeepFirstIf (a -> Bool) U64,
    KeepLastIf (a -> Bool) U64,
    DropFirst U64,
    DropLast U64,
    DropIf (a -> Bool),
    DropFirstIf (a -> Bool) U64,
    DropLastIf (a -> Bool) U64,
    KeepAll,
]


List.count : ListTransform U64
List.sum : ListTransform U64

Transform a b : [
    Count U64
    Sum ()
]

countAfter : List a, Filter a -> U64
sumAfter : List (Num a), Filter (Num a) -> Num a
productAfter : List (Num a), Filter (Num a) -> Num a
anyAfter : List a, Filter a, (a -> Bool) -> Bool
allAfter : List a, Filter a, (a -> Bool) -> Bool



x =
    things
    |> List.map toNum
    |> List.map Num.abs
    |> List.keepIf isInterestingNum
    |> List.walk { count: 0, stuff: 0 } \{ count, stuff }, num ->
        if num > 0 then
            { count: count + 1, stuff: stuff - 1 }
        else
            { count: count - stuff, stuff: stuff * 2 }


x =
    List.walk things { count: 0, stuff: 0 } \{ count, stuff }, thing ->
        num = toNum thing |> Num.abs

        if isInterestingNum then
            if elem > 0 then
                { count: count + 1, stuff: stuff - 1 }
            else
                { count: count - stuff, stuff: stuff * 2 }
        else
            { count, stuff }









## Only implementable as a lowlevel; exposed publicly so you can implement toIter on your custom collection
fromStartStep : {
    start : state,
    step : state -> [Produce state elem, Continue state, Stop],
} -> Iter elem

## Only implementable as a lowlevel; exposed publicly so you can implement fromIter on your custom collection
toStartStep : Iter elem -> {
    start : state,
    step : state -> [Produce state elem, Continue state, Stop],
}

List.map = \list, fn ->
    list
    |> toIter
    |> Iter.map fn
    |> fromIter


    list
    |> toIter
    |> Iter.map fn
    |> Iter.map fn
    |> Iter.walk \... ->

Iter.map : Iter a, (a -> b) -> Iter b
Iter.map = \iter, fn ->
    { start, step } = toStartStep iter

    fromStartStep {
        start,
        step: \state ->
            when step state is
                Produce s2 elem -> Produce s2 (fn elem)
                Continue s2 -> s2
                Stop -> state
    }

Iter.keepIf : Iter elem, (elem -> Bool) -> Iter elem
Iter.keepIf = \iter, pred ->
    { start, step } = toStartStep iter

    fromStartStep {
        start,
        step: \state ->
            when step state is
                Continue s2 -> s2
                Stop -> state
                Produce s2 elem if pred elem -> Produce s2 elem
                Produce s2 _ -> Continue s2
    }

Iter.takeFirst : Iter elem, U64 -> Iter elem
Iter.takeFirst = \iter, count ->
    { start, step } = toStartStep iter

    fromStartStep {
        start: { count, state: start },
        step: \{ count: remaining, state } ->
            if remaining > 0 then
                when step state is
                    Stop -> Stop
                    Continue s2 -> Continue { count: remaining, state: s2 }
                    Produce s2 elem -> Produce {
                        count: remaining |> Num.subWrap 1,
                        state: s2
                    } elem
            else
                Stop
    }

Iter.map : Iter a, (a -> b) -> Iter b
Iter.map = \iter, fn ->
    { start, step } = toStartStep iter

    fromStartStep {
        start,
        step: \state ->
            when step state is
                Produce s2 elem -> Produce s2 (fn elem)
                Continue s2 -> s2
                Stop -> state
    }

Iter.walk : Iter elem, state, (state, elem -> state) -> state
Iter.walk = \iter, initialAccum, fn ->
    { start, step } = toStartStep iter

    # This is self-tail-recursive
    go = \accum, state ->
        when step state is
            Produce s2 elem -> go (fn accum elem) s2
            Continue s2 -> go accum s2
            Stop -> accum

    go initialAccum start

## We don't actually want this in Roc, but included here for completeness
## since it was in the paper.
Iter.foldr : Iter elem, state, (state, elem -> state) -> state
Iter.foldr = \iter, initialAccum, fn ->
    { start, step } = toStartStep iter

    # This is *not* tail-recursive, and cannot be TCO'd
    go = \state ->
        when step state is
            Produce s2 elem -> fn (go s2) elem
            Continue s2 -> go s2
            Stop -> initialAccum

    go start

Iter.concat : Iter elem, Iter elem -> Iter elem
Iter.concat = \iterA, iterB ->
    { start: startA, step: stepA } = toStartStep iterA
    { start: startB, step: stepB } = toStartStep iterB

    fromStartStep {
        start: A startA,
        step: \state ->
            when state is
                A stateA ->
                    when stepA stateA is
                        Produce s2 elem -> Produce (A s2) elem
                        Continue s2 -> (A s2)
                        Stop -> Continue (B startB)

                B stateB ->
                    when stepB stateB is
                        Produce s2 elem -> Produce (B s2) elem
                        Continue s2 -> (B s2)
                        Stop -> Stop
    }

Iter.mapJoin : Iter a, (a -> Iter b) -> Iter b
Iter.mapJoin = \iterA, iterB, fn ->
    { start: startA, step: stepA } = toStartStep iterA
    { start: startB, step: stepB } = toStartStep iterB

    fromStartStep {
        start: (startA, Nothing),
        step: \(stateA, maybeB) ->
            when maybeB is
                Nothing ->
                    when stepA stateA is
                        Produce s2 elem -> Continue (s2, (Just (fn elem)))
                        Continue s2 -> Continue (s2, Nothing)
                        Stop -> Stop

                Just stateB ->
                    when stepB stateB is
                        Stop -> Continue (stateA, Nothing)
                        Continue s2 ->
                            Continue (stateA, Just (fromStartStep { state: s2, step: stepB }))
                        Produce s2 elem ->
                            Produce (stateA, Just (fromStartStep { state: s2, step: stepB })) elem
    }


Iter.zip : Iter a, Iter b -> Iter (a, b)
Iter.zip = \iterA, iterB ->
    { start: startA, step: stepA } = toStartStep iterA
    { start: startB, step: stepB } = toStartStep iterB

    fromStartStep {
        start: (startA, startB, Nothing),
        step: \(stateA, stateB, maybeElemA) ->
            when maybeElemA is
                Nothing ->
                    when stepA stateA is
                        Produce s2 elemA -> Continue (s2, stateB, Just elemA)
                        Continue s2 -> Continue (s2, stateB, Nothing)
                        Stop -> Stop

                Just elemA ->
                    when stepB stateB is
                        Stop -> Stop
                        Continue s2 -> Continue (stateA, s2, Just elemA)
                        Produce s2 elemB -> Produce (elemA, elemB) (stateA, s2, Nothing)
    }

Iter.map2 : Iter a, Iter b, (a, b -> c) -> Iter c
Iter.map2 = \iterA, iterB, fn ->
    { start: startA, step: stepA } = toStartStep iterA
    { start: startB, step: stepB } = toStartStep iterB

    fromStartStep {
        start: (startA, startB, Nothing),
        step: \(stateA, stateB, maybeElemA) ->
            when maybeElemA is
                Nothing ->
                    when stepA stateA is
                        Produce s2 elemA -> Continue (s2, stateB, Just elemA)
                        Continue s2 -> Continue (s2, stateB, Nothing)
                        Stop -> Stop

                Just elemA ->
                    when stepB stateB is
                        Stop -> Stop
                        Continue s2 -> Continue (stateA, s2, Just elemA)
                        Produce s2 elemB -> Produce (fn elemA elemB) (stateA, s2, Nothing)
    }


ConsList elem := [Empty, Cons elem (ConsList elem)]
    implements Collection { fromStartStep, toStartStep }

fromStartStep :
        {
            start : state,
            step : state -> [Produce state elem, Continue state, Stop],
        }
        -> ConsList elem
fromStartStep = \{ start, step } ->

stepHelp = \accum, state, step, append ->
    when step state is
        Stop -> accum
        Continue s2 -> stepHelp accum s2 step
        Produce s2 elem -> stepHelp (append accum elem)

Collection implements {
    empty : collection elem,
    push : collection elem, elem -> collection elem
    pop : collection elem -> Result { elem, rest: collection elem } [ItWasEmpty],
}

toStartStep : ConsList elem
toStartStep = \
    -> {
        start : state,
        step : state -> [Produce state elem, Continue state, Stop],
    }
        where collection implements Collection,

Iter.fromCollection : collection a -> Iter a
    where collection implements Collection
Iter.fromCollection = \coll ->
    fromStartStep {
        start: coll,
        step: \coll ->
            when splitFirst coll is
                Ok { elem, rest } -> Produce rest elem
                Err ItWasEmpty -> Stop
    }

Iter.toCollection : Iter a -> collection a
    where collection implements Collection
Iter.toCollection = \iter ->
    { start, step } = toStartStep iter

    unfold = \accum, coll ->
        when step coll is
            Stop -> accum
            Continue s2 -> unfold s2
            Produce coll2 elem -> unfold (push accum elem) coll2

    unfold empty start
