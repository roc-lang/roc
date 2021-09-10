app "spycatcher"
    packages { pf: "platform" }
    imports [ pf.Task.{ Task, await }, pf.Stdout, pf.Rand ]
    provides [ main ] to pf


EntityId : [ @EntityId Nat ]


KristyId : [ @KristyId EntityId ]


JanId : [ @JanId EntityId ]


RichardId : [ @RichardId EntityId ]


Richard : { oneHand : [ Kristy KristyId, None ] }


Jan :
    {
        leftHand : [ Kristy KristyId, None ],
        rightHand : [ Kristy KristyId, None ]
    }


Kristy :
    {
        leftHand : [ Jan JanId, None ],
        rightHand : [ Jan JanId, Richard RichardId, None ]
    }


Entity :
    [
        EJan Jan,
        EKristy Kristy,
        ERichard Richard,
        None
    ]


Sim :
    {
        entities : List Entity,
        chainsDropped : Nat
    }


Mix : { jans : Nat, richards : Nat, kristys : Nat }


Model :
    {
        sims : List { mix : Mix, sim : Sim }
    }


iterationsPerSim : Nat
iterationsPerSim = 1_000


maxSims : Nat
maxSims = 10


## Any chains of this length or higher will be dropped.
maxChainLength : Nat
maxChainLength = 6


## Any chains of this length or smaller will not be dropped.
##
## Any chains of length higher than this and lower than maxChainLength will
## have a 50% chance of being kept.
shortChainLength : Nat
shortChainLength = 4


initJan : Jan
initJan =
    { leftHand: None, rightHand: None }


initKristy : Kristy
initKristy =
    { leftHand: None, rightHand: None }



initRichard : Richard
initRichard =
    { oneHand: None }


## 0 to 1000 by 10
mixRanges : List Nat
mixRanges =
    List.range 0 100
        |> List.map \num -> num * 10


initialModel : Model
initialModel =
    { sims: [] }


initialSim : Mix -> Sim
initialSim = \{ jans, richards, kristys } ->
    janEntities : List Entity
    janEntities =
        List.repeat jans initJan
            |> List.map EJan

    richardEntities : List Entity
    richardEntities =
        List.repeat richards initRichard
            |> List.map ERichard

    kristyEntities : List Entity
    kristyEntities =
        List.repeat kristys initKristy
            |> List.map EKristy

    entities : List Entity
    entities =
        List.join [ janEntities, richardEntities, kristyEntities ]

    { entities, chainsDropped: 0 }


dropLongChains : Sim -> Sim
dropLongChains = \originalSim ->
    answer =
        List.walk originalSim.entities
            (\entity, { sim, index } ->
                finalSim =
                    # Only consider the Richard roots
                    when List.get sim.entities index is
                        Ok (ERichard { oneHand }) ->
                            chainLen =
                                sim.entities
                                    |> chainLength (@RichardId (@EntityId index))

                            remove =
                                if chainLen >= maxChainLength then
                                    # if len >= 6, it gets removed
                                    True
                                else if chainLen < shortChainLength then
                                    # if len < 4, it doesn't get removed
                                    False
                                else
                                    # if len is 4 or 5, there's a 50% chance it gets removed.
                                    # We base this on whether the richardId is odd,
                                    # so we don't keep it on one run and remove it the next,
                                    # even though the chain didn't change.
                                    index % 2 == Ok 1

                            if remove then
                                {
                                    entities: removeChain sim.entities (@RichardId (@EntityId index)),
                                    chainsDropped: sim.chainsDropped + 1
                                }
                            else
                                sim

                        _ ->
                            sim

                { sim: finalSim, index: index + 1 }
            )
            { sim: originalSim, index: 0 }

    answer.sim

randHand : Task [ Left, Right ] *
randHand =
    index <- await (Rand.natBetween 0 1)

    if index == 0 then
        Task.succeed Left
    else
        Task.succeed Right

runSimulation : Nat, Sim -> Task Sim *
runSimulation = \iterations, originalSim ->
    Task.succeed originalSim

    # sim =
    #     # Drop any chains of length maxChainLength or higher
    #     dropLongChains originalSim

    # entities =
    #     sim.entities

    # # Pick random entities
    # index1 <- await (Rand.natBetween 0 (List.len entities - 1))
    # index2 <- await (Rand.natBetween 0 (List.len entities - 1))

    # # Pick random hands
    # hand1 <- await randHand
    # hand2 <- await randHand

    # newEntities =
    #     when T (List.get entities index1) (List.get entities index2) is
    #         T (Ok entity1) (Ok entity2) ->
    #             when T entity1 entity2 is
    #                 T (EJan jan) (EKristy kristy) ->
    #                     janKristy entities jan (@EntityId index1) hand1 kristy (@EntityId index2) hand2

    #                 T (EKristy kristy) (EJan jan) ->
    #                     janKristy entities jan (@EntityId index2) hand2 kristy (@EntityId index1) hand1

    #                 T (ERichard richard) (EKristy kristy) ->
    #                     richardKristy entities richard (@EntityId index1) kristy (@EntityId index2) hand2

    #                 T (EKristy kristy) (ERichard richard) ->
    #                     richardKristy entities richard (@EntityId index2) kristy (@EntityId index1) hand1

    #                 T (EJan _) (ERichard _) | T (ERichard _) (EJan _) ->
    #                     # Jan and Richard can't bind
    #                     entities

    #                 T (EJan _) (EJan _) | T (ERichard _) (ERichard _) | T (EKristy _) (EKristy _) ->
    #                     # Same can't bind with same
    #                     entities

    #                 _ ->
    #                     # TODO the compiler should report this as redundant!
    #                     entities

    #         T (Err OutOfBounds) (Ok _) | T (Ok _) (Err OutOfBounds) ->
    #             # The entity wasn't in the dict, presumably because it was in
    #             # a chain that got removed.
    #             entities

    #         _ ->
    #             # TODO the compiler should report this as redundant!
    #             entities

    # if iterations > 0 then
    #     runSimulation (iterations - 1) { sim & entities: newEntities }
    # else
    #     Task.succeed { sim & entities: newEntities }


janKristy : List Entity, Jan, EntityId, [ Left, Right ], Kristy, EntityId, [ Left, Right ] -> List Entity
janKristy = \entities, jan, @EntityId janId, janHand, kristy, @EntityId kristyId, kristyHand ->
    # Jan's left hand can only bind to Kristy's left hand
    if janHand == Right && kristyHand == Right && jan.rightHand == None && kristy.rightHand == None then
        newJan = { jan & rightHand: Kristy (@KristyId (@EntityId kristyId)) }

        newKristy = { kristy & rightHand: Jan (@JanId (@EntityId janId)) }

        entities
            |> List.set janId (EJan newJan)
            |> List.set kristyId (EKristy newKristy)
    # Jan's left hand can bind to Kristy's left hand
    else if janHand == Left && kristyHand == Left && jan.leftHand == None && kristy.leftHand == None then
        newJan = { jan & leftHand: Kristy (@KristyId (@EntityId kristyId)) }

        newKristy = { kristy & leftHand: Jan (@JanId (@EntityId janId)) }

        entities
            |> List.set janId (EJan newJan)
            |> List.set kristyId (EKristy newKristy)
    else
        entities


richardKristy : List Entity, Richard, EntityId, Kristy, EntityId, [ Left, Right ] -> List Entity
richardKristy = \entities, richard, @EntityId richardId, kristy, @EntityId kristyId, kristyHand ->
    # Richard's one hand can bind to Kristy's right hand
    if kristyHand == Right && richard.oneHand == None && kristy.rightHand == None then
        newRichard = ERichard { richard & oneHand: Kristy (@KristyId (@EntityId kristyId)) }

        newKristy = EKristy { kristy & rightHand: Richard (@RichardId (@EntityId richardId)) }

        entities
            |> List.set richardId newRichard
            |> List.set kristyId newKristy
    else
        entities


removeChain : List Entity, RichardId -> List Entity
removeChain = \entries, @RichardId (@EntityId richardId) ->
    when List.get entries richardId is
        Ok (ERichard { oneHand: Kristy (@KristyId (@EntityId kristyId)) }) ->
            when List.get entries kristyId is
                Ok (EKristy kristy) ->
                    entries
                        |> List.set richardId None
                        |> removeChainHelp
                            (@EntityId richardId)
                            Left
                            (@EntityId kristyId)
                            (EKristy kristy)

                Ok _ ->
                    # Debug.todo ("kristyId " ++ String.fromInt kristyId ++ " did not have a Kristy")
                    entries

                Err OutOfBounds ->
                    # Debug.todo ("Could not find kristyId " ++ String.fromInt kristyId)
                    entries


        Ok _ ->
            # Debug.todo ("richardId " ++ String.fromInt richardId ++ " did not have a Richard")
            entries

        Err OutOfBounds ->
            # Debug.todo ("Could not find richardId " ++ String.fromInt richardId)
            entries


removeChainHelp : List Entity, EntityId, [ Left, Right ], EntityId, Entity -> List Entity
removeChainHelp = \originalList, @EntityId chainStart, hand, @EntityId entityId, entity ->
    # Remove this entity from the chain
    entities = List.set originalList entityId None

    when T hand entity is
        T Left (EJan { leftHand: None }) ->
            # Her left hand is empty; we're done!
            entities

        T Left (EJan { leftHand: Kristy (@KristyId (@EntityId kristyId)) }) ->
            when List.get entities kristyId is
                Ok (EKristy kristy) ->
                    # Verify the symmetry - that Kristy's
                    # left hand is indeed bound to this Jan's
                    if kristy.leftHand == Jan (@JanId (@EntityId entityId)) then
                        if kristyId == chainStart then
                            # We've encountered a cycle!
                            # This chain is a loop; we're done.
                            entities
                        else
                            # Recurse on this Jan's left hand
                            removeChainHelp entities
                                (@EntityId chainStart)
                                Left
                                (@EntityId kristyId)
                                (EKristy kristy)
                    else
                        #Debug.todo "Asymmetrical hand joins!" ()
                        entities

                Ok _ ->
                    #Debug.todo "Type mismatch: non-EKristy behind a KristyId!" ()
                    entities

                Err OutOfBounds ->
                    #Debug.todo "No entry found for KristyId!" ()
                    entities

        T Right (EJan { rightHand: None }) ->
            # Her right hand is empty; end of the chain!
            entities

        T Right (EJan { rightHand: Kristy (@KristyId (@EntityId kristyId)) }) ->
            when List.get entities kristyId is
                Ok (EKristy kristy) ->
                    # Verify the symmetry - that Kristy's
                    # right hand is indeed bound to this Jan's
                    if kristy.rightHand == Jan (@JanId (@EntityId entityId)) then
                        if kristyId == chainStart then
                            # We've encountered a cycle!
                            # This chain is a loop; we're done.
                            entities
                        else
                            # Recurse on this Jan's left hand
                            removeChainHelp entities
                                (@EntityId chainStart)
                                Left
                                (@EntityId kristyId)
                                (EKristy kristy)
                    else
                        # Debug.todo "Asymmetrical hand joins!" ()
                        entities

                Ok _ ->
                    #Debug.todo "Type mismatch: non-EKristy behind a KristyId!" ()
                    entities

                Err OutOfBounds ->
                    #Debug.todo "No entry found for KristyId!" ()
                    entities

        T Left (EKristy { leftHand: None }) ->
            # Her left hand is empty; end of the chain!
            entities

        T Left (EKristy { leftHand: Jan (@JanId (@EntityId janId)) }) ->
            when List.get entities janId is
                Ok (EJan jan) ->
                    # Verify the symmetry - that Jan's
                    # left hand is indeed bound to Kristy's
                    if jan.leftHand == Kristy (@KristyId (@EntityId entityId)) then
                        if janId == chainStart then
                            # We've encountered a cycle!
                            # This chain is a loop; we're done.
                            entities
                        else
                            # Recurse on Jan's right hand
                            removeChainHelp entities
                                (@EntityId chainStart)
                                Right
                                (@EntityId janId)
                                (EJan jan)
                    else
                        #Debug.todo "Asymmetrical hand joins!" ()
                        entities

                Ok _ ->
                    #Debug.todo "Type mismatch: non-EJan behind a JanId!" ()
                    entities

                Err OutOfBounds ->
                    #Debug.todo "No entry found for JanId!" ()
                    entities

        T Right (EKristy { rightHand: None }) ->
            # Her right hand is empty; end of the chain!
            entities

        T Right (EKristy { rightHand: Richard _ }) ->
            #Debug.todo "Found a Richard in the chain - this should be impossible!" ()
            entities

        T Right (EKristy { rightHand: Jan (@JanId (@EntityId janId)) }) ->
            when List.get entities janId is
                Ok (EJan jan) ->
                    # Verify the symmetry - that Jan's
                    # left hand is indeed bound to this Kristy's
                    if jan.leftHand == Kristy (@KristyId (@EntityId entityId)) then
                        if janId == chainStart then
                            # We've encountered a cycle!
                            # This chain is a loop; we're done.
                            entities
                        else
                            # Recurse on this Jan's right hand
                            removeChainHelp entities
                                (@EntityId chainStart)
                                Right
                                (@EntityId janId)
                                (EJan jan)
                    else
                        #Debug.todo "Asymmetrical hand joins!" ()
                        entities

                Ok _ ->
                    #Debug.todo "Type mismatch: non-EJan behind a JanId!" ()
                    entities

                Err OutOfBounds ->
                    #Debug.todo "No entry found for JanId!" ()
                    entities

        T _ (ERichard _) ->
            # Debug.todo "Found a Richard in the chain - this should be impossible!" ()
            entities

        _ ->
            # TODO the compiler should report this as redundant!
            entities


chainLength : List Entity, RichardId -> Nat
chainLength = \entries, @RichardId (@EntityId richardId) ->
    when List.get entries richardId is
        Ok (ERichard { oneHand: None }) ->
            0

        Ok (ERichard { oneHand: Kristy (@KristyId (@EntityId kristyId)) }) ->
            when List.get entries kristyId is
                Ok (EKristy kristy) ->
                    chainLengthHelp entries
                        (@EntityId richardId)
                        Left
                        (@EntityId kristyId)
                        (EKristy kristy)
                        1

                Ok _ ->
                    #Debug.todo ("kristyId " ++ String.fromInt kristyId ++ " did not have a Kristy")
                    0

                Err OutOfBounds ->
                    #Debug.todo ("Could not find kristyId " ++ String.fromInt kristyId)
                    0

        Ok _ ->
            #Debug.todo ("richardId " ++ String.fromInt richardId ++ " did not have a Richard")
            0

        Err OutOfBounds ->
            #Debug.todo ("Could not find richardId " ++ String.fromInt richardId)
            0


chainLengthHelp : List Entity, EntityId, [ Left, Right ], EntityId, Entity, Nat -> Nat
chainLengthHelp = \entities, @EntityId chainStart, hand, @EntityId entityId, entity, len ->
    when T hand entity is
        T Left (EJan { leftHand: None }) ->
            # Her left hand is empty; end of the chain!
            len

        T Left (EJan { leftHand: Kristy (@KristyId (@EntityId kristyId)) }) ->
            when List.get entities kristyId is
                Ok (EKristy kristy) ->
                    # Verify the symmetry - that Kristy's
                    # left hand is indeed bound to this Jan's
                    if kristy.leftHand == Jan (@JanId (@EntityId entityId)) then
                        if kristyId == chainStart then
                            # We've encountered a cycle!
                            # This chain is a loop; we're done.
                            len + 1
                        else
                            # Recurse on this Jan's left hand
                            chainLengthHelp entities
                                (@EntityId chainStart)
                                Left
                                (@EntityId kristyId)
                                (EKristy kristy)
                                (len + 1)
                    else
                        #Debug.todo "Asymmetrical hand joins!" ()
                        0

                Ok _ ->
                    #Debug.todo "Type mismatch: non-EKristy behind a KristyId!" ()
                    0

                Err OutOfBounds ->
                    #Debug.todo "No entry found for KristyId!" ()
                    0

        T Right (EJan { rightHand: None }) ->
            # Her right hand is empty; end of the chain!
            len

        T Right (EJan { rightHand: Kristy (@KristyId (@EntityId kristyId)) }) ->
            when List.get entities kristyId is
                Ok (EKristy kristy) ->
                    # Verify the symmetry - that Kristy's
                    # right hand is indeed bound to this Jan's
                    if kristy.rightHand == Jan (@JanId (@EntityId entityId)) then
                        if kristyId == chainStart then
                            # We've encountered a cycle!
                            # This chain is a loop; we're done.
                            len + 1
                        else
                            # Recurse on this Jan's left hand
                            chainLengthHelp
                                entities
                                (@EntityId chainStart)
                                Left
                                (@EntityId kristyId)
                                (EKristy kristy)
                                (len + 1)
                    else
                        #Debug.todo "Asymmetrical hand joins!" ()
                        0

                Ok _ ->
                    #Debug.todo "Type mismatch: non-EKristy behind a KristyId!" ()
                    0

                Err OutOfBounds ->
                    #Debug.todo "No entry found for KristyId!" ()
                    0

        T Left (EKristy { leftHand: None }) ->
            # Her left hand is empty; end of the chain!
            len

        T Left (EKristy { leftHand: Jan (@JanId (@EntityId janId)) }) ->
            when List.get entities janId is
                Ok (EJan jan) ->
                    # Verify the symmetry - that Jan's
                    # left hand is indeed bound to Kristy's
                    if jan.leftHand == Kristy (@KristyId (@EntityId entityId)) then
                        if janId == chainStart then
                            # We've encountered a cycle!
                            # This chain is a loop; we're done.
                            len + 1
                        else
                            # Recurse on Jan's right hand
                            chainLengthHelp entities
                                (@EntityId chainStart)
                                Right
                                (@EntityId janId)
                                (EJan jan)
                                (len + 1)
                    else
                        # Debug.todo "Asymmetrical hand joins!" ()
                        0

                Ok _ ->
                    # Debug.todo "Type mismatch: non-EJan behind a JanId!" ()
                    0

                Err OutOfBounds ->
                    # Debug.todo "No entry found for JanId!" ()
                    0

        T Right (EKristy { rightHand: None }) ->
            # Her right hand is empty; end of the chain!
            len

        T Right (EKristy { rightHand: Richard _ }) ->
            #Debug.todo "Found a Richard in the chain - this should be impossible!" ()
            0

        T Right (EKristy { rightHand: Jan (@JanId (@EntityId janId)) }) ->
            when List.get entities janId is
                Ok (EJan jan) ->
                    # Verify the symmetry - that Jan's
                    # left hand is indeed bound to this Kristy's
                    if jan.leftHand == Kristy (@KristyId (@EntityId entityId)) then
                        if janId == chainStart then
                            # We've encountered a cycle!
                            # This chain is a loop; we're done.
                            len + 1
                        else
                            # Recurse on this Jan's right hand
                            chainLengthHelp entities
                                (@EntityId chainStart)
                                Right
                                (@EntityId janId)
                                (EJan jan)
                                (len + 1)
                    else
                        #Debug.todo "Asymmetrical hand joins!" ()
                        0

                Ok _ ->
                    #Debug.todo "Type mismatch: non-EJan behind a JanId!" ()
                    0

                Err OutOfBounds ->
                    #Debug.todo "No entry found for JanId!" ()
                    0

        T _ (ERichard richard) ->
            #Debug.todo "Found a Richard in the chain - this should be impossible!" ()
            0

        _ ->
            # TODO the compiler should report this as redundant!
            0

step : Model -> Task Model *
step = \model ->
    allMixes : List (List (List Mix))
    allMixes =
        List.map mixRanges \richards ->
            List.map mixRanges \kristys ->
                List.map mixRanges \jans ->
                    { richards, jans, kristys }

    mixes : List Mix
    mixes = List.join (List.join allMixes)

    simNum = Str.fromInt (1 + List.len model.sims)

    totalSims = Str.fromInt maxSims

    {} <- await (Stdout.line "Running simulation #\(simNum) of \(totalSims)...")

    newSims <- await
        (
            List.walk mixes
                (\mix, task ->
                    sims <- await task

                    sim <- await (runSimulation iterationsPerSim (initialSim mix))

                    List.append sims { mix, sim }
                        |> Task.succeed
                )
                (Task.succeed [])
        )

    # Workaround for https://github.com/rtfeldman/roc/issues/1677
    newSimsAnn : List { mix : Mix, sim : Sim }
    newSimsAnn = newSims

    sims : List { mix : Mix, sim : Sim }
    sims = List.concat model.sims newSims

    newModel = { model & sims }

    # Recurse until we no longer need to run more sims
    if List.len sims < maxSims then
        step newModel
    else
        Task.succeed newModel


# mixToString : Mix -> String
# mixToString mix =
#     [ String.fromInt mix.richards
#     , "x R /"
#     , String.fromInt mix.kristys
#     , "x K /"
#     , String.fromInt mix.jans
#     , "x J"
#     ]
#         |> String.join ""


# view : Model -> Html msg
# view model =
#     let
#         scored =
#             model.sims
#                 |> List.indexedMap (\index ( mix, sim ) -> ( scoreSim sim, mix, index ))
#                 |> List.sortBy (\( score, _, _ ) -> -score)
#     in
#     div []
#         [ h1 [] [ text "Simulations" ]
#         , div [] [ text ("Running " ++ String.fromInt (List.length model.sims) ++ " simulations") ]
#         , div [] <|
#             List.map
#                 (\( score, mix, index ) ->
#                     let
#                         title =
#                             [ "Simulation #"
#                             , String.fromInt (index + 1)
#                             , ": "
#                             , mixToString mix
#                             , ", "
#                             , String.fromInt score
#                             , " chains of length 4+"
#                             ]
#                                 |> String.join ""
#                     in
#                     div []
#                         [ h2 [] [ text title ]

#                         -- , div [] [ viewSim sim ]
#                         ]
#                 )
#                 scored
#         ]


# viewSim : Sim -> Html msg
# viewSim sim =
#     Dict.foldl
#         (\id entity list ->
#             -- Only render the Richard roots
#             case Dict.get id sim.entities of
#                 Just (ERichard { oneHand }) ->
#                     let
#                         chainLen =
#                             sim.entities
#                                 |> chainLength (RichardId id)
#                                 |> String.fromInt

#                         str =
#                             chainLen
#                                 ++ ": "
#                                 ++ richardToString
#                                     oneHand
#                                     sim.entities
#                                     id
#                     in
#                     li [] [ text str ] :: list

#                 _ ->
#                     list
#         )
#         []
#         sim.entities
#         |> ol []


# richardToString : Maybe KristyId -> Dict EntityId Entity -> EntityId -> String
# richardToString oneHand dict richardId =
#     case oneHand of
#         Just (KristyId kristyId) ->
#             case Dict.get kristyId dict of
#                 Just (EKristy kristy) ->
#                     let
#                         rk =
#                             String.concat
#                                 [ "R"
#                                 , String.fromInt richardId
#                                 , " ➞ K"
#                                 , String.fromInt kristyId
#                                 ]
#                     in
#                     -- We know what's in Kristy's right hand (Richard!)
#                     -- so recurse on her left hand
#                     case kristy.leftHand of
#                         Just (JanId janId) ->
#                             case Dict.get janId dict of
#                                 Just (EJan jan) ->
#                                     -- We know what's in Jan's left hand (Kristy!)
#                                     -- so recurse on her right hand
#                                     String.concat
#                                         [ rk
#                                         , " ➞ "
#                                         , janRightHandToString jan.rightHand dict janId
#                                         ]

#                                 Just _ ->
#                                     Debug.todo "JanId was not associated with an EJan"

#                                 Nothing ->
#                                     Debug.todo "JanId not found"

#                         Nothing ->
#                             rk

#                 Just _ ->
#                     Debug.todo "KristyId was not associated with an EKristy"

#                 Nothing ->
#                     Debug.todo "KristyId not found"

#         Nothing ->
#             -- Nothing in the one hand; just an R!
#             "R" ++ String.fromInt richardId


# janRightHandToString : Maybe KristyId -> Dict EntityId Entity -> EntityId -> String
# janRightHandToString rightHand dict janId =
#     case rightHand of
#         Just (KristyId kristyId) ->
#             case Dict.get kristyId dict of
#                 Just (EKristy kristy) ->
#                     let
#                         jk =
#                             String.concat
#                                 [ "J"
#                                 , String.fromInt janId
#                                 , " ➞ K"
#                                 , String.fromInt kristyId
#                                 ]
#                     in
#                     case kristy.leftHand of
#                         Just (JanId janId2) ->
#                             case Dict.get janId2 dict of
#                                 Just (EJan jan) ->
#                                     -- We know what's in Jan's left hand (Kristy!)
#                                     -- so recurse on her right hand
#                                     String.concat
#                                         [ jk
#                                         , " ➞ "
#                                         , janRightHandToString jan.rightHand dict janId2
#                                         ]

#                                 Just _ ->
#                                     Debug.todo "JanId was not associated with an EJan"

#                                 Nothing ->
#                                     Debug.todo "JanId not found"

#                         Nothing ->
#                             jk

#                 Just _ ->
#                     Debug.todo "JanId was not associated with an EJan"

#                 Nothing ->
#                     Debug.todo "JanId not found"

#         Nothing ->
#             -- Nothing in the one hand; just a J!
#             "J" ++ String.fromInt janId


scoreSim : Sim -> Nat
scoreSim = \sim ->
    answer = List.walk sim.entities
        (\entity, { index, score } ->
            # Only the Richard roots can add to the score
            newScore =
                when List.get sim.entities index is
                    Ok (ERichard { oneHand }) ->
                        len = sim.entities
                            |> chainLength (@RichardId (@EntityId index))

                        # Any chain of length > 4 is long
                        if len > shortChainLength then
                            score + 1
                        else
                            score

                    _ ->
                        score

            { index: index + 1, score: newScore }
        )
        # All the dropped chains count for 1
        # (they were dropped because they were long)
        { index: 0, score: sim.chainsDropped }

    answer.score


main : Task {} *
main =
    times = Str.fromInt maxSims

    {} <- await (Stdout.line "Running simulation \(times) times...")

    model <- await (step initialModel)

    Stdout.line "Done!"
