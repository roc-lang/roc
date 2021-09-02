app "spycatcher"
    packages { pf: "platform" }
    imports [ pf.Task.{ Task, await }, pf.Stdout, pf.Rand ]
    provides [ main ] to pf


main : Task {} *
main =
    nat <- await Rand.nat

    natStr = Str.fromInt nat

    Stdout.line "Here's a random number: \(natStr)"


EntityId : [ @EntityId Nat ]


KristyId : [ @KristyId EntityId ]


JanId : [ @JanId EntityId ]


RichardId : [ @RichardId EntityId ]


iterationsPerSim : Nat
iterationsPerSim = 1_000


maxSims : Nat
maxSims = 100


## Any chains of this length or higher will be dropped.
maxChainLength : Nat
maxChainLength = 6


## Any chains of this length or smaller will not be dropped.
##
## Any chains of length higher than this and lower than maxChainLength will
## have a 50% chance of being kept.
shortChainLength : Nat
shortChainLength = 4


Jan :
    {
        leftHand : [ Kristy KristyId, None ],
        rightHand : [ Kristy KristyId, None ]
    }


initJan : Jan
initJan =
    { leftHand: None, rightHand: None }


Kristy :
    {
        leftHand : [ Jan JanId, None ],
        rightHand : [ Jan JanId, Richard RichardId, None ]
    }


initKristy : Kristy
initKristy =
    { leftHand: None, rightHand: None }


Richard : { oneHand : [ Kristy KristyId, None ] }


Entity :
    [
        EJan Jan,
        EKristy Kristy,
        ERichard Richard
    ]


initRichard : Richard
initRichard =
    { oneHand: None }


Model :
    {
        sims : List { mix : Mix, sim : Sim }
    }


initialModel : Model
initialModel =
    { sims: [] }


## 0 to 1000 by 10
mixRanges : List Nat
mixRanges =
    List.range 0 100
        |> List.map \num -> num * 10


Sim :
    {
        entities : List Entity,
        chainsDropped : Nat
    }


Mix : { jans : Nat, richards : Nat, kristys : Nat }


initialSim : Mix -> Sim
initialSim = \{ jans, richards, kristys } ->
    janEntities =
        List.repeat jans initJan
            |> List.map EJan

    richardEntities =
        List.repeat richards initRichard
            |> List.map ERichard

    kristyEntities =
        List.repeat kristys initKristy
            |> List.map EKristy

    entities =
        List.join [ janEntities, richardEntities, kristyEntities ]

    { entities: entities, chainsDropped: 0 }


randHand : Task [ Left, Right ] *
randHand =
    Task.map Rand.nat \nat ->
        when nat % 2 is
            Ok(0) -> Left
            _ -> Right


dropLongChains : Sim -> Sim
dropLongChains = \originalSim ->
    List.walk originalSim.entities
        (\{ sim, index }, entity ->
            finalSim =
                # Only consider the Richard roots
                when List.get index sim.entities is
                    Just (ERichard { oneHand }) ->
                        chainLen =
                            sim.entities
                                |> chainLength (RichardId id)

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
                                index % 2 == 1

                        if remove then
                            {
                                entities: removeChain (RichardId index) sim.entities,
                                chainsDropped: sim.chainsDropped + 1
                            }
                        else
                            sim

                    _ ->
                        sim

            { sim: finalSim, index: index + 1 }
        )
        { sim: originalSim, index: 0 }

# dropLongChains : Sim -> Sim
# dropLongChains originalSim =
#     Dict.foldl
#         (\id entity sim ->
#             -- Only consider the Richard roots
#             case Dict.get id sim.entities of
#                 Just (ERichard { oneHand }) ->
#                     let
#                         chainLen =
#                             sim.entities
#                                 |> chainLength (RichardId id)

#                         remove =
#                             if chainLen >= maxChainLength then
#                                 -- if len >= 6, it gets removed
#                                 True

#                             else if chainLen < shortChainLength then
#                                 -- if len < 4, it doesn't get removed
#                                 False

#                             else
#                                 -- if len is 4 or 5, there's a 50% chance it gets removed.
#                                 -- We base this on whether the richardId is odd,
#                                 -- so we don't keep it on one run and remove it the next,
#                                 -- even though the chain didn't change.
#                                 remainderBy 2 id == 1
#                     in
#                     if remove then
#                         { entities = removeChain (RichardId id) sim.entities
#                         , chainsDropped = sim.chainsDropped + 1
#                         }

#                     else
#                         sim

#                 _ ->
#                     sim
#         )
#         originalSim
#         originalSim.entities


# runSimulation : Seed -> Int -> Sim -> ( Sim, Seed )
# runSimulation seed iterations originalSim =
#     let
#         sim =
#             -- Drop any chains of length maxChainLength or higher
#             dropLongChains originalSim

#         dict =
#             sim.entities

#         -- Drop long chains
#         -- Pick a random entity
#         ( index1, seed1 ) =
#             Random.step
#                 (Random.int 0 (Dict.size dict - 1))
#                 seed

#         -- Pick another random entity
#         ( index2, seed2 ) =
#             Random.step
#                 (Random.int 0 (Dict.size dict - 1))
#                 seed1

#         -- Pick a random hand
#         ( hand1, seed3 ) =
#             Random.step randHand seed2

#         -- Pick another random hand
#         ( hand2, seed4 ) =
#             Random.step randHand seed3

#         newDict =
#             case ( Dict.get index1 dict, Dict.get index2 dict ) of
#                 ( Just entity1, Just entity2 ) ->
#                     case ( entity1, entity2 ) of
#                         ( EJan jan, EKristy kristy ) ->
#                             janKristy jan index1 hand1 kristy index2 hand2 dict

#                         ( EKristy kristy, EJan jan ) ->
#                             janKristy jan index2 hand2 kristy index1 hand1 dict

#                         ( ERichard richard, EKristy kristy ) ->
#                             richardKristy richard index1 kristy index2 hand2 dict

#                         ( EKristy kristy, ERichard richard ) ->
#                             richardKristy richard index2 kristy index1 hand1 dict

#                         ( EJan jan, ERichard richard ) ->
#                             -- Jan and Richard can't bind
#                             dict

#                         ( ERichard richard, EJan jan ) ->
#                             -- Jan and Richard can't bind
#                             dict

#                         ( EJan j1, EJan j2 ) ->
#                             -- Same can't bind with same
#                             dict

#                         ( ERichard j1, ERichard j2 ) ->
#                             -- Same can't bind with same
#                             dict

#                         ( EKristy k1, EKristy k2 ) ->
#                             -- Same can't bind with same
#                             dict

#                 ( Nothing, _ ) ->
#                     -- The entity wasn't in the dict, presumably because it was in
#                     -- a chain that got removed.
#                     dict

#                 ( _, Nothing ) ->
#                     -- The entity wasn't in the dict, presumably because it was in
#                     -- a chain that got removed.
#                     dict
#     in
#     if iterations > 0 then
#         runSimulation seed4 (iterations - 1) { sim | entities = newDict }

#     else
#         ( { sim | entities = newDict }, seed4 )


# janKristy : Jan -> EntityId -> Hand -> Kristy -> EntityId -> Hand -> Dict EntityId Entity -> Dict EntityId Entity
# janKristy jan janId janHand kristy kristyId kristyHand dict =
#     -- Jan's left hand can only bind to Kristy's left hand
#     if janHand == Right && kristyHand == Right && jan.rightHand == Nothing && kristy.rightHand == Nothing then
#         let
#             newJan =
#                 { jan | rightHand = Just (KristyId kristyId) }

#             newKristy =
#                 { kristy | rightHand = Just (IsJan (JanId janId)) }

#             newDict =
#                 Dict.insert janId (EJan newJan) dict
#                     |> Dict.insert kristyId (EKristy newKristy)
#         in
#         newDict
#         -- Jan's left hand can bind to Kristy's left hand

#     else if janHand == Left && kristyHand == Left && jan.leftHand == Nothing && kristy.leftHand == Nothing then
#         let
#             newJan =
#                 { jan | leftHand = Just (KristyId kristyId) }

#             newKristy =
#                 { kristy | leftHand = Just (JanId janId) }

#             newDict =
#                 Dict.insert janId (EJan newJan) dict
#                     |> Dict.insert kristyId (EKristy newKristy)
#         in
#         newDict

#     else
#         dict


# richardKristy : Richard -> EntityId -> Kristy -> EntityId -> Hand -> Dict EntityId Entity -> Dict EntityId Entity
# richardKristy richard richardId kristy kristyId kristyHand dict =
#     -- Richard's one hand can bind to Kristy's right hand
#     if kristyHand == Right && richard.oneHand == Nothing && kristy.rightHand == Nothing then
#         let
#             newRichard =
#                 ERichard { richard | oneHand = Just (KristyId kristyId) }

#             newKristy =
#                 EKristy { kristy | rightHand = Just (IsRichard (RichardId richardId)) }

#             newDict =
#                 Dict.insert richardId newRichard dict
#                     |> Dict.insert kristyId newKristy
#         in
#         newDict

#     else
#         dict


# removeChain : RichardId -> Dict EntityId Entity -> Dict EntityId Entity
# removeChain (RichardId richardId) dict =
#     case Dict.get richardId dict of
#         Just (ERichard { oneHand }) ->
#             case oneHand of
#                 Just (KristyId kristyId) ->
#                     case Dict.get kristyId dict of
#                         Just (EKristy kristy) ->
#                             let
#                                 newDict =
#                                     Dict.remove richardId dict
#                             in
#                             removeChainHelp richardId Left kristyId (EKristy kristy) newDict

#                         Just _ ->
#                             Debug.todo ("kristyId " ++ String.fromInt kristyId ++ " did not have a Kristy")

#                         Nothing ->
#                             Debug.todo ("Could not find kristyId " ++ String.fromInt kristyId)

#                 Nothing ->
#                     dict

#         Just _ ->
#             Debug.todo ("richardId " ++ String.fromInt richardId ++ " did not have a Richard")

#         Nothing ->
#             Debug.todo ("Could not find richardId " ++ String.fromInt richardId)


# removeChainHelp : EntityId -> Hand -> EntityId -> Entity -> Dict EntityId Entity -> Dict EntityId Entity
# removeChainHelp chainStart hand entityId entity originalDict =
#     let
#         -- Remove this entity from the chain
#         dict =
#             Dict.remove entityId originalDict
#     in
#     case ( entity, hand ) of
#         ( EJan jan, Left ) ->
#             case jan.leftHand of
#                 Nothing ->
#                     -- Her left hand is empty; we're done!
#                     dict

#                 Just (KristyId kristyId) ->
#                     case Dict.get kristyId dict of
#                         Just (EKristy kristy) ->
#                             -- Verify the symmetry - that Kristy's
#                             -- left hand is indeed bound to this Jan's
#                             if kristy.leftHand == Just (JanId entityId) then
#                                 if kristyId == chainStart then
#                                     -- We've encountered a cycle!
#                                     -- This chain is a loop; we're done.
#                                     dict

#                                 else
#                                     -- Recurse on this Jan's left hand
#                                     removeChainHelp chainStart
#                                         Left
#                                         kristyId
#                                         (EKristy kristy)
#                                         dict

#                             else
#                                 Debug.todo "Asymmetrical hand joins!" ()

#                         Just _ ->
#                             Debug.todo "Type mismatch: non-EKristy behind a KristyId!" ()

#                         Nothing ->
#                             Debug.todo "No entry found for KristyId!" ()

#         ( EJan jan, Right ) ->
#             case jan.rightHand of
#                 Nothing ->
#                     -- Her right hand is empty; end of the chain!
#                     dict

#                 Just (KristyId kristyId) ->
#                     case Dict.get kristyId dict of
#                         Just (EKristy kristy) ->
#                             -- Verify the symmetry - that Kristy's
#                             -- right hand is indeed bound to this Jan's
#                             if kristy.rightHand == Just (IsJan (JanId entityId)) then
#                                 if kristyId == chainStart then
#                                     -- We've encountered a cycle!
#                                     -- This chain is a loop; we're done.
#                                     dict

#                                 else
#                                     -- Recurse on this Jan's left hand
#                                     removeChainHelp chainStart
#                                         Left
#                                         kristyId
#                                         (EKristy kristy)
#                                         dict

#                             else
#                                 Debug.todo "Asymmetrical hand joins!" ()

#                         Just _ ->
#                             Debug.todo "Type mismatch: non-EKristy behind a KristyId!" ()

#                         Nothing ->
#                             Debug.todo "No entry found for KristyId!" ()

#         ( EKristy kristy, Left ) ->
#             case kristy.leftHand of
#                 Nothing ->
#                     -- Her left hand is empty; end of the chain!
#                     dict

#                 Just (JanId janId) ->
#                     case Dict.get janId dict of
#                         Just (EJan jan) ->
#                             -- Verify the symmetry - that Jan's
#                             -- left hand is indeed bound to Kristy's
#                             if jan.leftHand == Just (KristyId entityId) then
#                                 if janId == chainStart then
#                                     -- We've encountered a cycle!
#                                     -- This chain is a loop; we're done.
#                                     dict

#                                 else
#                                     -- Recurse on Jan's right hand
#                                     removeChainHelp chainStart
#                                         Right
#                                         janId
#                                         (EJan jan)
#                                         dict

#                             else
#                                 Debug.todo "Asymmetrical hand joins!" ()

#                         Just _ ->
#                             Debug.todo "Type mismatch: non-EJan behind a JanId!" ()

#                         Nothing ->
#                             Debug.todo "No entry found for JanId!" ()

#         ( EKristy kristy, Right ) ->
#             case kristy.rightHand of
#                 Nothing ->
#                     -- Her right hand is empty; end of the chain!
#                     dict

#                 Just (IsRichard _) ->
#                     Debug.todo "Found a Richard in the chain - this should be impossible!" ()

#                 Just (IsJan (JanId janId)) ->
#                     case Dict.get janId dict of
#                         Just (EJan jan) ->
#                             -- Verify the symmetry - that Jan's
#                             -- left hand is indeed bound to this Kristy's
#                             if jan.leftHand == Just (KristyId entityId) then
#                                 if janId == chainStart then
#                                     -- We've encountered a cycle!
#                                     -- This chain is a loop; we're done.
#                                     dict

#                                 else
#                                     -- Recurse on this Jan's right hand
#                                     removeChainHelp chainStart
#                                         Right
#                                         janId
#                                         (EJan jan)
#                                         dict

#                             else
#                                 Debug.todo "Asymmetrical hand joins!" ()

#                         Just _ ->
#                             Debug.todo "Type mismatch: non-EJan behind a JanId!" ()

#                         Nothing ->
#                             Debug.todo "No entry found for JanId!" ()

#         ( ERichard richard, _ ) ->
#             Debug.todo "Found a Richard in the chain - this should be impossible!" ()


# chainLength : RichardId, List Entity -> Nat
# chainLength = \RichardId richardId, dict ->
#     0
#     case Dict.get richardId dict of
#         Just (ERichard { oneHand }) ->
#             case oneHand of
#                 Just (KristyId kristyId) ->
#                     case Dict.get kristyId dict of
#                         Just (EKristy kristy) ->
#                             chainLengthHelp richardId Left kristyId (EKristy kristy) dict 1

#                         Just _ ->
#                             Debug.todo ("kristyId " ++ String.fromInt kristyId ++ " did not have a Kristy")

#                         Nothing ->
#                             Debug.todo ("Could not find kristyId " ++ String.fromInt kristyId)

#                 Nothing ->
#                     0

#         Just _ ->
#             Debug.todo ("richardId " ++ String.fromInt richardId ++ " did not have a Richard")

#         Nothing ->
#             Debug.todo ("Could not find richardId " ++ String.fromInt richardId)


# chainLengthHelp : EntityId -> Hand -> EntityId -> Entity -> Dict EntityId Entity -> Int -> Int
# chainLengthHelp chainStart hand entityId entity dict len =
#     case ( entity, hand ) of
#         ( EJan jan, Left ) ->
#             case jan.leftHand of
#                 Nothing ->
#                     -- Her left hand is empty; end of the chain!
#                     len

#                 Just (KristyId kristyId) ->
#                     case Dict.get kristyId dict of
#                         Just (EKristy kristy) ->
#                             -- Verify the symmetry - that Kristy's
#                             -- left hand is indeed bound to this Jan's
#                             if kristy.leftHand == Just (JanId entityId) then
#                                 if kristyId == chainStart then
#                                     -- We've encountered a cycle!
#                                     -- This chain is a loop; we're done.
#                                     len + 1

#                                 else
#                                     -- Recurse on this Jan's left hand
#                                     chainLengthHelp chainStart
#                                         Left
#                                         kristyId
#                                         (EKristy kristy)
#                                         dict
#                                         (len + 1)

#                             else
#                                 Debug.todo "Asymmetrical hand joins!" ()

#                         Just _ ->
#                             Debug.todo "Type mismatch: non-EKristy behind a KristyId!" ()

#                         Nothing ->
#                             Debug.todo "No entry found for KristyId!" ()

#         ( EJan jan, Right ) ->
#             case jan.rightHand of
#                 Nothing ->
#                     -- Her right hand is empty; end of the chain!
#                     len

#                 Just (KristyId kristyId) ->
#                     case Dict.get kristyId dict of
#                         Just (EKristy kristy) ->
#                             -- Verify the symmetry - that Kristy's
#                             -- right hand is indeed bound to this Jan's
#                             if kristy.rightHand == Just (IsJan (JanId entityId)) then
#                                 if kristyId == chainStart then
#                                     -- We've encountered a cycle!
#                                     -- This chain is a loop; we're done.
#                                     len + 1

#                                 else
#                                     -- Recurse on this Jan's left hand
#                                     chainLengthHelp chainStart
#                                         Left
#                                         kristyId
#                                         (EKristy kristy)
#                                         dict
#                                         (len + 1)

#                             else
#                                 Debug.todo "Asymmetrical hand joins!" ()

#                         Just _ ->
#                             Debug.todo "Type mismatch: non-EKristy behind a KristyId!" ()

#                         Nothing ->
#                             Debug.todo "No entry found for KristyId!" ()

#         ( EKristy kristy, Left ) ->
#             case kristy.leftHand of
#                 Nothing ->
#                     -- Her left hand is empty; end of the chain!
#                     len

#                 Just (JanId janId) ->
#                     case Dict.get janId dict of
#                         Just (EJan jan) ->
#                             -- Verify the symmetry - that Jan's
#                             -- left hand is indeed bound to Kristy's
#                             if jan.leftHand == Just (KristyId entityId) then
#                                 if janId == chainStart then
#                                     -- We've encountered a cycle!
#                                     -- This chain is a loop; we're done.
#                                     len + 1

#                                 else
#                                     -- Recurse on Jan's right hand
#                                     chainLengthHelp chainStart
#                                         Right
#                                         janId
#                                         (EJan jan)
#                                         dict
#                                         (len + 1)

#                             else
#                                 Debug.todo "Asymmetrical hand joins!" ()

#                         Just _ ->
#                             Debug.todo "Type mismatch: non-EJan behind a JanId!" ()

#                         Nothing ->
#                             Debug.todo "No entry found for JanId!" ()

#         ( EKristy kristy, Right ) ->
#             case kristy.rightHand of
#                 Nothing ->
#                     -- Her right hand is empty; end of the chain!
#                     len

#                 Just (IsRichard _) ->
#                     Debug.todo "Found a Richard in the chain - this should be impossible!" ()

#                 Just (IsJan (JanId janId)) ->
#                     case Dict.get janId dict of
#                         Just (EJan jan) ->
#                             -- Verify the symmetry - that Jan's
#                             -- left hand is indeed bound to this Kristy's
#                             if jan.leftHand == Just (KristyId entityId) then
#                                 if janId == chainStart then
#                                     -- We've encountered a cycle!
#                                     -- This chain is a loop; we're done.
#                                     len + 1

#                                 else
#                                     -- Recurse on this Jan's right hand
#                                     chainLengthHelp chainStart
#                                         Right
#                                         janId
#                                         (EJan jan)
#                                         dict
#                                         (len + 1)

#                             else
#                                 Debug.todo "Asymmetrical hand joins!" ()

#                         Just _ ->
#                             Debug.todo "Type mismatch: non-EJan behind a JanId!" ()

#                         Nothing ->
#                             Debug.todo "No entry found for JanId!" ()

#         ( ERichard richard, _ ) ->
#             Debug.todo "Found a Richard in the chain - this should be impossible!" ()


# main : Program () Model Msg
# main =
#     Browser.element
#         { init = \_ -> step initialModel
#         , update = update
#         , view = view
#         , subscriptions = \_ -> Sub.none
#         }


# type Msg
#     = Step


# update : Msg -> Model -> ( Model, Cmd Msg )
# update msg model =
#     case msg of
#         Step ->
#             if List.length model.sims < maxSims then
#                 step model

#             else
#                 ( model, Cmd.none )


# step : Model -> ( Model, Cmd Msg )
# step model =
#     let
#         allMixes : List Mix
#         allMixes =
#             mixRanges
#                 |> List.concatMap
#                     (\richards ->
#                         mixRanges
#                             |> List.concatMap
#                                 (\kristys ->
#                                     mixRanges
#                                         |> List.map
#                                             (\jans -> { richards = richards, jans = jans, kristys = kristys })
#                                 )
#                     )

#         ( finalSeed, simMixPairs ) =
#             List.foldl
#                 (\mix ( seed, sims ) ->
#                     let
#                         ( sim, nextSeed ) =
#                             runSimulation model.seed iterationsPerSim (initialSim mix)
#                     in
#                     ( nextSeed, ( mix, sim ) :: sims )
#                 )
#                 ( model.seed, [] )
#                 allMixes

#         cmd =
#             Process.sleep 1
#                 |> Task.perform (\() -> Step)
#     in
#     ( { model | sims = model.sims ++ simMixPairs, seed = finalSeed }, cmd )


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


# scoreSim : Sim -> Int
# scoreSim sim =
#     Dict.foldl
#         (\id entity score ->
#             -- Only the Richard roots can add to the score
#             case Dict.get id sim.entities of
#                 Just (ERichard { oneHand }) ->
#                     -- Any chain of length > 4 is long
#                     if chainLength (RichardId id) sim.entities > shortChainLength then
#                         score + 1

#                     else
#                         score

#                 _ ->
#                     score
#         )
#         -- All the dropped chains count for 1
#         -- (they were dropped because they were long)
#         sim.chainsDropped
#         sim.entities
