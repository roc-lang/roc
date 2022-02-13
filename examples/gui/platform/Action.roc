interface Action
    exposes [ Action, update, bimap ]
    imports []

Val :
    [
        Str Str,
        Bool Bool,
        List (List Val),
        Bytes (List U8), # listed separately bc it's so common
        Pair Val Val,
        U8 U8,
        I8 I8,
        # TODO …all the other number types go here
    ]


Task ok err : [ Task U32 Val (Val -> Result ok err) ]

Action state : [ Always state, Fx U32 Val (state, Val -> Action state) ]

## Convert a Task to an Action
toAction : Task ok err (state, Result ok err -> Action state) -> Action state
# toAction = \Task id arg retToResult, stateResultToAction ->
#     Fx id arg \state, ret ->
#         stateResultToAction state (retToResult ret)

update : state -> Action state
update = Always

bimap : Action child, (parent -> child), (child -> parent) -> Action parent
# bimap = \action, toChild, toParent ->
#     when action is
#         Always child -> toParent child
#         Fx id arg childRetToAction ->
#             Fx id arg \parent, ret ->
#                 childRetToAction (toChild parent) ret

# Action state : [ None, Always state, Chain (state -> Action state) ]

# none : Action *
# none = None

# map : Action a, (a -> b) -> Action b
# map = \action, transform ->
#     when action is
#         None -> None
#         Update state -> Update (transform state)


# await : Action a, (b -> a), (a -> b), (a -> Action b) -> Action b
# await = \action, toChild, toParent, transform ->
#     when action is
#         None -> None
#         Chain toChildAction -> Chain \toParentAction ->
#             toChild parent
#                 |> xform
#                 |> toParent

#             ...? # doesn't work I don't think


# after : Action a, (a -> Action a) -> Action a
# await = \action, toChild, toParent, transform ->


# toAction :
#     Task ok err,
#     (Result ok err -> Action state)
#     -> Action state
# toAction = \task, fromResult ->
#     action : Action { state, task : Task ok err }
#     action = Action.update \{ state, task } ->
#         { state & task }

#     Action.map action .state

# toAction :
#     Task ok err,
#     (Result ok err -> Action state)
#     -> Action state
# toAction = \task, fromResult ->



# toAction :
#     Task ok err,
#     (state, Result ok err -> Action state)
#     -> Action state
# toAction = \task, fromResult ->
#     action : Action { state, task : Task ok err }
#     action = Action.update \{ state, task } ->
#         { state & task }

#     Action.bimap action .state \child -> { state: child, task }

# Task ok err : [ @Task U32 Val (Val -> Result ok err) ]

# taskToCallback : Task ok err -> (Val -> Result ok err)
# taskToCallback = \@Task _ _ cb -> cb

# Eff state : [ Run U32 Val (Val, state -> Action state) ]

# Eff.bimap : Eff child, (parent -> child), (child -> parent) -> Eff parent
# Eff.bimap = \child, toChild, toParent ->
#     when child is
#         Eff u32 val cb -> Eff u32 val \parent ->
#             toChild parent
#                 |> cb
#                 |> Action.bimap toChild toParent

# Eff.after : Eff state, (Val, state -> Eff state) -> Eff state
# Eff.after = \eff ->
#     when eff is
#         Eff u32 val cb ->
#             Eff


# Eff state : [ Eff U32 Val (Val, state -> Action state) ]

# Action state : [ None, Update state, Many (List (state -> state)) ]

# Action state : [ None, Update state, Effect U32 Val (state, Val -> Action state) ]



# Action state : [ Always state, Effect U32 Val (state, Val -> Action state) ]

# after : Action child, (child -> Action parent) -> Action parent
# after = \action, callback ->
#     when action is
#         Always child -> callback child
#         Effect id arg cb ->
#             Effect ??? ??? \parent ->


# # What if state is a phantom type?
# Effect a : [ Effect U32 Val (Val -> a) ]

# getBytes : Str -> Effect (Result (List U8) Http.Err)
# getBytes = \url ->
#     Effect 1 (Str url) \resp ->
#         when resp is
#             # TODO translate status code etc
#             _ -> Ok []


# Action state : [ Always state, Fx U32 Val (state, Val -> Action state) ]

# toAction :
#     Effect a
#     (state, a -> Action state)
#     -> Action state
# toAction = \Effect u32 arg retToA, transform ->
#     Fx u32 arg \state, ret ->
#         transform state (retToA ret)





# toAction :
#     Effect a
#     (state, a -> state)
#     -> Action state
# toAction = \Effect u32 val cb, fromResult ->
#     action : Action { state, effect : Effect a }
#     action = Step \{ state, effect: Effect u32 val cb } ->
#         Fx u32 val \st8 ->
#             cb



#     Action.bimap action .state \child -> { state: child, effect }
