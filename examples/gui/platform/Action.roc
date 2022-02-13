interface Action
    exposes [ Action, update, translate ]
    imports []

Effect a : [ Effect a ]

effectDotMap : Effect a, (a -> b) -> Effect b

Action state : [ None, Always state, Fx (state -> Effect (state -> Action state)) ]

Task ok err : Effect (Result ok err)

## Convert a Task to an Action
toAction : Task ok err, (Result ok err -> (state -> Action state)) -> Action state
toAction = \effect, resultToCb ->
    fx : Effect (state -> Action state)
    fx = effectDotMap effect resultToCb

    Fx fx

translate : Action child, (parent -> child), (child -> parent) -> Action parent
translate = \action, toChild, toParent ->
    when action is
        None -> None
        Always child -> Always (toParent child)
        Fx childToEffect ->
            Fx \parent ->
                effect : child -> Action child
                effect = childToEffect (toChild parent)

                effectDotMap effect \childToActionChild ->
                    \finalParent ->
                        actionChild : Action child
                        actionChild = childToActionChild (toChild finalParent)

                        translate actionChild toChild toParent

update : state -> Action state
update = Always

# Val :
#     [
#         Str Str,
#         Bool Bool,
#         List (List Val),
#         Bytes (List U8), # listed separately bc it's so common
#         Pair Val Val,
#         U8 U8,
#         I8 I8,
#         # TODO …all the other number types go here
#     ]

# Effect a : [ Effect a ]

# Task ok err : [ Task U32 Val (Val -> Effect (Result ok err) ]

# await : Task a err, (a -> Task b err) -> Task b err
# await = \Task id arg retToEffect, aToTaskB ->
#     Task id arg \ret ->
#         result <- Effect.after (retToEffect ret)
#         when result ret is
#             Ok a ->
#                 Task idB argB retBToEffectB = aToTaskB a

#                 retB <- Effect.after (Effect.runRaw idB argB)
#                 retBToEffectB retB

#             Err err -> Effect.always (Err err)

# # defined in `effects`
# runRaw : U32, Val -> Effect Val

# Action state : [ None, Always state, Fx U32 Val (state, Val -> Action state) ]

# ## Convert a Task to an Action
# toAction : Task ok err, (state, Result ok err -> Action state) -> Action state
# toAction = \task, stateResultToAction ->
#     Task id arg retToResult = task

#     Fx id arg \state, ret ->
#         stateResultToAction state (retToResult ret)

# translate : Action child, (parent -> child), (child -> parent) -> Action parent
# translate = \action, toChild, toParent ->
#     when action is
#         None -> None
#         Always child -> Always (toParent child)
#         Fx id arg childRetToAction ->
#             Fx id arg \parent, ret ->
#                 childRetToAction (toChild parent) ret
