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
toAction : Task ok err, (state, Result ok err -> Action state) -> Action state
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
