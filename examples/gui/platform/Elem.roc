interface Elem
    exposes [ Elem, PressEvent, row, col, text, button, none, translate, list ]
    imports [ Action.{ Action }]

Elem state :
    [
        Button (ButtonConfig state) (Elem state),
        Text Str,
        Col (List (Elem state)),
        Row (List (Elem state)),
        Lazy (state -> Elem state),
        Cached (Result { state, elem : Elem state } [ NotCached ] -> { state, elem : Elem state }),
        # TODO FIXME: using this definition of Lazy causes a stack overflow in the compiler!
        #Lazy (Result (Cached state) [ NotCached ] -> Cached state),
        None,
    ]

# traverse : Elem state ->
# when elem is
#     Button (ButtonConfig state) (Elem state),
#     Text Str,
#     Col (List (Elem state)),
#     Row (List (Elem state)),
#     Lazy (state -> Elem state),
#     Cached (Result { state, elem : Elem state } [ NotCached ] -> { state, elem : Elem state }),
#     # TODO FIXME: using this definition of Lazy causes a stack overflow in the compiler!
#     #Lazy (Result (Cached state) [ NotCached ] -> Cached state),
#     None,
# ]

# TODO this can be an Ability so we don't have to pass a big record around
# this is how to define a way to traverse the opaque Elem type.
#
# ElemWalker state accum : {
#     ifButton : (accum, ButtonConfig state, Elem state -> accum),
#     ifText : (accum, Str -> accum),
#     ifCol : (accum, List (Elem state) -> accum),
#     ifRow : (accum, List (Elem state) -> accum),
#     ifLazy (accum, (state -> Elem state) -> accum),
#     ifCached accum, (Result { state, elem : Elem state } [ NotCached ] -> { state, elem : Elem state }) -> accum),
#     # NOTE: if it's None, `step` automatically returns Err ElemWasNone; no handler needed for that one!
# }
#
#
# step : Elem state, accum, ElemWalker state accum -> ElemWalkerOutput state accum
#
# root = render state
#
# Example usage: Walk the Elem tree to build up a layout (or maybe just layout constraints at first).
# Note that the layout functions might also call `step` (e.g. Row, Col, and Button would all recurse on child Elems)
# step root Layout.empty {
#     ifText: Layout.addText
#     ifButton: Layout.addButton
#     ifRow: Layout.addRow
#     ifLazy: \layout, renderChild -> Layout.addElem layout (renderChild state) # maybe just ifLazy: Layout.addLazy?
# }

## Used internally in the type definition of Lazy
Cached state : { state, elem: Elem state }

ButtonConfig state : { onPress : state, PressEvent -> Action state }

PressEvent : { button : [ Touch, Mouse [ Left, Right, Middle ] ] }

text : Str -> Elem *
text = \str ->
    Text str

button : { onPress : state, PressEvent -> Action state }, Elem state -> Elem state
button = \config, label ->
    Button config label

row : List (Elem state) -> Elem state
row = \children ->
    Row children

col : List (Elem state) -> Elem state
col = \children ->
    Col children

cached : state, (state -> Elem state) -> Elem state
cached = \state, render ->
    # This function gets called by the host during rendering. It will
    # receive the cached state and element (wrapped in Ok) if we've
    # ever rendered this before, and Err otherwise.
    Cached \result ->
        when result is
            Ok cached if cached.state == state ->
                # If we have a cached value, and the new state is the
                # same as the cached one, then we can return exactly
                # what we had cached.
                cached

            _ ->
                # Either the state changed or else we didn't have a
                # cached value to use. Either way, we need to render
                # with the new state and store that for future use.
                { state, elem: render state }

none : Elem *
none = None # I've often wanted this in elm/html. Usually end up resorting to (Html.text "") - this seems nicer.

## Change an element's state type.
##
##     State : { photo : Photo }
##
##     render : State -> Elem State
##     render = \state ->
##         child : Elem State
##         child =
##             Photo.render state.photo
##                 |> Elem.translate .photo &photo
##
##         col {} [ child, otherElems ]
translate :
    (child -> Elem child),
    (parent -> child),
    (parent, child -> parent)
    -> Elem parent
translate = \renderChild, toChild, toParent ->
    Lazy \parent ->
        when renderChild (toChild parent) is
            Text str -> Text str
            Col elems -> Col (List.map elems \elem -> translate elem toChild toParent)
            Row elems -> Row (List.map elems \elem -> translate elem toChild toParent)
            Button config label ->
                onPress = \prevParent, event ->
                    toChild prevParent
                        |> config.onPress event
                        |> Action.translate toChild (\c -> toParent prevParent c)

                Button { onPress } (translate label toChild toParent)
            Cached renderChild ->
                Cached \prevParent ->
                    { elem, state } = renderChild (toChild prevParent)

                    {
                        elem: translate renderChild toChild toParent,
                        state: toParent prevParent state
                    }

            None -> None


## Render a list of elements, using [Elem.translate] on each of them.
##
## Convenient when you have a [List] in your state and want to make
## a [List] of child elements out of it.
##
##     State : { photos : List Photo }
##
##     render : State -> Elem State
##     render = \state ->
##         children : List (Elem State)
##         children =
##             Elem.list Photo.render state .photos &photos
##
##         col {} children
list :
    (child -> Elem child),
    parent,
    (parent -> List child),
    (parent, List child -> parent)
    -> List (Elem parent)
list = \renderChild, parent, toChildren, toParent ->
    List.mapWithIndex (toChildren parent) \index, child ->
        toChild = \par -> List.get (toChildren par) index

        newChild = translateOrDrop child toChild \par, ch ->
            toChildren par
                |> List.set ch index
                |> toParent

        renderChild newChild

## Internal helper function for Elem.list
##
## Tries to translate a child to a parent, but
## if the child has been removed from the parent,
## drops it.
translateOrDrop :
    Elem child,
    (parent -> Result child *),
    (parent, child -> parent)
    -> Elem parent
translateOrDrop = \child, toChild, toParent ->
    when child is
        Text str -> Text str
        Col elems -> Col (List.map elems \elem -> translateOrDrop elem toChild toParent)
        Row elems -> Row (List.map elems \elem -> translateOrDrop elem toChild toParent)
        Button config label ->
            onPress = \parentState, event ->
                when toChild parentState is
                    Ok newChild ->
                        newChild
                            |> config.onPress event
                            |> Action.translate toChild (\c -> toParent parentState c)

                    Err _ ->
                        # The child was removed from the list before this onPress handler resolved.
                        # (For example, by a previous event handler that fired simultaneously.)
                        Action.none

            Button { onPress } (translateOrDrop label toChild toParent)
        Cached childState renderChild ->
            Cached (toParent childState) \parentState ->
                when toChild parentState is
                    Ok newChild ->
                        renderChild newChild
                            |> translateOrDrop toChild toParent

                    Err _ -> None # I don't think this should ever happen in practice.
        None -> None
