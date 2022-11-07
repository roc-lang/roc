interface Html.Internal
    exposes [
        App,
        Html,
        Attribute,
        CyclicStructureAccessor,
        Handler,
        element,
        text,
        # lazy, TODO
        none,
        translate,
        translateStatic,
        initServerApp,
        initClientApp,
        insertHandler,
        replaceHandler,
        dispatchEvent,
        appendRenderedStatic,
        nodeSize,
    ]
    imports [
        Action.{ Action },
        Effect.{ Effect },
        Encode,
        Json,
        Html.HostJavaScript.{ hostJavaScript },
    ]

PlatformState state initData : {
    app : App state initData,
    state,
    view : Html state,
    handlers : HandlerLookup state,
    isOddArena : Bool,
}

# TODO: keep a list of free indices that we push and pop like a stack
HandlerLookup state : List (Result (Handler state) [NoHandler])

App state initData : {
    init : initData -> state,
    render : state -> Html state,
    wasmUrl : Str,
}

Html state : [
    None,
    Text MaybeJsIndex Str,
    Element Str MaybeJsIndex Nat (List (Attribute state)) (List (Html state)),
    Lazy (Result { state, node : Html state } [NotCached] -> { state, node : Html state }),
]

MaybeJsIndex : [
    JsIndex Nat, # Index of the corresponding real DOM node in a JavaScript array
    NoJsIndex, # We don't have a JavaScript index for fresh nodes not yet diffed, or for static HTML
]

LazyCallback state : Result { state, node : Html state } [NotCached] -> { state, node : Html state }

Attribute state : [
    EventListener Str (List CyclicStructureAccessor) (Result (Handler state) Nat),
    HtmlAttr Str Str,
    DomProp Str (List U8),
    Style Str Str,
]

CyclicStructureAccessor : [
    ObjectField Str CyclicStructureAccessor,
    ArrayIndex Nat CyclicStructureAccessor,
    SerializableValue,
]

# If we are only exposing the functions then are we better off just turning everything into a Custom?
# At some point we need a common format anyway. Wrapper lambda is irrelevant for perf in context of an event.
Handler state := [
    Normal (state, List (List U8) -> Action state),
    Custom (state, List (List U8) -> { action : Action state, stopPropagation : Bool, preventDefault : Bool }),
]

# -------------------------------
#   VIEW FUNCTIONS
# -------------------------------
## Define an HTML Element
element : Str -> (List (Attribute state), List (Html state) -> Html state)
element = \tagName ->
    \attrs, children ->
        # While building the node tree, calculate the size of Str it will render to
        withTag = 2 * (3 + Str.countUtf8Bytes tagName)
        withAttrs = List.walk attrs withTag \acc, attr -> acc + attrSize attr
        totalSize = List.walk children withAttrs \acc, child -> acc + nodeSize child

        Element tagName NoJsIndex totalSize attrs children

text : Str -> Html state
text = \content -> Text NoJsIndex content

# TODO: causes stack overflow in compiler
# lazy : (Result { state, node : Html state } [NotCached] -> { state, node : Html state }) -> Html state
# lazy = \f -> Lazy f
none : Html state
none = None

nodeSize : Html state -> Nat
nodeSize = \node ->
    when node is
        Text _ content -> Str.countUtf8Bytes content
        Element _ _ size _ _ -> size
        Lazy _ -> 0 # Ignore Lazy for buffer size estimate. renderStatic might have to reallocate, but that's OK.
        None -> 0

attrSize : Attribute state -> Nat
attrSize = \attr ->
    when attr is
        EventListener _ _ _ -> 0
        HtmlAttr key value -> 4 + Str.countUtf8Bytes key + Str.countUtf8Bytes value
        DomProp _ _ -> 0
        Style key value -> 4 + Str.countUtf8Bytes key + Str.countUtf8Bytes value

# -------------------------------
#   STATIC HTML
# -------------------------------
appendRenderedStatic : Str, Html [] -> Str
appendRenderedStatic = \buffer, node ->
    when node is
        Text _ content ->
            Str.concat buffer content

        Element name _ _ attrs children ->
            withTagName = "\(buffer)<\(name)"
            withAttrs =
                if List.isEmpty attrs then
                    withTagName
                else
                    init = { buffer: Str.concat withTagName " ", styles: "" }
                    { buffer: attrBuffer, styles } =
                        List.walk attrs init appendRenderedStaticAttr

                    if Str.isEmpty styles then
                        attrBuffer
                    else
                        "\(attrBuffer) style=\"\(styles)\""

            withTag = Str.concat withAttrs ">"
            withChildren = List.walk children withTag appendRenderedStatic

            "\(withChildren)</\(name)>"

        # Lazy can only be constructed in virtual DOM, not static
        None -> buffer

appendRenderedStaticAttr : { buffer : Str, styles : Str }, Attribute [] -> { buffer : Str, styles : Str }
appendRenderedStaticAttr = \{ buffer, styles }, attr ->
    when attr is
        HtmlAttr key value ->
            newBuffer = "\(buffer) \(key)=\"\(value)\""

            { buffer: newBuffer, styles }

        Style key value ->
            newStyles = "\(styles) \(key): \(value);"

            { buffer, styles: newStyles }

        # The remaining variants only make sense on the front end. Ignore for server-side rendering.
        EventListener _ _ _ -> { buffer, styles }
        DomProp _ _ -> { buffer, styles }

# -------------------------------
#   TRANSLATE STATE TYPE
# -------------------------------
# translate : Html c, (p -> c), (c -> p) -> Html p # TODO: use this type signature when it no longer triggers a type checker bug
translate : Html _, (_ -> _), (_ -> _) -> Html _
translate = \node, parentToChild, childToParent ->
    when node is
        Text jsIndex content ->
            Text jsIndex content

        Element name jsIndex size attrs children ->
            newAttrs = List.map attrs \a -> translateAttr a parentToChild childToParent
            newChildren = List.map children \c -> translate c parentToChild childToParent

            Element name jsIndex size newAttrs newChildren

        Lazy childCallback ->
            Lazy (translateLazy childCallback parentToChild childToParent)

        None -> None

# translateLazy : LazyCallback c, (p -> c), (c -> p) -> LazyCallback p # TODO: use this type signature when it no longer triggers a type checker bug
translateLazy : LazyCallback _, (_ -> _), (_ -> _) -> LazyCallback _
translateLazy = \childCallback, parentToChild, childToParent ->
    \parentCacheValue ->
        childCacheValue =
            parentCacheValue
            |> Result.map \v -> { state: parentToChild v.state, node: translate v.node childToParent parentToChild }
        { node, state } = childCallback childCacheValue

        {
            node: translate node parentToChild childToParent,
            state: childToParent state,
        }

# translateAttr : Attribute c, (p -> c), (c -> p) -> Attribute p
translateAttr : Attribute _, (_ -> _), (_ -> _) -> Attribute p
translateAttr = \attr, parentToChild, childToParent ->
    when attr is
        EventListener eventName accessors (Ok childHandler) ->
            EventListener eventName accessors (Ok (translateHandler childHandler parentToChild childToParent))

        EventListener eventName accessors (Err handlerId) ->
            EventListener eventName accessors (Err handlerId)

        HtmlAttr k v -> HtmlAttr k v
        DomProp k v -> DomProp k v
        Style k v -> Style k v

# translateHandler : Handler c, (p -> c), (c -> p) -> Handler p
translateHandler : Handler _, (_ -> _), (_ -> _) -> Handler _
translateHandler = \childHandler, parentToChild, childToParent ->
    when childHandler is
        @Handler (Normal childFn) ->
            parentFn = \parentState, jsons ->
                parentState |> parentToChild |> childFn jsons |> Action.map childToParent

            @Handler (Normal parentFn)

        @Handler (Custom childFn) ->
            parentFn = \parentState, jsons ->
                { action, stopPropagation, preventDefault } = childFn (parentToChild parentState) jsons

                { action: action |> Action.map childToParent, stopPropagation, preventDefault }

            @Handler (Custom parentFn)

translateStatic : Html state -> Html *
translateStatic = \node ->
    when node is
        Text jsIndex content ->
            Text jsIndex content

        Element name jsIndex size attrs children ->
            newAttrs = List.keepOks attrs keepStaticAttr
            newChildren = List.map children translateStatic

            Element name jsIndex size newAttrs newChildren

        # TODO: Triggers a stack overflow in the compiler. I think in type checking.
        # That's a pity because if someone used Lazy, it's probably worth server-side rendering.
        # Lazy callback ->
        #     { node: dynamicNode } = callback (Err NotCached)
        #     translateStatic dynamicNode
        Lazy _ -> None
        None -> None

keepStaticAttr : Attribute _ -> Result (Attribute *) {}
keepStaticAttr = \attr ->
    when attr is
        EventListener _ _ _ -> Err {}
        HtmlAttr k v -> Ok (HtmlAttr k v)
        DomProp _ _ -> Err {}
        Style k v -> Ok (Style k v)

# -------------------------------
#   EVENT HANDLING
# -------------------------------
JsEventResult state initData : {
    platformState : Box (PlatformState state initData),
    stopPropagation : Bool,
    preventDefault : Bool,
}

## Dispatch a JavaScript event to a Roc handler, given the handler ID and some JSON event data.
## We use Box to pass data structures on the Wasm heap. This is a lot easier than trying to access Wasm stack from JS.
## DANGER: this function does unusual stuff with memory allocation lifetimes. Be as careful as you would with Zig or C code!
dispatchEvent : Box (PlatformState state initData), Box (List (List U8)), Nat -> Effect (Box (JsEventResult state initData))
dispatchEvent = \boxedPlatformState, boxedEventData, handlerId ->
    { app, state, view, handlers, isOddArena: wasOddArena } =
        Box.unbox boxedPlatformState
    eventData =
        Box.unbox boxedEventData
    maybeHandler =
        List.get handlers handlerId
        |> Result.withDefault (Err NoHandler)
    { action, stopPropagation, preventDefault } =
        when maybeHandler is
            Err NoHandler ->
                { action: Action.none, stopPropagation: Bool.false, preventDefault: Bool.false }

            Ok (@Handler (Normal handler)) ->
                { action: handler state eventData, stopPropagation: Bool.false, preventDefault: Bool.false }

            Ok (@Handler (Custom handler)) ->
                handler state eventData

    when action is
        Update newState ->
            # Any values created in the arena will all be freed on the next update
            isOddArena = !wasOddArena

            Effect.runInVdomArena isOddArena \_ ->
                newViewUnindexed = app.render newState
                emptyHandlers = List.repeat (Err NoHandler) (List.len handlers)

                { newHandlers, node: newViewIndexed } <- diffAndUpdateDom emptyHandlers view newViewUnindexed |> Effect.after
                newBoxedPlatformState = Box.box {
                    app,
                    state: newState,
                    view: newViewIndexed,
                    handlers: newHandlers,
                    isOddArena,
                }

                Effect.always (Box.box { platformState: newBoxedPlatformState, stopPropagation, preventDefault })

        # TODO: Roc compiler tells me I need a `_` pattern but I think I should just need `None`
        _ ->
            Effect.always (Box.box { platformState: boxedPlatformState, stopPropagation, preventDefault })

insertHandler : List (Result (Handler state) [NoHandler]), Handler state -> { index : Nat, handlers : List (Result (Handler state) [NoHandler]) }
insertHandler = \handlers, newHandler ->
    when List.findFirstIndex handlers Result.isErr is
        Ok index ->
            {
                index,
                handlers: List.set handlers index (Ok newHandler),
            }

        Err NotFound ->
            {
                index: List.len handlers,
                handlers: List.append handlers (Ok newHandler),
            }

replaceHandler : List (Result (Handler state) [NoHandler]), Nat, Handler state -> List (Result (Handler state) [NoHandler])
replaceHandler = \handlers, index, newHandler ->
    { list } = List.replace handlers index (Ok newHandler)

    list

# -------------------------------
#   SERVER SIDE INIT
# -------------------------------
initServerApp : initData, App state initData -> Result (Html []) [InvalidDocument] | initData has Encoding
initServerApp = \initData, app ->
    initData
    |> app.init
    |> app.render
    |> translateStatic
    |> insertRocScript initData app.wasmUrl

insertRocScript : Html [], initData, Str -> Result (Html []) [InvalidDocument] | initData has Encoding
insertRocScript = \document, initData, wasmUrl ->
    # Convert initData to JSON as a Roc Str, then convert the Roc Str to a JS string.
    # JSON won't have invalid UTF-8 in it, since it would be escaped as part of JSON encoding.
    jsInitData =
        initData
        |> Encode.toBytes Json.toUtf8
        |> Str.fromUtf8
        |> Encode.toBytes Json.toUtf8
        |> Str.fromUtf8
        |> Result.withDefault ""
    jsWasmUrl =
        wasmUrl
        |> Encode.toBytes Json.toUtf8
        |> Str.fromUtf8
        |> Result.withDefault ""

    script : Html []
    script = (element "script") [] [
        text
            """
            (function(){
            \(hostJavaScript)
            const initData = \(jsInitData);
            const wasmUrl = \(jsWasmUrl);
            window.roc = roc_init(initData, wasmUrl);
            })();
            """,
    ]

    # append the <script> to the end of the <body>
    when document is
        Element "html" hIndex hSize hAttrs hChildren ->
            empty = List.withCapacity (List.len hChildren)
            walkResult =
                List.walk hChildren { newHtmlChildren: empty, foundBody: Bool.false } \{ newHtmlChildren, foundBody }, hChild ->
                    when hChild is
                        Element "body" bIndex bSize bAttrs bChildren ->
                            {
                                newHtmlChildren: List.append newHtmlChildren (Element "body" bIndex bSize bAttrs (List.append bChildren script)),
                                foundBody: Bool.true,
                            }

                        _ ->
                            {
                                newHtmlChildren: List.append newHtmlChildren hChild,
                                foundBody,
                            }

            if walkResult.foundBody then
                Ok (Element "html" hIndex hSize hAttrs walkResult.newHtmlChildren)
            else
                Err InvalidDocument

        _ -> Err InvalidDocument

# -------------------------------
#   CLIENT SIDE INIT
# -------------------------------
ClientInit state : {
    state,
    staticView : Html state,
    dynamicView : Html state,
}

initClientApp : List U8, App state initData -> Result (ClientInit state) [JsonError] | initData has Decoding
initClientApp = \json, app ->
    initData <-
        json
        |> Decode.fromBytes Json.fromUtf8
        |> Result.mapErr (\_ -> JsonError)
        |> Result.try
    state = app.init initData
    dynamicView = app.render state
    staticUnindexed = translateStatic dynamicView
    staticView =
        indexNodes { list: [], index: 0 } staticUnindexed
        |> .list
        |> List.first
        |> Result.withDefault (Text NoJsIndex "The impossible happened in virtual-dom. Couldn't get the first item in a single-element list.")

    Ok {
        state,
        staticView,
        dynamicView,
    }

# Assign an index to each (virtual) DOM node.
# In JavaScript, we maintain an array of references to real DOM nodes.
# In Roc, each virtual DOM node in the "old" tree knows the index of its real DOM node in the JS array.
# Here we traverse the tree in the same order as JavaScript does when it initialises the array.
indexNodes : { list : List (Html state), index : Nat }, Html state -> { list : List (Html state), index : Nat }
indexNodes = \{ list, index }, node ->
    when node is
        Text jsIndex content ->
            { nodeIndex, nextIndex } =
                when jsIndex is
                    JsIndex id -> { nodeIndex: JsIndex id, nextIndex: index }
                    NoJsIndex -> { nodeIndex: JsIndex index, nextIndex: index + 1 }

            {
                list: List.append list (Text nodeIndex content),
                index: nextIndex,
            }

        Element name jsIndex size attrs children ->
            { list: newChildren, index: afterChildren } =
                List.walk children { list, index } indexNodes
            { nodeIndex, nextIndex } =
                when jsIndex is
                    JsIndex id -> { nodeIndex: JsIndex id, nextIndex: afterChildren }
                    NoJsIndex -> { nodeIndex: JsIndex afterChildren, nextIndex: afterChildren + 1 }

            {
                list: List.append list (Element name nodeIndex size attrs newChildren),
                index: nextIndex,
            }

        _ ->
            {
                list: List.append list node,
                index,
            }

# -------------------------------
#   VIRTUAL DOM DIFF
# -------------------------------
diffAndUpdateDom : HandlerLookup state, Html state, Html state -> Effect { newHandlers : HandlerLookup state, node : Html state }
diffAndUpdateDom = \newHandlers, oldNode, newNode ->
    todo = Effect.always { newHandlers, node: newNode }

    when { oldNode, newNode } is
        { oldNode: Text (JsIndex index) oldContent, newNode: Text NoJsIndex newContent } ->
            retVal = { newHandlers, node: Text (JsIndex index) newContent }

            if newContent == oldContent then
                Effect.always retVal
            else
                Effect.updateTextNode index newContent
                |> Effect.map \_ -> retVal

        { oldNode: Text _ _, newNode: Text _ _ } ->
            Effect.always { newHandlers, node: newNode }

        { oldNode: Element oldName (JsIndex index) oldSize oldAttrs oldChildren, newNode: Element newName NoJsIndex newSize newAttrs newChildren } ->
            if newName == oldName then
                # iterate over the children and attrs
                todo
            else
                # create a new subtree from the bottom up
                # swap it
                todo

        { oldNode: Element _ _ _ _ _, newNode: Element _ _ _ _ _ } ->
            todo #  TODO: debug message for framework dev

        { oldNode: Lazy oldCallback, newNode: Lazy newCallback } ->
            todo

        { oldNode: None, newNode: None } ->
            todo

        _ ->
            # Just replace
            todo

createSubTree : HandlerLookup state, Html state -> Effect { newHandlers : HandlerLookup state, node : Html state }
createSubTree = \newHandlers, node ->
    when node is
        Text _ content ->
            Effect.createTextNode content
            |> Effect.map \index ->
                jsIndex : MaybeJsIndex
                jsIndex = JsIndex index

                { newHandlers, node: Text jsIndex content }

        Element name _ size attrs children ->
            nodeIndex <- Effect.createElement name |> Effect.after
            { style, newHandlers: updatedHandlers, effects } =
                List.walk attrs { nodeIndex, style: "", newHandlers, renderedAttrs: [], effects: Effect.always {} } addAttribute

            _ <- effects |> Effect.after
            _ <- (if style != "" then Effect.setAttribute nodeIndex "style" style else Effect.always {}) |> Effect.after
            jsIndex : MaybeJsIndex
            jsIndex = JsIndex nodeIndex

            #   for each child
            #       recurse into it
            #       then append it
            Effect.always {
                newHandlers: updatedHandlers,
                node: Element name jsIndex size attrs children,
            }

        Lazy callback ->
            createSubTree newHandlers (callback (Err NotCached)).node

        None -> Effect.always { newHandlers, node: None }

AddAttrWalk state : {
    nodeIndex : Nat,
    style : Str,
    newHandlers : HandlerLookup state,
    renderedAttrs : List (Attribute state),
    effects : Effect {},
}

# addAttribute : AddAttrWalk state, Attribute state -> AddAttrWalk state
addAttribute : AddAttrWalk _, Attribute _ -> AddAttrWalk _
addAttribute = \{ nodeIndex, style, newHandlers, renderedAttrs, effects }, attr ->
    when attr is
        EventListener name accessors (Ok handler) ->
            { updatedHandlers, handlerIndex } =
                # TODO: speed this up using a free list
                when List.findFirstIndex newHandlers Result.isErr is
                    Err NotFound ->
                        { updatedHandlers: [], #List.append newHandlers (Ok handler),
                          handlerIndex: List.len newHandlers,
                        }
                    Ok freeIndex ->
                        { updatedHandlers: [], #List.set newHandlers freeIndex (Ok handler),
                          handlerIndex: freeIndex,
                        }
            # Store the handlerIndex in the rendered virtual DOM tree, since we'll need it for the next diff
            renderedAttr = EventListener name accessors (Err handlerIndex)

            { nodeIndex, style, newHandlers: updatedHandlers, renderedAttrs: List.append renderedAttrs renderedAttr, effects }

        EventListener name accessors (Err handlerIndex) ->
            # This pattern should never be reached!
            { nodeIndex, style, newHandlers, renderedAttrs: List.append renderedAttrs (EventListener name accessors (Err handlerIndex)), effects }

        HtmlAttr k v ->
            { nodeIndex, style, newHandlers, renderedAttrs: List.append renderedAttrs (HtmlAttr k v), effects: Effect.after effects (\_ -> Effect.setAttribute nodeIndex k v) }

        DomProp k v ->
            { nodeIndex, style, newHandlers, renderedAttrs: List.append renderedAttrs (DomProp k v), effects: Effect.after effects (\_ -> Effect.setProperty nodeIndex k v) }

        Style k v ->
            newStyle = "\(style) \(k):\(v);"

            { nodeIndex, style: newStyle, newHandlers, renderedAttrs: List.append renderedAttrs (Style k v), effects }

# a -> Effect [Step a, Done b]
# Effect.loop : a, (a -> Effect [Step a, Done b]) -> Effect b
# text
#   create it
# element
#   create it
#   for each attr
#       insert it
#   for each child
#       recurse into it
#       then append it
# lazy
#   expand it with the current state
#   recurse
# none
#   no effects, just return it
# Element name jsIndex size attrs children ->
#     newAttrs = List.map attrs \a -> translateAttr a parentToChild childToParent
#     newChildren = List.map children \c -> translate c parentToChild childToParent
#     Element name jsIndex size newAttrs newChildren
# Lazy childCallback ->
#     Lazy (translateLazy childCallback parentToChild childToParent)
# None -> None
