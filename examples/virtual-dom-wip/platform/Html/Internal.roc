interface Html.Internal
    exposes [
        App,
        Html,
        Attribute,
        CyclicStructureAccessor,
        Handler,
        element,
        text,
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
    ]

PlatformState state initData : {
    app : App state initData,
    state,
    view : RenderedHtml,
    handlerLookup : HandlerLookup state,
    isOddArena : Bool,
}

HandlerLookup state : {
    handlers : List (Result (Handler state) [NoHandler]),
    freeList : List Nat,
}

App state initData : {
    init : DecodingResult initData -> state,
    render : state -> Html state,
    wasmUrl : Str,
}

DecodingResult a : Result a [Leftover (List U8), TooShort]

Html state : [
    None,
    Text Str,
    Element Str Size (List (Attribute state)) (List (Html state)),
]

RenderedHtml : [
    RenderedNone,
    RenderedText JsIndex Str,
    RenderedElement JsIndex Str Size (List RenderedAttribute) (List RenderedHtml),
]

JsIndex : Nat # Index of the corresponding real DOM node in the JavaScript `nodes` array
Size : Nat

Attribute state : [
    EventListener Str (List CyclicStructureAccessor) (Handler state),
    HtmlAttr Str Str,
    DomProp Str (List U8),
    Style Str Str,
]

RenderedAttribute : [
    RenderedEventListener Str (List CyclicStructureAccessor) HandlerId,
    RenderedHtmlAttr Str Str,
    RenderedDomProp Str (List U8),
    RenderedStyle Str Str,
]

HandlerId : Nat

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

        Element tagName totalSize attrs children

text : Str -> Html state
text = \content -> Text content

none : Html state
none = None

nodeSize : Html state -> Nat
nodeSize = \node ->
    when node is
        Text content -> Str.countUtf8Bytes content
        Element _ size _ _ -> size
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
        Text content ->
            Str.concat buffer content

        Element name _ attrs children ->
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

        DomProp _ _ -> { buffer, styles }

# -------------------------------
#   TRANSLATE STATE TYPE
# -------------------------------
translate : Html c, (p -> c), (c -> p) -> Html p
translate = \node, parentToChild, childToParent ->
    when node is
        Text content ->
            Text content

        Element name size attrs children ->
            newAttrs = List.map attrs \a -> translateAttr a parentToChild childToParent
            newChildren = List.map children \c -> translate c parentToChild childToParent

            Element name size newAttrs newChildren

        None -> None

translateAttr : Attribute c, (p -> c), (c -> p) -> Attribute p
translateAttr = \attr, parentToChild, childToParent ->
    when attr is
        EventListener eventName accessors childHandler ->
            EventListener eventName accessors (translateHandler childHandler parentToChild childToParent)

        HtmlAttr k v -> HtmlAttr k v
        DomProp k v -> DomProp k v
        Style k v -> Style k v

translateHandler : Handler c, (p -> c), (c -> p) -> Handler p
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
        Text content ->
            Text content

        Element name size attrs children ->
            newAttrs = List.keepOks attrs keepStaticAttr
            newChildren = List.map children translateStatic

            Element name size newAttrs newChildren

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
    platformState : PlatformState state initData,
    stopPropagation : Bool,
    preventDefault : Bool,
}

## Dispatch a JavaScript event to a Roc handler, given the handler ID and some JSON event data.
## DANGER: this function does unusual stuff with memory allocation lifetimes. Be as careful as you would with Zig or C code!
dispatchEvent : PlatformState state initData, List (List U8), Nat -> Effect (JsEventResult state initData)
dispatchEvent = \platformState, eventData, handlerId ->
    { app, state, view, handlerLookup, isOddArena: wasOddArena } = platformState
    maybeHandler =
        List.get handlerLookup.handlers handlerId
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

            runInVdomArena isOddArena \_ ->
                newViewUnrendered = app.render newState
                numHandlers = List.len handlerLookup.handlers
                emptyHandlerLookup = {
                    handlers: List.repeat (Err NoHandler) numHandlers,
                    freeList: List.range { start: At (numHandlers - 1), end: At 0 },
                }

                { newHandlers, node: newViewRendered } <-
                    diffAndUpdateDom emptyHandlerLookup view newViewUnrendered |> Effect.after
                newPlatformState = {
                    app,
                    state: newState,
                    view: newViewRendered,
                    handlerLookup: newHandlers,
                    isOddArena,
                }

                Effect.always ({ platformState: newPlatformState, stopPropagation, preventDefault })

        None ->
            Effect.always { platformState, stopPropagation, preventDefault }

runInVdomArena : Bool, ({} -> Effect a) -> Effect a
runInVdomArena = \useOddArena, run ->
    _ <- Effect.enableVdomAllocator useOddArena |> Effect.after
    returnVal <- run {} |> Effect.after
    _ <- Effect.disableVdomAllocator |> Effect.after
    Effect.always returnVal

insertHandler : HandlerLookup state, Handler state -> { index : Nat, handlerLookup : HandlerLookup state }
insertHandler = \{ handlers, freeList }, newHandler ->
    when List.last freeList is
        Ok index ->
            {
                index,
                handlerLookup: {
                    handlers: List.set handlers index (Ok newHandler),
                    freeList: List.dropLast freeList,
                },
            }

        Err _ ->
            {
                index: List.len handlers,
                handlerLookup: {
                    handlers: List.append handlers (Ok newHandler),
                    freeList: freeList,
                },
            }

replaceHandler : HandlerLookup state, Nat, Handler state -> HandlerLookup state
replaceHandler = \{ handlers, freeList }, index, newHandler ->
    { list } = List.replace handlers index (Ok newHandler)

    {
        handlers: list,
        freeList,
    }

# -------------------------------
#   SERVER SIDE INIT
# -------------------------------
initServerApp : App state initData, initData, Str -> Result (Html []) [InvalidDocument] | initData has Encoding
initServerApp = \app, initData, hostJavaScript ->
    initData
    |> Ok
    |> app.init
    |> app.render
    |> translateStatic
    |> insertRocScript initData app.wasmUrl hostJavaScript

insertRocScript : Html [], initData, Str, Str -> Result (Html []) [InvalidDocument] | initData has Encoding
insertRocScript = \document, initData, wasmUrl, hostJavaScript ->
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
            \(hostJavaScript)
            (function(){
            const initData = \(jsInitData);
            const wasmUrl = \(jsWasmUrl);
            window.roc = roc_init(initData, wasmUrl);
            })();
            """,
    ]

    # append the <script> to the end of the <body>
    when document is
        Element "html" hSize hAttrs hChildren ->
            empty = List.withCapacity (List.len hChildren)
            walkResult =
                List.walk hChildren { newHtmlChildren: empty, foundBody: Bool.false } \{ newHtmlChildren, foundBody }, hChild ->
                    when hChild is
                        Element "body" bSize bAttrs bChildren ->
                            {
                                newHtmlChildren: List.append newHtmlChildren (Element "body" bSize bAttrs (List.append bChildren script)),
                                foundBody: Bool.true,
                            }

                        _ ->
                            {
                                newHtmlChildren: List.append newHtmlChildren hChild,
                                foundBody,
                            }

            if walkResult.foundBody then
                Ok (Element "html" hSize hAttrs walkResult.newHtmlChildren)
            else
                Err InvalidDocument

        _ -> Err InvalidDocument

# -------------------------------
#   CLIENT SIDE INIT
# -------------------------------
ClientInit state : {
    state,
    staticView : RenderedHtml,
    dynamicView : Html state,
}

initClientApp : List U8, App state initData -> PlatformState state initData | initData has Decoding
initClientApp = \json, app ->
    state =
        json
        |> Decode.fromBytes Json.fromUtf8
        |> app.init
    dynamicView = app.render state
    staticUnindexed = translateStatic dynamicView
    staticView =
        indexNodes { list: [], index: 0 } staticUnindexed
        |> .list
        |> List.first
        |> Result.withDefault (RenderedText 0 "The impossible happened in virtual-dom. Couldn't get the first item in a single-element list.")

    emptyHandlers = {
        handlers: [],
        freeList: [],
    }

    # Run the first diff. The only differences are event listeners, so they will be attached.
    { newHandlers: handlerLookup, node: view } <-
        diffAndUpdateDom handlerLookup staticView dynamicView |> Effect.after

    Effect.always {
        app,
        state,
        view,
        handlerLookup,
        isOddArena: Bool.false,
    }

# Assign an index to each (virtual) DOM node.
# In JavaScript, we maintain an array of references to real DOM nodes.
# In Roc, each virtual DOM node in the "old" tree knows the index of its real DOM node in the JS array.
# Here we traverse the tree in the same order as JavaScript does when it initialises the array.
indexNodes : { list : List RenderedHtml, index : Nat }, Html state -> { list : List RenderedHtml, index : Nat }
indexNodes = \{ list, index }, unrendered ->
    when unrendered is
        Text content ->
            {
                list: List.append list (RenderedText index content),
                index: index + 1,
            }

        Element name size attrs children ->
            { list: renderedChildren, index: nodeIndex } =
                List.walk children { list, index } indexNodes
            renderedAttrs =
                List.walk attrs [] \walkedAttrs, attr ->
                    when attr is
                        EventListener _ _ _ -> walkedAttrs # Dropped! Server-rendered HTML has no listeners
                        HtmlAttr k v -> List.append walkedAttrs (RenderedHtmlAttr k v)
                        DomProp k v -> List.append walkedAttrs (RenderedDomProp k v)
                        Style k v -> List.append walkedAttrs (RenderedStyle k v)

            {
                list: List.append list (RenderedElement nodeIndex name size renderedAttrs renderedChildren),
                index: nodeIndex + 1,
            }

        None ->
            {
                list: List.append list RenderedNone,
                index,
            }

# TODO:
# Emit patches first, then interpret those into Effects.
# It'll be way more debuggable and the code will probably be simpler.
# I can write tests more easily, and run more of it outside the browser.
# Fits with upcoming plans for how Effects will work anyway?
# First step is to allocate a NodeId without calling out to JS. Wasm needs to drive it.
diffAndUpdateDom : HandlerLookup state, RenderedHtml, Html state -> Effect { newHandlers : HandlerLookup state, node : RenderedHtml }
diffAndUpdateDom = \newHandlers, oldNode, newNode ->
    when { oldNode, newNode } is
        { oldNode: RenderedText index oldContent, newNode: Text newContent } ->
            # Always return the newContent even when equal. This will enable some memory allocation tricks later.
            retVal = { newHandlers, node: RenderedText index newContent }

            if newContent == oldContent then
                Effect.always retVal
            else
                Effect.updateTextNode index newContent
                |> Effect.map \_ -> retVal

        { oldNode: RenderedElement _index _oldName _oldSize _oldAttrs _oldChildren, newNode: Element _newName _newSize _newAttrs _newChildren } ->
            # TODO: actual diffing! LOL This is just to get something up and running
            renderFromScratch newHandlers newNode

        { oldNode: RenderedNone, newNode: None } ->
            Effect.always { newHandlers, node: RenderedNone }

        _ ->
            # old node has been replaced with a totally different variant. There's no point in diffing, just replace.
            renderFromScratch newHandlers newNode

renderFromScratch : HandlerLookup state, Html state -> Effect { newHandlers : HandlerLookup state, node : RenderedHtml }
renderFromScratch = \newHandlers, newNode ->
    { newHandlers: subTreeHandlers, renderedNodes: renderedNewNodeSingleton } <-
        createSubTree (Effect.always { newHandlers, renderedNodes: [] }) newNode |> Effect.after
    renderedNewNode =
        renderedNewNodeSingleton
        |> List.first
        |> Result.withDefault (RenderedText 0 "The impossible happened. createSubTree returned a non-singleton list")

    Effect.always { newHandlers: subTreeHandlers, node: renderedNewNode }

createSubTree :Effect { newHandlers : HandlerLookup state, renderedNodes : List RenderedHtml },
    Html state
    -> Effect { newHandlers : HandlerLookup state, renderedNodes : List RenderedHtml }
createSubTree = \previousEffects, node ->
    { newHandlers, renderedNodes } <- previousEffects |> Effect.after
    when node is
        Element name size attrs children ->
            nodeIndex <- Effect.createElement name |> Effect.after
            { style, newHandlers: newHandlersAttrs, renderedAttrs, effects: attrEffects } =
                List.walk attrs { nodeIndex, style: "", newHandlers, renderedAttrs: [], effects: Effect.always {} } addAttribute

            _ <- attrEffects |> Effect.after
            _ <- (if style != "" then Effect.setAttribute nodeIndex "style" style else Effect.always {}) |> Effect.after

            childWalkInit = Effect.always { newHandlers: newHandlersAttrs, renderedNodes: [] }

            { newHandlers: newHandlersKids, renderedNodes: renderedNodesKids } <-
                List.walk children childWalkInit createSubTree |> Effect.after

            Effect.always {
                newHandlers: newHandlersKids,
                renderedNodes: List.append renderedNodes (RenderedElement nodeIndex name size renderedAttrs renderedNodesKids),
            }

        Text content ->
            Effect.createTextNode content
            |> Effect.map \nodeIndex ->

                { newHandlers, renderedNodes: List.append renderedNodes (RenderedText nodeIndex content) }

        None -> Effect.always { newHandlers, renderedNodes: List.append renderedNodes RenderedNone }

AddAttrWalk state : {
    nodeIndex : Nat,
    style : Str,
    newHandlers : HandlerLookup state,
    renderedAttrs : List RenderedAttribute,
    effects : Effect {},
}
addAttribute : AddAttrWalk state, Attribute state -> AddAttrWalk state
addAttribute = \{ nodeIndex, style, newHandlers, renderedAttrs, effects }, attr ->
    when attr is
        EventListener name accessors handler ->
            { handlerLookup: updatedHandlers, index: handlerIndex } =
                insertHandler newHandlers handler

            # Store the handlerIndex in the rendered virtual DOM tree, since we'll need it for the next diff
            renderedAttr =
                RenderedEventListener name accessors handlerIndex

            { nodeIndex, style, newHandlers: updatedHandlers, renderedAttrs: List.append renderedAttrs renderedAttr, effects }

        HtmlAttr k v ->
            { nodeIndex, style, newHandlers, renderedAttrs: List.append renderedAttrs (RenderedHtmlAttr k v), effects: Effect.after effects (\_ -> Effect.setAttribute nodeIndex k v) }

        DomProp k v ->
            { nodeIndex, style, newHandlers, renderedAttrs: List.append renderedAttrs (RenderedDomProp k v), effects: Effect.after effects (\_ -> Effect.setProperty nodeIndex k v) }

        Style k v ->
            newStyle = "\(style) \(k):\(v);"

            { nodeIndex, style: newStyle, newHandlers, renderedAttrs: List.append renderedAttrs (RenderedStyle k v), effects }
