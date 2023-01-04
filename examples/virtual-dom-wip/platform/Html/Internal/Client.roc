interface Html.Internal.Client
    exposes [
        PlatformState,
        initClientApp,
        dispatchEvent,
    ]
    imports [
        Effect.{
            Effect,
            NodeId,
            HandlerId,
            TagName,
            AttrType,
            EventType,
        },
        Html.Internal.Shared.{
            App,
            Html,
            Attribute,
            CyclicStructureAccessor,
            Handler,
            Size,
            translateStatic,
        },
        Json,
        Action,
    ]

PlatformState state initData : {
    app : App state initData,
    state,
    rendered : RenderedTree state,
}

# The rendered tree uses indices rather than pointers
# This makes it easier to communicate with JS using integer indices.
# There is a JavaScript `nodes` array that matches the Roc `nodes` List
RenderedTree state : {
    root : NodeId,
    nodes : List (Result RenderedNode [DeletedNode]),
    deletedNodeCache : List NodeId,
    handlers : List (Result (Handler state) [DeletedHandler]),
    deletedHandlerCache : List HandlerId,
}

RenderedNode : [
    RenderedNone,
    RenderedText Str,
    RenderedElement Str (List RenderedAttribute) (List NodeId),
]

RenderedAttribute : [
    RenderedEventListener Str (List CyclicStructureAccessor) HandlerId,
    RenderedHtmlAttr Str Str,
    RenderedDomProp Str (List U8),
    RenderedStyle Str Str,
]

Patch : [
    CreateElement NodeId TagName,
    CreateTextNode NodeId Str,
    UpdateTextNode NodeId Str,
    AppendChild NodeId NodeId,
    RemoveNode NodeId,
    ReplaceNode NodeId NodeId,
    SetAttribute NodeId AttrType Str,
    RemoveAttribute NodeId AttrType,
    SetProperty NodeId Str (List U8),
    RemoveProperty NodeId Str,
    SetStyle NodeId Str Str,
    SetListener NodeId EventType (List U8) HandlerId,
    RemoveListener NodeId EventType,
]

DiffState state : { rendered : RenderedTree state, patches : List Patch }

# -------------------------------
#   INITIALISATION
# -------------------------------
initClientApp : List U8, App state initData -> Effect (PlatformState state initData) | initData has Decoding
initClientApp = \json, app ->
    # Initialise the Roc representation of the rendered DOM, and calculate patches (for event listeners)
    { state, rendered, patches } =
        initClientAppHelp json app

    # Call out to JS to patch the DOM, attaching the event listeners
    _ <- applyPatches patches |> Effect.after

    Effect.always {
        app,
        state,
        rendered,
    }

# Testable helper function to initialise the app
initClientAppHelp : List U8, App state initData -> { state, rendered : RenderedTree state, patches : List Patch } | initData has Decoding
initClientAppHelp = \json, app ->
    state =
        json
        |> Decode.fromBytes Json.fromUtf8
        |> app.init
    dynamicView =
        app.render state
    staticUnindexed =
        translateStatic dynamicView
    { nodes: staticNodes } =
        indexNodes { nodes: [], siblingIds: [] } staticUnindexed
    staticRendered = {
        root: List.len staticNodes - 1,
        nodes: List.map staticNodes Ok,
        deletedNodeCache: [],
        handlers: [],
        deletedHandlerCache: [],
    }

    # Run our first diff. The only differences will be event listeners, so we will generate patches to attach those.
    { rendered, patches } =
        diff { rendered: staticRendered, patches: [] } dynamicView

    { state, rendered, patches }

# Assign an index to each (virtual) DOM node.
# In JavaScript, we maintain an array of references to real DOM nodes.
# In Roc, we maintain a matching List of virtual DOM nodes with the same indices.
# They are both initialised separately, but use the same indexing algorithm.
# (We *could* pass this data in as JSON from the HTML file, but it would roughly double the size of that HTML file!)
indexNodes : { nodes : List RenderedNode, siblingIds : List Nat }, Html state -> { nodes : List RenderedNode, siblingIds : List Nat }
indexNodes = \{ nodes, siblingIds }, unrendered ->
    when unrendered is
        Text content ->
            {
                nodes: List.append nodes (RenderedText content),
                siblingIds: List.append siblingIds (List.len nodes),
            }

        Element name _ attrs children ->
            { nodes: listWithChildren, siblingIds: childIds } =
                List.walk children { nodes, siblingIds: [] } indexNodes
            renderedAttrs =
                List.walk attrs [] \walkedAttrs, attr ->
                    when attr is
                        EventListener _ _ _ -> walkedAttrs # Dropped! Server-rendered HTML has no listeners
                        HtmlAttr k v -> List.append walkedAttrs (RenderedHtmlAttr k v)
                        DomProp k v -> List.append walkedAttrs (RenderedDomProp k v)
                        Style k v -> List.append walkedAttrs (RenderedStyle k v)

            {
                nodes: List.append listWithChildren (RenderedElement name renderedAttrs childIds),
                siblingIds: List.append siblingIds (List.len listWithChildren),
            }

        None ->
            {
                nodes: List.append nodes RenderedNone,
                siblingIds: List.append siblingIds (List.len nodes),
            }

# -------------------------------
#   Patches
# -------------------------------
applyPatch : Patch -> Effect {}
applyPatch = \patch ->
    when patch is
        CreateElement nodeId tagName -> Effect.createElement nodeId tagName
        CreateTextNode nodeId content -> Effect.createTextNode nodeId content
        UpdateTextNode nodeId content -> Effect.updateTextNode nodeId content
        AppendChild parentId childId -> Effect.appendChild parentId childId
        RemoveNode id -> Effect.removeNode id
        ReplaceNode oldId newId -> Effect.replaceNode oldId newId
        SetAttribute nodeId attrName value -> Effect.setAttribute nodeId attrName value
        RemoveAttribute nodeId attrName -> Effect.removeAttribute nodeId attrName
        SetProperty nodeId propName json -> Effect.setProperty nodeId propName json
        RemoveProperty nodeId propName -> Effect.removeProperty nodeId propName
        SetStyle nodeId key value -> Effect.setStyle nodeId key value
        SetListener nodeId eventType accessorsJson handlerId -> Effect.setListener nodeId eventType accessorsJson handlerId
        RemoveListener nodeId eventType -> Effect.removeListener nodeId eventType

walkPatches : Effect {}, Patch -> Effect {}
walkPatches = \previousEffects, patch ->
    Effect.after previousEffects \{} -> applyPatch patch

applyPatches : List Patch -> Effect {}
applyPatches = \patches ->
    List.walk patches (Effect.always {}) walkPatches

# -------------------------------
#   EVENT HANDLING
# -------------------------------
JsEventResult state initData : {
    platformState : PlatformState state initData,
    stopPropagation : Bool,
    preventDefault : Bool,
}

## Dispatch a JavaScript event to a Roc handler, given the handler ID and some JSON event data.
dispatchEvent : PlatformState state initData, List (List U8), HandlerId -> Effect (JsEventResult state initData) | initData has Decoding
dispatchEvent = \platformState, eventData, handlerId ->
    { app, state, rendered } =
        platformState
    maybeHandler =
        List.get rendered.handlers handlerId
        |> Result.withDefault (Err DeletedHandler)
    { action, stopPropagation, preventDefault } =
        when maybeHandler is
            Err DeletedHandler ->
                { action: Action.none, stopPropagation: Bool.false, preventDefault: Bool.false }

            Ok (Normal handler) ->
                { action: handler state eventData, stopPropagation: Bool.false, preventDefault: Bool.false }

            Ok (Custom handler) ->
                handler state eventData

    when action is
        Update newState ->
            newViewUnrendered =
                app.render newState
            { rendered: newRendered, patches } =
                diff { rendered, patches: [] } newViewUnrendered

            _ <- applyPatches patches |> Effect.after
            Effect.always {
                platformState: {
                    app,
                    state: newState,
                    rendered: newRendered,
                },
                stopPropagation,
                preventDefault,
            }

        None ->
            Effect.always { platformState, stopPropagation, preventDefault }

insertHandler : RenderedTree state, Handler state -> { handlerId : HandlerId, rendered : RenderedTree state }
insertHandler = \rendered, newHandler ->
    when List.last rendered.deletedHandlerCache is
        Ok handlerId ->
            {
                handlerId,
                rendered: { rendered &
                    handlers: List.set rendered.handlers handlerId (Ok newHandler),
                    deletedHandlerCache: List.dropLast rendered.deletedHandlerCache,
                },
            }

        Err _ ->
            {
                handlerId: List.len rendered.handlers,
                rendered: { rendered &
                    handlers: List.append rendered.handlers (Ok newHandler),
                },
            }

# replaceHandler : RenderedTree state, Nat, Handler state -> RenderedTree state
# replaceHandler = \rendered, index, newHandler ->
#     { list } = List.replace rendered.handlers index (Ok newHandler)
#     { rendered & handlers: list }
# -------------------------------
#   DIFF
# -------------------------------
diff : DiffState state, Html state -> DiffState state
diff = \{ rendered, patches }, newNode ->
    oldNode =
        List.get rendered.nodes rendered.root
        |> Result.withDefault (Ok RenderedNone)
        |> Result.withDefault (RenderedNone)

    when { oldNode, newNode } is
        { oldNode: RenderedText oldContent, newNode: Text newContent } ->
            if newContent != oldContent then
                newNodes =
                    List.set rendered.nodes rendered.root (Ok (RenderedText newContent))

                {
                    rendered: { rendered &
                        nodes: newNodes,
                    },
                    patches: List.append patches (UpdateTextNode rendered.root newContent),
                }
            else
                { rendered, patches }

        { oldNode: RenderedElement _oldName _oldAttrs _oldChildren, newNode: Element _newName _newSize _newAttrs _newChildren } ->
            # TODO: actual diffing! LOL This is just to get something up and running
            replaceNode { rendered, patches } rendered.root newNode

        { oldNode: RenderedNone, newNode: None } ->
            { rendered, patches }

        _ ->
            # old node has been replaced with a totally different variant. There's no point in diffing, just replace.
            replaceNode { rendered, patches } rendered.root newNode

replaceNode : DiffState state, NodeId, Html state -> DiffState state
replaceNode = \diffState, oldNodeId, newNode ->
    { rendered: createRendered, patches: createPatches, id: createNodeId } =
        createNode diffState newNode
    preDeleteState = {
        rendered: createRendered,
        patches: List.append createPatches (ReplaceNode oldNodeId createNodeId),
    }

    deleteNode preDeleteState oldNodeId

deleteNode : DiffState state, NodeId -> DiffState state
deleteNode = \diffState, id ->
    { rendered, patches } =
        when List.get diffState.rendered.nodes id is
            Ok node ->
                when node is
                    Ok (RenderedElement _ _ children) ->
                        List.walk children diffState deleteNode

                    _ -> diffState

            _ -> diffState
    newNodes = List.set rendered.nodes id (Err DeletedNode)
    newDeletedNodeCache = List.append rendered.deletedNodeCache id
    newPatches = List.append patches (RemoveNode id)

    {
        rendered: { rendered &
            nodes: newNodes,
            deletedNodeCache: newDeletedNodeCache,
        },
        patches: newPatches,
    }

createNode : DiffState state, Html state -> { rendered : RenderedTree state, patches : List Patch, id : NodeId }
createNode = \{ rendered, patches }, newNode ->
    when newNode is
        Text content ->
            { rendered: newRendered, id } =
                insertNode rendered (RenderedText content)

            {
                rendered: newRendered,
                patches: List.append patches (CreateTextNode id content),
                id,
            }

        None ->
            { rendered: newRendered, id } =
                insertNode rendered RenderedNone

            { rendered: newRendered, patches, id }

        Element tagName _ attrs children ->
            { rendered: renderedWithChildren, patches: patchesWithChildren, ids: childIds } =
                List.walk children { rendered, patches, ids: [] } createChildNode
            nodeId =
                nextNodeId renderedWithChildren
            patchesWithElem =
                List.append patchesWithChildren (CreateElement nodeId tagName)
            { renderedAttrs, rendered: renderedWithAttrs, patches: patchesWithAttrs } =
                renderAttrs attrs renderedWithChildren patchesWithElem nodeId
            { rendered: renderedWithNode } =
                insertNode renderedWithAttrs (RenderedElement tagName renderedAttrs childIds)

            {
                rendered: renderedWithNode,
                patches: patchesWithAttrs,
                id: nodeId,
            }

renderAttrs : List (Attribute state), RenderedTree state, List Patch, NodeId -> { renderedAttrs : List RenderedAttribute, rendered : RenderedTree state, patches : List Patch }
renderAttrs = \attrs, rendered, patches, nodeId ->
    List.walk attrs { renderedAttrs: [], rendered, patches } \walkState, attr ->
        when attr is
            HtmlAttr k v ->
                { walkState &
                    renderedAttrs: List.append walkState.renderedAttrs (RenderedHtmlAttr k v),
                    patches: List.append walkState.patches (SetAttribute nodeId k v),
                }

            DomProp k v ->
                { walkState &
                    renderedAttrs: List.append walkState.renderedAttrs (RenderedDomProp k v),
                    patches: List.append walkState.patches (SetProperty nodeId k v),
                }

            Style k v ->
                { walkState &
                    renderedAttrs: List.append walkState.renderedAttrs (RenderedStyle k v),
                    patches: List.append walkState.patches (SetStyle nodeId k v),
                }

            EventListener eventType accessors handler ->
                { handlerId, rendered: renderedWithHandler } =
                    insertHandler walkState.rendered handler
                accessorsJson =
                    accessors |> Encode.toBytes Json.toUtf8
                listener =
                    RenderedEventListener eventType accessors handlerId
                patch =
                    SetListener nodeId eventType accessorsJson handlerId

                {
                    renderedAttrs: List.append walkState.renderedAttrs listener,
                    rendered: renderedWithHandler,
                    patches: List.append walkState.patches patch,
                }

createChildNode :
    { rendered : RenderedTree state, patches : List Patch, ids : List NodeId },
    Html state
    -> { rendered : RenderedTree state, patches : List Patch, ids : List NodeId }
createChildNode = \{ rendered, patches, ids }, childHtml ->
    { rendered: renderedChild, patches: childPatches, id } =
        createNode { rendered, patches } childHtml

    {
        rendered: renderedChild,
        patches: childPatches,
        ids: List.append ids id,
    }

# insert a node into the nodes list, assigning it a NodeId
insertNode : RenderedTree state, RenderedNode -> { rendered : RenderedTree state, id : NodeId }
insertNode = \rendered, node ->
    when List.last rendered.deletedNodeCache is
        Ok id ->
            newRendered =
                { rendered &
                    nodes: List.set rendered.nodes id (Ok node),
                    deletedNodeCache: List.dropLast rendered.deletedNodeCache,
                }

            { rendered: newRendered, id }

        Err _ ->
            newRendered =
                { rendered &
                    nodes: List.append rendered.nodes (Ok node),
                }

            { rendered: newRendered, id: List.len rendered.nodes }

# Predict what NodeId will be assigned next, without actually assigning it
nextNodeId : RenderedTree state -> NodeId
nextNodeId = \rendered ->
    when List.last rendered.deletedNodeCache is
        Ok id -> id
        Err _ -> List.len rendered.nodes

# -------------------------------
#   TESTS
# -------------------------------
# indexNodes
expect
    html : Html {}
    html =
        Element "a" 43 [HtmlAttr "href" "https://www.roc-lang.org/"] [Text "Roc"]

    actual : { nodes : List RenderedNode, siblingIds : List Nat }
    actual =
        indexNodes { nodes: [], siblingIds: [] } html

    expected : { nodes : List RenderedNode, siblingIds : List Nat }
    expected = {
        nodes: [
            RenderedText "Roc",
            RenderedElement "a" [RenderedHtmlAttr "href" "https://www.roc-lang.org/"] [0],
        ],
        siblingIds: [1],
    }

    actual == expected

# initClientAppHelp
expect
    State : { answer : Nat }

    init = \result ->
        when result is
            Ok state -> state
            Err _ -> { answer: 0 }

    onClickHandler : Handler State
    onClickHandler =
        Normal \state, _ -> Action.update { answer: state.answer + 1 }

    render : State -> Html State
    render = \state ->
        num = Num.toStr state.answer

        onClickAttr : Attribute State
        onClickAttr =
            EventListener "click" [] onClickHandler

        # Sizes don't matter for this front-end test, only for back end rendering of HTML strings.
        Element "body" 0 [] [
            Element "h1" 0 [] [Text "The app"],
            Element "div" 0 [onClickAttr] [Text "The answer is \(num)"],
        ]

    app : App State State
    app = {
        init,
        render,
        wasmUrl: "assets/test.wasm",
    }

    initJson : List U8
    initJson =
        # { answer: 42 } |> Encode.toBytes Json.toUtf8 # panics at mono/src/ir.rs:5739:56
        "{ answer: 42 }" |> Str.toUtf8

    # COMPILER BUG? 'no lambdaset found'
    actual : { state : State, rendered : RenderedTree State, patches : List Patch }
    actual =
        initClientAppHelp initJson app

    expected : { state : State, rendered : RenderedTree State, patches : List Patch }
    expected = {
        state: { answer: 42 },
        rendered: {
            root: 0,
            nodes: [
                Ok (RenderedText "The app"),
                Ok (RenderedElement "h1" [] [0]),
                Ok (RenderedText "The answer is 42"),
                Ok (RenderedElement "div" [RenderedEventListener "click" [] 0] [2]),
                Ok (RenderedElement "body" [] [1, 3]),
            ],
            deletedNodeCache: [],
            handlers: [Ok onClickHandler],
            deletedHandlerCache: [],
        },
        patches: [SetListener 3 "click" [] 0],
    }

    (actual.state == expected.state)
    && (actual.rendered.root == expected.rendered.root)
    && (actual.rendered.nodes == expected.rendered.nodes)
    && (actual.rendered.deletedNodeCache == expected.rendered.deletedNodeCache)
    && (actual.rendered.deletedHandlerCache == expected.rendered.deletedHandlerCache)
    && (actual.patches == expected.patches)
