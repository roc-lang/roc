module [
    PlatformState,
    initClientApp,
    dispatchEvent,
]

import Effect exposing [
    Effect,
    NodeId,
    HandlerId,
    TagName,
    AttrType,
    EventType,
]
import Html.Internal.Shared exposing [
    App,
    Html,
    Attribute,
    CyclicStructureAccessor,
    Handler,
    translateStatic,
]
import Json
import Action

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
    RenderedElement Str RenderedAttributes (List NodeId),
]

RenderedAttributes : {
    eventListeners : Dict Str { accessors : List CyclicStructureAccessor, handlerId : HandlerId },
    htmlAttrs : Dict Str Str,
    domProps : Dict Str (List U8),
    styles : Dict Str Str,
}

emptyRenderedAttrs = {
    eventListeners: Dict.empty {},
    htmlAttrs: Dict.empty {},
    domProps: Dict.empty {},
    styles: Dict.empty {},
}

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
    RemoveListener NodeId HandlerId,
]

DiffState state : { rendered : RenderedTree state, patches : List Patch }

# -------------------------------
#   INITIALISATION
# -------------------------------
initClientApp : List U8, App state initData -> Effect (PlatformState state initData) where initData implements Decoding
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
initClientAppHelp : List U8, App state initData -> { state, rendered : RenderedTree state, patches : List Patch } where initData implements Decoding
initClientAppHelp = \json, app ->
    state =
        json
        |> Decode.fromBytes Json.json
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
indexNodes : { nodes : List RenderedNode, siblingIds : List U64 }, Html state -> { nodes : List RenderedNode, siblingIds : List U64 }
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
                List.walk attrs emptyRenderedAttrs \walkedAttrs, attr ->
                    when attr is
                        EventListener _ _ _ -> walkedAttrs # Dropped! Server-rendered HTML has no listeners
                        HtmlAttr k v -> { walkedAttrs & htmlAttrs: Dict.insert walkedAttrs.htmlAttrs k v }
                        DomProp k v -> { walkedAttrs & domProps: Dict.insert walkedAttrs.domProps k v }
                        Style k v -> { walkedAttrs & styles: Dict.insert walkedAttrs.styles k v }

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
        RemoveListener nodeId handlerId -> Effect.removeListener nodeId handlerId

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
dispatchEvent : PlatformState state initData, List (List U8), HandlerId -> Effect (JsEventResult state initData) where initData implements Decoding
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

# -------------------------------
#   DIFF
# -------------------------------
diff : DiffState state, Html state -> DiffState state
diff = \{ rendered, patches }, newNode ->
    root =
        rendered.root
    oldNode =
        List.get rendered.nodes root
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

        { oldNode: RenderedElement oldName oldAttrs oldChildren, newNode: Element newName _ newAttrs newChildren } ->
            if newName != oldName then
                replaceNode { rendered, patches } root newNode
            else
                stateAttrs =
                    diffAttrs { rendered, patches } root oldAttrs newAttrs
                stateChildPairs =
                    List.map2 oldChildren newChildren (\oldChildId, newChild -> { oldChildId, newChild })
                    |> List.walk stateAttrs \childWalkState, { oldChildId, newChild } ->
                        { rendered: childWalkRendered, patches: childWalkPatches } = childWalkState
                        diff { rendered: { childWalkRendered & root: oldChildId }, patches: childWalkPatches } newChild
                { rendered: renderedLeftOverChildren, patches: patchesLeftOverChildren } =
                    if List.len oldChildren > List.len newChildren then
                        List.walkFrom oldChildren (List.len newChildren) stateChildPairs deleteNode
                    else if List.len oldChildren < List.len newChildren then
                        stateBeforeCreate = {
                            rendered: stateChildPairs.rendered,
                            patches: stateChildPairs.patches,
                            ids: [],
                        }
                        { rendered: renderedAfterCreate, patches: patchesAfterCreate, ids: createdIds } =
                            List.walkFrom newChildren (List.len oldChildren) stateBeforeCreate createChildNode
                        # Look up the children again since they might have new node IDs!
                        nodeWithUpdatedChildren =
                            when List.get renderedAfterCreate.nodes root is
                                Ok (Ok (RenderedElement n a c)) -> RenderedElement n a (List.concat c createdIds)
                                _ -> crash "Bug in virtual-dom framework: nodeWithUpdatedChildren not found"
                        updatedNodes =
                            List.set renderedAfterCreate.nodes root (Ok nodeWithUpdatedChildren)

                        {
                            rendered: { renderedAfterCreate & nodes: updatedNodes },
                            patches: List.walk createdIds patchesAfterCreate \p, id -> List.append p (AppendChild root id),
                        }
                    else
                        stateChildPairs

                {
                    rendered: { renderedLeftOverChildren & root },
                    patches: patchesLeftOverChildren,
                }

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

# Delete a node, and drop any JS references to its children and event listeners
# TODO: see if it would speed things up to leave this junk lying around until the slot is reused.
# Any danger of spurious events being sent to the wrong handler?
# Otherwise, can we sweep everything at once at the end of the diff?
# Let's be conservative on things like this until we have more test cases working.
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

    patchesRemoveListeners =
        when List.get rendered.nodes id is
            Ok (Ok (RenderedElement _ attrs _)) ->
                Dict.walk attrs.eventListeners patches \p, _, { handlerId } ->
                    List.append p (RemoveListener id handlerId)

            _ -> patches

    newNodes =
        List.set rendered.nodes id (Err DeletedNode)
    newDeletedNodeCache =
        List.append rendered.deletedNodeCache id
    newPatches =
        List.append patchesRemoveListeners (RemoveNode id)

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

AttrDiffState state : {
    nodeId : NodeId,
    attrs : RenderedAttributes,
    patches : List Patch,
    handlers : List (Result (Handler state) [DeletedHandler]),
    deletedHandlerCache : List HandlerId,
}

diffAttrs : DiffState state, NodeId, RenderedAttributes, List (Attribute state) -> DiffState state
diffAttrs = \{ rendered, patches }, nodeId, attrs, newAttrs ->
    initState = {
        nodeId,
        attrs,
        patches,
        handlers: rendered.handlers,
        deletedHandlerCache: rendered.deletedHandlerCache,
    }
    finalState =
        List.walk newAttrs initState diffAttr
    newRendered =
        { rendered &
            handlers: finalState.handlers,
            deletedHandlerCache: finalState.deletedHandlerCache,
        }

    {
        rendered: newRendered,
        patches: finalState.patches,
    }

diffAttr : AttrDiffState state, Attribute state -> AttrDiffState state
diffAttr = \{ nodeId, attrs, patches, handlers, deletedHandlerCache }, attr ->
    when attr is
        EventListener eventName newAccessors newHandler ->
            when Dict.get attrs.eventListeners eventName is
                Ok { accessors, handlerId } ->
                    (Tuple newAttrs newPatches) =
                        if accessors == newAccessors then
                            Tuple attrs patches
                        else
                            json = newAccessors |> Encode.toBytes Json.json

                            Tuple
                                { attrs & eventListeners: Dict.insert attrs.eventListeners eventName { accessors, handlerId } }
                                (
                                    patches
                                    |> List.append (RemoveListener nodeId handlerId)
                                    |> List.append (SetListener nodeId eventName json handlerId)
                                )

                    {
                        nodeId,
                        attrs: newAttrs,
                        patches: newPatches,
                        handlers: List.set handlers handlerId (Ok newHandler),
                        deletedHandlerCache,
                    }

                Err KeyNotFound ->
                    renderAttr { nodeId, attrs, patches, handlers, deletedHandlerCache } attr

        HtmlAttr k v ->
            when Dict.get attrs.htmlAttrs k is
                Ok oldVal ->
                    (Tuple newAttrs newPatches) =
                        if oldVal == v then
                            Tuple attrs patches
                        else
                            Tuple
                                { attrs & htmlAttrs: Dict.insert attrs.htmlAttrs k v }
                                (patches |> List.append (SetAttribute nodeId k v))
                    {
                        nodeId,
                        attrs: newAttrs,
                        patches: newPatches,
                        handlers,
                        deletedHandlerCache,
                    }

                Err KeyNotFound ->
                    renderAttr { nodeId, attrs, patches, handlers, deletedHandlerCache } attr

        DomProp k v ->
            when Dict.get attrs.domProps k is
                Ok oldVal ->
                    (Tuple newAttrs newPatches) =
                        if oldVal == v then
                            Tuple attrs patches
                        else
                            Tuple
                                { attrs & domProps: Dict.insert attrs.domProps k v }
                                (patches |> List.append (SetProperty nodeId k v))
                    {
                        nodeId,
                        attrs: newAttrs,
                        patches: newPatches,
                        handlers,
                        deletedHandlerCache,
                    }

                Err KeyNotFound ->
                    renderAttr { nodeId, attrs, patches, handlers, deletedHandlerCache } attr

        Style k v ->
            when Dict.get attrs.styles k is
                Ok oldVal ->
                    (Tuple newAttrs newPatches) =
                        if oldVal == v then
                            Tuple attrs patches
                        else
                            Tuple
                                { attrs & styles: Dict.insert attrs.styles k v }
                                (patches |> List.append (SetStyle nodeId k v))
                    {
                        nodeId,
                        attrs: newAttrs,
                        patches: newPatches,
                        handlers,
                        deletedHandlerCache,
                    }

                Err KeyNotFound ->
                    renderAttr { nodeId, attrs, patches, handlers, deletedHandlerCache } attr

renderAttrs : List (Attribute state), RenderedTree state, List Patch, NodeId -> { renderedAttrs : RenderedAttributes, rendered : RenderedTree state, patches : List Patch }
renderAttrs = \attrs, rendered, patches, nodeId ->
    initState = {
        nodeId,
        attrs: emptyRenderedAttrs,
        patches,
        handlers: rendered.handlers,
        deletedHandlerCache: rendered.deletedHandlerCache,
    }
    finalState =
        List.walk attrs initState renderAttr

    {
        renderedAttrs: finalState.attrs,
        rendered: { rendered &
            handlers: finalState.handlers,
            deletedHandlerCache: finalState.deletedHandlerCache,
        },
        patches: finalState.patches,
    }

renderAttr : AttrDiffState state, Attribute state -> AttrDiffState state
renderAttr = \{ nodeId, attrs, patches, handlers, deletedHandlerCache }, attr ->
    when attr is
        HtmlAttr k v ->
            {
                nodeId,
                handlers,
                deletedHandlerCache,
                attrs: { attrs & htmlAttrs: Dict.insert attrs.htmlAttrs k v },
                patches: List.append patches (SetAttribute nodeId k v),
            }

        DomProp k v ->
            {
                nodeId,
                handlers,
                deletedHandlerCache,
                attrs: { attrs & domProps: Dict.insert attrs.domProps k v },
                patches: List.append patches (SetProperty nodeId k v),
            }

        Style k v ->
            {
                nodeId,
                handlers,
                deletedHandlerCache,
                attrs: { attrs & styles: Dict.insert attrs.styles k v },
                patches: List.append patches (SetStyle nodeId k v),
            }

        EventListener eventType accessors handler ->
            { handlerId, newHandlers, newDeletedHandlerCache } =
                when List.last deletedHandlerCache is
                    Ok id ->
                        {
                            handlerId: id,
                            newHandlers: List.set handlers id (Ok handler),
                            newDeletedHandlerCache: List.dropLast deletedHandlerCache 1,
                        }

                    Err _ ->
                        {
                            handlerId: List.len handlers,
                            newHandlers: List.append handlers (Ok handler),
                            newDeletedHandlerCache: deletedHandlerCache,
                        }
            accessorsJson =
                accessors |> Encode.toBytes Json.json
            patch =
                SetListener nodeId eventType accessorsJson handlerId

            {
                nodeId,
                attrs: { attrs & eventListeners: Dict.insert attrs.eventListeners eventType { accessors, handlerId } },
                handlers: newHandlers,
                deletedHandlerCache: newDeletedHandlerCache,
                patches: List.append patches patch,
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
                    deletedNodeCache: List.dropLast rendered.deletedNodeCache 1,
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
eqRenderedTree : RenderedTree state, RenderedTree state -> Bool
eqRenderedTree = \a, b ->
    (a.root == b.root)
    && (a.nodes == b.nodes)
    && (List.len a.handlers == List.len b.handlers)
    && (a.deletedNodeCache == b.deletedNodeCache)
    && (a.deletedHandlerCache == b.deletedHandlerCache)

# indexNodes
expect
    html : Html {}
    html =
        Element "a" 43 [HtmlAttr "href" "https://www.roc-lang.org/"] [Text "Roc"]

    actual : { nodes : List RenderedNode, siblingIds : List U64 }
    actual =
        indexNodes { nodes: [], siblingIds: [] } html

    expected : { nodes : List RenderedNode, siblingIds : List U64 }
    expected = {
        nodes: [
            RenderedText "Roc",
            RenderedElement "a" { emptyRenderedAttrs & htmlAttrs: Dict.fromList [("href", "https://www.roc-lang.org/")] } [0],
        ],
        siblingIds: [1],
    }

    (actual.nodes == expected.nodes)
    && (actual.siblingIds == expected.siblingIds)

# diff
expect
    State : { answer : U32 }

    diffStateBefore : DiffState State
    diffStateBefore = {
        rendered: {
            root: 4,
            nodes: [
                Ok (RenderedText "The app"),
                Ok (RenderedElement "h1" emptyRenderedAttrs [0]),
                Ok (RenderedText "The answer is 42"),
                Ok (RenderedElement "div" emptyRenderedAttrs [2]),
                Ok (RenderedElement "body" emptyRenderedAttrs [1, 3]),
            ],
            deletedNodeCache: [],
            handlers: [],
            deletedHandlerCache: [],
        },
        patches: [],
    }

    # Sizes don't matter, use zero. We are not creating a HTML string so we don't care what size it would be.
    newNode : Html State
    newNode =
        Element "body" 0 [] [
            Element "h1" 0 [] [Text "The app"],
            Element "div" 0 [] [Text "The answer is 111"],
        ]

    expected : DiffState State
    expected = {
        rendered: {
            root: 4,
            nodes: [
                Ok (RenderedText "The app"),
                Ok (RenderedElement "h1" emptyRenderedAttrs [0]),
                Ok (RenderedText "The answer is 111"),
                Ok (RenderedElement "div" emptyRenderedAttrs [2]),
                Ok (RenderedElement "body" emptyRenderedAttrs [1, 3]),
            ],
            deletedNodeCache: [],
            handlers: [],
            deletedHandlerCache: [],
        },
        patches: [UpdateTextNode 2 "The answer is 111"],
    }

    actual : DiffState State
    actual =
        diff diffStateBefore newNode

    (actual.patches == expected.patches)
    && eqRenderedTree actual.rendered expected.rendered

# initClientAppHelp
expect
    State : { answer : U32 }

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

        # Sizes don't matter, use zero. We are not creating a HTML string so we don't care what size it would be.
        Element "body" 0 [] [
            Element "h1" 0 [] [Text "The app"],
            Element "div" 0 [onClickAttr] [Text "The answer is $(num)"],
        ]

    app : App State State
    app = {
        init,
        render,
        wasmUrl: "assets/test.wasm",
    }

    initJson : List U8
    initJson =
        { answer: 42 } |> Encode.toBytes Json.json # panics at mono/src/ir.rs:5739:56
    expected : { state : State, rendered : RenderedTree State, patches : List Patch }
    expected = {
        state: { answer: 42 },
        rendered: {
            root: 4,
            nodes: [
                Ok (RenderedText "The app"),
                Ok (RenderedElement "h1" emptyRenderedAttrs [0]),
                Ok (RenderedText "The answer is 42"),
                Ok (RenderedElement "div" { emptyRenderedAttrs & eventListeners: Dict.fromList [("click", { accessors: [], handlerId: 0 })] } [2]),
                Ok (RenderedElement "body" emptyRenderedAttrs [1, 3]),
            ],
            deletedNodeCache: [],
            handlers: [Ok onClickHandler],
            deletedHandlerCache: [],
        },
        patches: [SetListener 3 "click" [] 0],
    }

    actual : { state : State, rendered : RenderedTree State, patches : List Patch }
    actual =
        initClientAppHelp initJson app

    (actual.state == expected.state)
    && eqRenderedTree actual.rendered expected.rendered
    && (actual.patches == expected.patches)
