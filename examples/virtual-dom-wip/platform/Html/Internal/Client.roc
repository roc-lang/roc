interface Html.Internal.Client
    exposes [
        Patch,
        initClientApp,

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
    ]

PlatformState state initData : {
    app : App state initData,
    state,
    rendered : RenderedTree state,
    isOddArena : Bool,
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
    SetListener NodeId EventType HandlerId,
    RemoveListener NodeId EventType,
]

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
        SetListener nodeId eventType handlerId -> Effect.setListener nodeId eventType handlerId
        RemoveListener nodeId eventType -> Effect.removeListener nodeId eventType

walkPatches : Effect {}, Patch -> Effect {}
walkPatches = \previousEffects, patch ->
    Effect.after previousEffects \{} -> applyPatch patch

applyPatches : List Patch -> Effect {}
applyPatches = \patches ->
    List.walk patches (Effect.always {}) walkPatches

# -------------------------------
#   INITIALISATION
# -------------------------------
initClientApp : List U8, App state initData -> Effect (PlatformState state initData) | initData has Decoding
initClientApp = \json, app ->
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

    staticRendered : RenderedTree state
    staticRendered = {
        root: 0,
        nodes: List.map staticNodes Ok,
        deletedNodeCache: [],
        handlers: [],
        deletedHandlerCache: [],
    }
    # Run the first diff. The only differences are event listeners, so they will be inserted into the real DOM.
    { rendered, patches } =
        diff { rendered: staticRendered, patches: [] } dynamicView

    _ <- applyPatches patches |> Effect.after

    Effect.always {
        app,
        state,
        rendered,
        isOddArena: Bool.false,
    }

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
#   EVENT HANDLING
# -------------------------------
# JsEventResult state initData : {
#     platformState : PlatformState state initData,
#     stopPropagation : Bool,
#     preventDefault : Bool,
# }
# ## Dispatch a JavaScript event to a Roc handler, given the handler ID and some JSON event data.
# ## DANGER: this function does unusual stuff with memory allocation lifetimes. Be as careful as you would with Zig or C code!
# dispatchEvent : PlatformState state initData, List (List U8), Nat -> Effect (JsEventResult state initData)
# dispatchEvent = \platformState, eventData, handlerId ->
#     { app, state, rendered, isOddArena: wasOddArena } = platformState
#     maybeHandler =
#         List.get handlerLookup.handlers handlerId
#         |> Result.withDefault (Err DeletedHandler)
#     { action, stopPropagation, preventDefault } =
#         when maybeHandler is
#             Err DeletedHandler ->
#                 { action: Action.none, stopPropagation: Bool.false, preventDefault: Bool.false }
#             Ok (Normal handler) ->
#                 { action: handler state eventData, stopPropagation: Bool.false, preventDefault: Bool.false }
#             Ok (Custom handler) ->
#                 handler state eventData
#     when action is
#         Update newState ->
#             # Any values created in the arena will all be freed on the next update
#             isOddArena = !wasOddArena
#             runInVdomArena isOddArena \_ ->
#                 newViewUnrendered = app.render newState
#                 numHandlers = List.len handlerLookup.handlers
#                 emptyHandlerLookup = {
#                     handlers: List.repeat (Err DeletedHandler) numHandlers,
#                     freeList: List.range { start: At (numHandlers - 1), end: At 0 },
#                 }
#                 # { newHandlers, rendered: newRendered } <-
#                 #     diffAndUpdateDom emptyHandlerLookup rendered newViewUnrendered |> Effect.after
#                 newPlatformState = {
#                     app,
#                     state: newState,
#                     rendered: [], #newRendered,
#                     handlerLookup: emptyHandlerLookup, # newHandlers,
#                     isOddArena,
#                 }
#                 Effect.always ({ platformState: newPlatformState, stopPropagation, preventDefault })
#         None ->
#             Effect.always { platformState, stopPropagation, preventDefault }
# runInVdomArena : Bool, ({} -> Effect a) -> Effect a
# runInVdomArena = \useOddArena, run ->
#     _ <- Effect.enableVdomAllocator useOddArena |> Effect.after
#     returnVal <- run {} |> Effect.after
#     _ <- Effect.disableVdomAllocator |> Effect.after
#     Effect.always returnVal
# insertHandler : HandlerLookup state, Handler state -> { index : Nat, handlerLookup : HandlerLookup state }
# insertHandler = \{ handlers, freeList }, newHandler ->
#     when List.last freeList is
#         Ok index ->
#             {
#                 index,
#                 handlerLookup: {
#                     handlers: List.set handlers index (Ok newHandler),
#                     freeList: List.dropLast freeList,
#                 },
#             }
#         Err _ ->
#             {
#                 index: List.len handlers,
#                 handlerLookup: {
#                     handlers: List.append handlers (Ok newHandler),
#                     freeList: freeList,
#                 },
#             }
# replaceHandler : HandlerLookup state, Nat, Handler state -> HandlerLookup state
# replaceHandler = \{ handlers, freeList }, index, newHandler ->
#     { list } = List.replace handlers index (Ok newHandler)
#     {
#         handlers: list,
#         freeList,
#     }
# -------------------------------
#   DIFF
# -------------------------------
DiffState state : { rendered : RenderedTree state, patches : List Patch }

diff : DiffState state, Html state -> DiffState state
diff = \{ rendered, patches }, newNode ->
    oldNode =
        List.get rendered.nodes rendered.root
        |> Result.withDefault (Ok RenderedNone)
        |> Result.withDefault (RenderedNone)

    when { oldNode, newNode } is
        { oldNode: RenderedText oldContent, newNode: Text newContent } ->
            if newContent != oldContent then
                newNodes = List.set rendered.nodes rendered.root (Ok (RenderedText newContent))

                {
                    rendered: { rendered & nodes: newNodes },
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
        rendered: { rendered & nodes: newNodes, deletedNodeCache: newDeletedNodeCache },
        patches: newPatches,
    }

createNode : DiffState state, Html state -> { rendered : RenderedTree state, patches : List Patch, id : NodeId }
createNode = \{ rendered, patches }, newNode ->
    when newNode is
        Text content ->
            { rendered: newRendered, id } = insertNode rendered (RenderedText content)

            {
                rendered: newRendered,
                patches: List.append patches (CreateTextNode id content),
                id,
            }

        None ->
            { rendered: newRendered, id } = insertNode rendered RenderedNone

            { rendered: newRendered, patches, id }

        Element name _ _attrs children ->
            { rendered: renderedWithChildren, patches: patchesChildren, ids: childIds } = List.walk children { rendered, patches, ids: [] } createChildNode

            renderedAttrs = [] # TODO
            { rendered: newRendered, id } =
                insertNode renderedWithChildren (RenderedElement name renderedAttrs childIds)

            patchesCreate = List.append patchesChildren (CreateElement id name)
            patchesAttrs = patchesCreate # TODO

            {
                rendered: newRendered,
                patches: patchesAttrs,
                id,
            }

# renderedAttrs =
#     List.walk attrs [] \walkedAttrs, attr ->
#         when attr is
#             EventListener _ _ _ -> walkedAttrs # Dropped! Server-rendered HTML has no listeners
#             HtmlAttr k v -> List.append walkedAttrs (RenderedHtmlAttr k v)
#             DomProp k v -> List.append walkedAttrs (RenderedDomProp k v)
#             Style k v -> List.append walkedAttrs (RenderedStyle k v)
createChildNode :
    { rendered : RenderedTree state, patches : List Patch, ids : List NodeId },
    Html state
    -> { rendered : RenderedTree state, patches : List Patch, ids : List NodeId }
createChildNode = \{ rendered, patches, ids }, childHtml ->
    { rendered: renderedChild, patches: childPatches, id } = createNode { rendered, patches } childHtml

    {
        rendered: renderedChild,
        patches: childPatches,
        ids: List.append ids id,
    }

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
