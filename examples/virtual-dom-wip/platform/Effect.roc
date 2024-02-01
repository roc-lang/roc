hosted Effect
    exposes [
        Effect,
        NodeId,
        HandlerId,
        TagName,
        AttrType,
        EventType,
        after,
        always,
        map,
        createElement,
        createTextNode,
        updateTextNode,
        appendChild,
        removeNode,
        replaceNode,
        setAttribute,
        removeAttribute,
        setProperty,
        removeProperty,
        setStyle,
        setListener,
        removeListener,
        enableVdomAllocator,
        disableVdomAllocator,
    ]
    generates Effect with [after, always, map]

# TODO: private types
NodeId : Nat
HandlerId : Nat

# TODO: make these tag unions to avoid encoding/decoding standard names
# but for now, this is much easier to code and debug!
TagName : Str
AttrType : Str
EventType : Str

## createElement tagName
createElement : NodeId, TagName -> Effect {}

## createTextNode content
createTextNode : NodeId, Str -> Effect {}

## updateTextNode content
updateTextNode : NodeId, Str -> Effect {}

## appendChild parentId childId
appendChild : NodeId, NodeId -> Effect {}

## removeNode id
removeNode : NodeId -> Effect {}

## replaceNode oldId newId
replaceNode : NodeId, NodeId -> Effect {}

## setAttribute nodeId attrName value
setAttribute : NodeId, AttrType, Str -> Effect {}

## removeAttribute nodeId attrName
removeAttribute : NodeId, AttrType -> Effect {}

## setProperty nodeId propName json
setProperty : NodeId, Str, List U8 -> Effect {}

## removeProperty nodeId propName
removeProperty : NodeId, Str -> Effect {}

## setStyle nodeId key value
setStyle : NodeId, Str, Str -> Effect {}

## setListener nodeId eventType accessorsJson handlerId
setListener : NodeId, EventType, List U8, HandlerId -> Effect {}

## removeListener nodeId handlerId
removeListener : NodeId, HandlerId -> Effect {}

# Enable a special memory allocator for virtual DOM
# This consists of two arenas, "even" and "odd", which alternately hold the "old" and "new" VDOM.
# After we do a diff, the "old" virtual DOM can be dropped without checking refcounts.
# Danger: Could cause memory unsafety bugs if used incorrectly! Do not expose!
# Not suitable for values that have a different lifetime from the virtual DOM!
# TODO: actually implement this for real! LOL
enableVdomAllocator : Bool -> Effect {}

# Switch back from the virtual DOM allocator to the "normal"
# allocator that is safe to use with long-lived values.
# At the same time, drop the entire "old" virtual DOM arena.
disableVdomAllocator : Effect {}
