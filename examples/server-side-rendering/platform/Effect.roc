hosted Effect
    exposes [
        Effect,
        after,
        always,
        map,
        NodeId,
        EventHandlerId,
        eventHandlerId,
        createElement,
        createTextNode,
        updateTextNode,
        appendChild,
        removeNode,
        setAttribute,
        removeAttribute,
        setProperty,
        removeProperty,
        setListener,
        removeListener,
        enableVdomAllocator,
        disableVdomAllocator,
    ]
    imports []
    generates Effect with [after, always, map]

# TODO: private type
NodeId : Nat

EventHandlerId := Nat
eventHandlerId = \id -> @EventHandlerId id

# TODO: make these tag unions to avoid encoding/decoding standard names
TagName : Str
AttrType : Str
EventType : Str

## createElement tagName
createElement : TagName -> Effect NodeId

## createTextNode content
createTextNode : Str -> Effect NodeId

## updateTextNode content
updateTextNode : NodeId, Str -> Effect {}

## appendChild parentId childId
appendChild : NodeId, NodeId -> Effect {}

## removeNode id
removeNode : NodeId -> Effect {}

## setAttribute nodeId attrName value
setAttribute : NodeId, AttrType, Str -> Effect {}

## removeAttribute nodeId attrName
removeAttribute : NodeId, AttrType -> Effect {}

## setProperty nodeId propName json
setProperty : NodeId, Str, List U8 -> Effect {}

## removeProperty nodeId propName
removeProperty : NodeId, Str -> Effect {}

## setListener nodeId eventType handlerId
setListener : NodeId, EventType, EventHandlerId -> Effect {}

## removeListener nodeId eventType
removeListener : NodeId, EventType -> Effect {}

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
