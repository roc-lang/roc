hosted Effect
    exposes [
        Effect,
        always,
        after,
        NodeId,
        nodeId,
        EventHandlerId,
        eventHandlerId,
        createElement,
        createTextNode,
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
    generates Effect with [always, after]

NodeId := Nat
nodeId = \id -> @NodeId id

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
# This consists of two arenas, which alternate between "old" and "new".
# After we do a diff, the "old" virtual DOM can be dropped without checking refcounts.
# Danger: Could cause memory unsafety bugs if used incorrectly! Do not expose!
# Not suitable for values that have a different lifetime from the virtual DOM!
enableVdomAllocator : Effect {}

# Switch back from the virtual DOM allocator to the "normal"
# allocator that is safe to use with long-lived values.
# At the same time, drop the entire "old" virtual DOM arena.
disableVdomAllocator : Effect {}
