hosted PlatformTask
    exposes [
        NodeId,
        HandlerId,
        TagName,
        AttrType,
        EventType,
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
    imports []

# TODO: private types
NodeId : U64
HandlerId : U64

# TODO: make these tag unions to avoid encoding/decoding standard names
# but for now, this is much easier to code and debug!
TagName : Str
AttrType : Str
EventType : Str

## createElement tagName
createElement : NodeId, TagName -> Task {} *

## createTextNode content
createTextNode : NodeId, Str -> Task {} *

## updateTextNode content
updateTextNode : NodeId, Str -> Task {} *

## appendChild parentId childId
appendChild : NodeId, NodeId -> Task {} *

## removeNode id
removeNode : NodeId -> Task {} *

## replaceNode oldId newId
replaceNode : NodeId, NodeId -> Task {} *

## setAttribute nodeId attrName value
setAttribute : NodeId, AttrType, Str -> Task {} *

## removeAttribute nodeId attrName
removeAttribute : NodeId, AttrType -> Task {} *

## setProperty nodeId propName json
setProperty : NodeId, Str, List U8 -> Task {} *

## removeProperty nodeId propName
removeProperty : NodeId, Str -> Task {} *

## setStyle nodeId key value
setStyle : NodeId, Str, Str -> Task {} *

## setListener nodeId eventType accessorsJson handlerId
setListener : NodeId, EventType, List U8, HandlerId -> Task {} *

## removeListener nodeId handlerId
removeListener : NodeId, HandlerId -> Task {} *

# Enable a special memory allocator for virtual DOM
# This consists of two arenas, "even" and "odd", which alternately hold the "old" and "new" VDOM.
# After we do a diff, the "old" virtual DOM can be dropped without checking refcounts.
# Danger: Could cause memory unsafety bugs if used incorrectly! Do not expose!
# Not suitable for values that have a different lifetime from the virtual DOM!
# TODO: actually implement this for real! LOL
enableVdomAllocator : Bool -> Task {} *

# Switch back from the virtual DOM allocator to the "normal"
# allocator that is safe to use with long-lived values.
# At the same time, drop the entire "old" virtual DOM arena.
disableVdomAllocator : Task {} *
