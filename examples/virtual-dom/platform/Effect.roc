hosted Effect
    exposes [
        Effect,
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
    ]
    imports []
    generates Effect with [always, map]

NodeId := Nat
nodeId = \id -> @NodeId id

EventHandlerId := Nat
eventHandlerId = \id -> @EventHandlerId id

## createElement tagName
createElement : Str -> Effect NodeId

## createTextNode content
createTextNode : Str -> Effect NodeId

## appendChild parentId childId
appendChild : NodeId, NodeId -> Effect {}

## removeNode id
removeNode : NodeId -> Effect {}

## setAttribute nodeId attrName value
setAttribute : NodeId, Str, Str -> Effect {}

## removeAttribute nodeId attrName
removeAttribute : NodeId, Str -> Effect {}

## setProperty nodeId propName json
setProperty : NodeId, Str, List U8 -> Effect {}

## removeProperty nodeId propName
removeProperty : NodeId, Str -> Effect {}

## setListener nodeId eventType handlerId
setListener : NodeId, Str, EventHandlerId -> Effect {}

## removeListener nodeId eventType
removeListener : NodeId, Str -> Effect {}
