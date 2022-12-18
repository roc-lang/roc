interface Html.Patch
    exposes [
        Patch,
    ]
    imports [
        Effect.{
            Effect,
            NodeId,
            EventHandlerId,
            TagName,
            AttrType,
            EventType,
        },
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
    SetListener NodeId EventType EventHandlerId,
    RemoveListener NodeId EventType,
]

apply : Patch -> Effect {}
apply = \patch ->
    when patch is
        CreateElement nodeId tagName -> Effect.createElement nodeId tagName
        CreateTextNode nodeId content -> Effect.createTextNode nodeId content
        UpdateTextNode content -> Effect.updateTextNode content
        AppendChild parentId childId -> Effect.appendChild parentId childId
        RemoveNode id -> Effect.removeNode id
        ReplaceNode oldId newId -> Effect.replaceNode oldId newId
        SetAttribute nodeId attrName value -> Effect.setAttribute nodeId attrName value
        RemoveAttribute nodeId attrName -> Effect.removeAttribute nodeId attrName
        SetProperty nodeId propName json -> Effect.setProperty nodeId propName json
        RemoveProperty nodeId propName -> Effect.removeProperty nodeId propName
        SetListener nodeId eventType handlerId -> Effect.setListener nodeId eventType handlerId
        RemoveListener nodeId eventType -> Effect.removeListener nodeId eventType
