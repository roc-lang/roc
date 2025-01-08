hosted PlatformTasks
    exposes [
        NodeId,
        HandlerId,
        TagName,
        AttrType,
        EventType,
        create_element,
        create_text_node,
        update_text_node,
        append_child,
        remove_node,
        replace_node,
        set_attribute,
        remove_attribute,
        set_property,
        remove_property,
        set_style,
        set_listener,
        remove_listener,
        enable_vdom_allocator,
        disable_vdom_allocator,
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
create_element : NodeId, TagName -> Task {} *

## createTextNode content
create_text_node : NodeId, Str -> Task {} *

## updateTextNode content
update_text_node : NodeId, Str -> Task {} *

## appendChild parentId childId
append_child : NodeId, NodeId -> Task {} *

## removeNode id
remove_node : NodeId -> Task {} *

## replaceNode oldId newId
replace_node : NodeId, NodeId -> Task {} *

## setAttribute nodeId attrName value
set_attribute : NodeId, AttrType, Str -> Task {} *

## removeAttribute nodeId attrName
remove_attribute : NodeId, AttrType -> Task {} *

## setProperty nodeId propName json
set_property : NodeId, Str, List U8 -> Task {} *

## removeProperty nodeId propName
remove_property : NodeId, Str -> Task {} *

## setStyle nodeId key value
set_style : NodeId, Str, Str -> Task {} *

## setListener nodeId eventType accessorsJson handlerId
set_listener : NodeId, EventType, List U8, HandlerId -> Task {} *

## removeListener nodeId handlerId
remove_listener : NodeId, HandlerId -> Task {} *

# Enable a special memory allocator for virtual DOM
# This consists of two arenas, "even" and "odd", which alternately hold the "old" and "new" VDOM.
# After we do a diff, the "old" virtual DOM can be dropped without checking refcounts.
# Danger: Could cause memory unsafety bugs if used incorrectly! Do not expose!
# Not suitable for values that have a different lifetime from the virtual DOM!
# TODO: actually implement this for real! LOL
enable_vdom_allocator : Bool -> Task {} *

# Switch back from the virtual DOM allocator to the "normal"
# allocator that is safe to use with long-lived values.
# At the same time, drop the entire "old" virtual DOM arena.
disable_vdom_allocator : Task {} *
