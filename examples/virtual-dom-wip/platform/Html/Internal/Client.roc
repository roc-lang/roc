module [
    PlatformState,
    init_client_app,
    dispatch_event,
]

import PlatformTasks exposing [
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
    translate_static,
]
import Json
import Action

PlatformState state init_data : {
    app : App state init_data,
    state,
    rendered : RenderedTree state,
}

# The rendered tree uses indices rather than pointers
# This makes it easier to communicate with JS using integer indices.
# There is a JavaScript `nodes` array that matches the Roc `nodes` List
RenderedTree state : {
    root : NodeId,
    nodes : List (Result RenderedNode [DeletedNode]),
    deleted_node_cache : List NodeId,
    handlers : List (Result (Handler state) [DeletedHandler]),
    deleted_handler_cache : List HandlerId,
}

RenderedNode : [
    RenderedNone,
    RenderedText Str,
    RenderedElement Str RenderedAttributes (List NodeId),
]

RenderedAttributes : {
    event_listeners : Dict Str { accessors : List CyclicStructureAccessor, handler_id : HandlerId },
    html_attrs : Dict Str Str,
    dom_props : Dict Str (List U8),
    styles : Dict Str Str,
}

empty_rendered_attrs = {
    event_listeners: Dict.empty({}),
    html_attrs: Dict.empty({}),
    dom_props: Dict.empty({}),
    styles: Dict.empty({}),
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
init_client_app : List U8, App state init_data -> Task (PlatformState state init_data) * where init_data implements Decoding
init_client_app = \json, app ->
    # Initialise the Roc representation of the rendered DOM, and calculate patches (for event listeners)
    { state, rendered, patches } =
        init_client_app_help(json, app)

    # Call out to JS to patch the DOM, attaching the event listeners
    apply_patches!(patches)

    Task.ok({
        app,
        state,
        rendered,
    })

# Testable helper function to initialise the app
init_client_app_help : List U8, App state init_data -> { state, rendered : RenderedTree state, patches : List Patch } where init_data implements Decoding
init_client_app_help = \json, app ->
    state =
        json
        |> Decode.from_bytes(Json.json)
        |> app.init
    dynamic_view =
        app.render(state)
    static_unindexed =
        translate_static(dynamic_view)
    { nodes: static_nodes } =
        index_nodes({ nodes: [], sibling_ids: [] }, static_unindexed)
    static_rendered = {
        root: List.len(static_nodes) - 1,
        nodes: List.map(static_nodes, Ok),
        deleted_node_cache: [],
        handlers: [],
        deleted_handler_cache: [],
    }

    # Run our first diff. The only differences will be event listeners, so we will generate patches to attach those.
    { rendered, patches } =
        diff({ rendered: static_rendered, patches: [] }, dynamic_view)

    { state, rendered, patches }

# Assign an index to each (virtual) DOM node.
# In JavaScript, we maintain an array of references to real DOM nodes.
# In Roc, we maintain a matching List of virtual DOM nodes with the same indices.
# They are both initialised separately, but use the same indexing algorithm.
# (We *could* pass this data in as JSON from the HTML file, but it would roughly double the size of that HTML file!)
index_nodes : { nodes : List RenderedNode, sibling_ids : List U64 }, Html state -> { nodes : List RenderedNode, sibling_ids : List U64 }
index_nodes = \{ nodes, sibling_ids }, unrendered ->
    when unrendered is
        Text(content) ->
            {
                nodes: List.append(nodes, RenderedText(content)),
                sibling_ids: List.append(sibling_ids, List.len(nodes)),
            }

        Element(name, _, attrs, children) ->
            { nodes: list_with_children, sibling_ids: child_ids } =
                List.walk(children, { nodes, sibling_ids: [] }, index_nodes)
            rendered_attrs =
                List.walk(attrs, empty_rendered_attrs, \walked_attrs, attr ->
                    when attr is
                        EventListener(_, _, _) -> walked_attrs # Dropped! Server-rendered HTML has no listeners
                        HtmlAttr(k, v) -> { walked_attrs & html_attrs: Dict.insert(walked_attrs.html_attrs, k, v) }
                        DomProp(k, v) -> { walked_attrs & dom_props: Dict.insert(walked_attrs.dom_props, k, v) }
                        Style(k, v) -> { walked_attrs & styles: Dict.insert(walked_attrs.styles, k, v) })

            {
                nodes: List.append(list_with_children, RenderedElement(name, rendered_attrs, child_ids)),
                sibling_ids: List.append(sibling_ids, List.len(list_with_children)),
            }

        None ->
            {
                nodes: List.append(nodes, RenderedNone),
                sibling_ids: List.append(sibling_ids, List.len(nodes)),
            }

# -------------------------------
#   Patches
# -------------------------------
apply_patch : Patch -> Task {} *
apply_patch = \patch ->
    when patch is
        CreateElement(node_id, tag_name) -> PlatformTasks.create_element(node_id, tag_name)
        CreateTextNode(node_id, content) -> PlatformTasks.create_text_node(node_id, content)
        UpdateTextNode(node_id, content) -> PlatformTasks.update_text_node(node_id, content)
        AppendChild(parent_id, child_id) -> PlatformTasks.append_child(parent_id, child_id)
        RemoveNode(id) -> PlatformTasks.remove_node(id)
        ReplaceNode(old_id, new_id) -> PlatformTasks.replace_node(old_id, new_id)
        SetAttribute(node_id, attr_name, value) -> PlatformTasks.set_attribute(node_id, attr_name, value)
        RemoveAttribute(node_id, attr_name) -> PlatformTasks.remove_attribute(node_id, attr_name)
        SetProperty(node_id, prop_name, json) -> PlatformTasks.set_property(node_id, prop_name, json)
        RemoveProperty(node_id, prop_name) -> PlatformTasks.remove_property(node_id, prop_name)
        SetStyle(node_id, key, value) -> PlatformTasks.set_style(node_id, key, value)
        SetListener(node_id, event_type, accessors_json, handler_id) -> PlatformTasks.set_listener(node_id, event_type, accessors_json, handler_id)
        RemoveListener(node_id, handler_id) -> PlatformTasks.remove_listener(node_id, handler_id)

apply_patches : List Patch -> Task {} *
apply_patches = \patches ->
    List.walk(patches, Task.ok({}), \previous_effects, patch ->
        previous_effects!
        apply_patch(patch))

# -------------------------------
#   EVENT HANDLING
# -------------------------------
JsEventResult state init_data : {
    platform_state : PlatformState state init_data,
    stop_propagation : Bool,
    prevent_default : Bool,
}

## Dispatch a JavaScript event to a Roc handler, given the handler ID and some JSON event data.
dispatch_event : PlatformState state init_data, List (List U8), HandlerId -> Task (JsEventResult state init_data) * where init_data implements Decoding
dispatch_event = \platform_state, event_data, handler_id ->
    { app, state, rendered } =
        platform_state
    maybe_handler =
        List.get(rendered.handlers, handler_id)
        |> Result.with_default(Err(DeletedHandler))
    { action, stop_propagation, prevent_default } =
        when maybe_handler is
            Err(DeletedHandler) ->
                { action: Action.none, stop_propagation: Bool.false, prevent_default: Bool.false }

            Ok(Normal(handler)) ->
                { action: handler(state, event_data), stop_propagation: Bool.false, prevent_default: Bool.false }

            Ok(Custom(handler)) ->
                handler(state, event_data)

    when action is
        Update(new_state) ->
            new_view_unrendered =
                app.render(new_state)
            { rendered: new_rendered, patches } =
                diff({ rendered, patches: [] }, new_view_unrendered)

            apply_patches!(patches)
            Task.ok({
                platform_state: {
                    app,
                    state: new_state,
                    rendered: new_rendered,
                },
                stop_propagation,
                prevent_default,
            })

        None ->
            Task.ok({ platform_state, stop_propagation, prevent_default })

# -------------------------------
#   DIFF
# -------------------------------
diff : DiffState state, Html state -> DiffState state
diff = \{ rendered, patches }, new_node ->
    root =
        rendered.root
    old_node =
        List.get(rendered.nodes, root)
        |> Result.with_default(Ok(RenderedNone))
        |> Result.with_default(RenderedNone)

    when { old_node, new_node } is
        { old_node: RenderedText(old_content), new_node: Text(new_content) } ->
            if new_content != old_content then
                new_nodes =
                    List.set(rendered.nodes, rendered.root, Ok(RenderedText(new_content)))

                {
                    rendered: { rendered &
                        nodes: new_nodes,
                    },
                    patches: List.append(patches, UpdateTextNode(rendered.root, new_content)),
                }
            else
                { rendered, patches }

        { old_node: RenderedElement(old_name, old_attrs, old_children), new_node: Element(new_name, _, new_attrs, new_children) } ->
            if new_name != old_name then
                replace_node({ rendered, patches }, root, new_node)
            else
                state_attrs =
                    diff_attrs({ rendered, patches }, root, old_attrs, new_attrs)
                state_child_pairs =
                    List.map2(old_children, new_children, \old_child_id, new_child -> { old_child_id, new_child })
                    |> List.walk(state_attrs, \child_walk_state, { old_child_id, new_child } ->
                        { rendered: child_walk_rendered, patches: child_walk_patches } = child_walk_state
                        diff({ rendered: { child_walk_rendered & root: old_child_id }, patches: child_walk_patches }, new_child))
                { rendered: rendered_left_over_children, patches: patches_left_over_children } =
                    if List.len(old_children) > List.len(new_children) then
                        List.walk_from(old_children, List.len(new_children), state_child_pairs, delete_node)
                    else if List.len(old_children) < List.len(new_children) then
                        state_before_create = {
                            rendered: state_child_pairs.rendered,
                            patches: state_child_pairs.patches,
                            ids: [],
                        }
                        { rendered: rendered_after_create, patches: patches_after_create, ids: created_ids } =
                            List.walk_from(new_children, List.len(old_children), state_before_create, create_child_node)
                        # Look up the children again since they might have new node IDs!
                        node_with_updated_children =
                            when List.get(rendered_after_create.nodes, root) is
                                Ok(Ok(RenderedElement(n, a, c))) -> RenderedElement(n, a, List.concat(c, created_ids))
                                _ -> crash("Bug in virtual-dom framework: nodeWithUpdatedChildren not found")
                        updated_nodes =
                            List.set(rendered_after_create.nodes, root, Ok(node_with_updated_children))

                        {
                            rendered: { rendered_after_create & nodes: updated_nodes },
                            patches: List.walk(created_ids, patches_after_create, \p, id -> List.append(p, AppendChild(root, id))),
                        }
                    else
                        state_child_pairs

                {
                    rendered: { rendered_left_over_children & root },
                    patches: patches_left_over_children,
                }

        { old_node: RenderedNone, new_node: None } ->
            { rendered, patches }

        _ ->
            # old node has been replaced with a totally different variant. There's no point in diffing, just replace.
            replace_node({ rendered, patches }, rendered.root, new_node)

replace_node : DiffState state, NodeId, Html state -> DiffState state
replace_node = \diff_state, old_node_id, new_node ->
    { rendered: create_rendered, patches: create_patches, id: create_node_id } =
        create_node(diff_state, new_node)
    pre_delete_state = {
        rendered: create_rendered,
        patches: List.append(create_patches, ReplaceNode(old_node_id, create_node_id)),
    }

    delete_node(pre_delete_state, old_node_id)

# Delete a node, and drop any JS references to its children and event listeners
# TODO: see if it would speed things up to leave this junk lying around until the slot is reused.
# Any danger of spurious events being sent to the wrong handler?
# Otherwise, can we sweep everything at once at the end of the diff?
# Let's be conservative on things like this until we have more test cases working.
delete_node : DiffState state, NodeId -> DiffState state
delete_node = \diff_state, id ->
    { rendered, patches } =
        when List.get(diff_state.rendered.nodes, id) is
            Ok(node) ->
                when node is
                    Ok(RenderedElement(_, _, children)) ->
                        List.walk(children, diff_state, delete_node)

                    _ -> diff_state

            _ -> diff_state

    patches_remove_listeners =
        when List.get(rendered.nodes, id) is
            Ok(Ok(RenderedElement(_, attrs, _))) ->
                Dict.walk(attrs.event_listeners, patches, \p, _, { handler_id } ->
                    List.append(p, RemoveListener(id, handler_id)))

            _ -> patches

    new_nodes =
        List.set(rendered.nodes, id, Err(DeletedNode))
    new_deleted_node_cache =
        List.append(rendered.deleted_node_cache, id)
    new_patches =
        List.append(patches_remove_listeners, RemoveNode(id))

    {
        rendered: { rendered &
            nodes: new_nodes,
            deleted_node_cache: new_deleted_node_cache,
        },
        patches: new_patches,
    }

create_node : DiffState state, Html state -> { rendered : RenderedTree state, patches : List Patch, id : NodeId }
create_node = \{ rendered, patches }, new_node ->
    when new_node is
        Text(content) ->
            { rendered: new_rendered, id } =
                insert_node(rendered, RenderedText(content))

            {
                rendered: new_rendered,
                patches: List.append(patches, CreateTextNode(id, content)),
                id,
            }

        None ->
            { rendered: new_rendered, id } =
                insert_node(rendered, RenderedNone)

            { rendered: new_rendered, patches, id }

        Element(tag_name, _, attrs, children) ->
            { rendered: rendered_with_children, patches: patches_with_children, ids: child_ids } =
                List.walk(children, { rendered, patches, ids: [] }, create_child_node)
            node_id =
                next_node_id(rendered_with_children)
            patches_with_elem =
                List.append(patches_with_children, CreateElement(node_id, tag_name))
            { rendered_attrs, rendered: rendered_with_attrs, patches: patches_with_attrs } =
                render_attrs(attrs, rendered_with_children, patches_with_elem, node_id)
            { rendered: rendered_with_node } =
                insert_node(rendered_with_attrs, RenderedElement(tag_name, rendered_attrs, child_ids))

            {
                rendered: rendered_with_node,
                patches: patches_with_attrs,
                id: node_id,
            }

AttrDiffState state : {
    node_id : NodeId,
    attrs : RenderedAttributes,
    patches : List Patch,
    handlers : List (Result (Handler state) [DeletedHandler]),
    deleted_handler_cache : List HandlerId,
}

diff_attrs : DiffState state, NodeId, RenderedAttributes, List (Attribute state) -> DiffState state
diff_attrs = \{ rendered, patches }, node_id, attrs, new_attrs ->
    init_state = {
        node_id,
        attrs,
        patches,
        handlers: rendered.handlers,
        deleted_handler_cache: rendered.deleted_handler_cache,
    }
    final_state =
        List.walk(new_attrs, init_state, diff_attr)
    new_rendered =
        { rendered &
            handlers: final_state.handlers,
            deleted_handler_cache: final_state.deleted_handler_cache,
        }

    {
        rendered: new_rendered,
        patches: final_state.patches,
    }

diff_attr : AttrDiffState state, Attribute state -> AttrDiffState state
diff_attr = \{ node_id, attrs, patches, handlers, deleted_handler_cache }, attr ->
    when attr is
        EventListener(event_name, new_accessors, new_handler) ->
            when Dict.get(attrs.event_listeners, event_name) is
                Ok({ accessors, handler_id }) ->
                    Tuple(new_attrs, new_patches) =
                        if accessors == new_accessors then
                            Tuple(attrs, patches)
                        else
                            json = new_accessors |> Encode.to_bytes(Json.json)

                            Tuple(
                                { attrs & event_listeners: Dict.insert(attrs.event_listeners, event_name, { accessors, handler_id }) },
                                (
                                    patches
                                    |> List.append(RemoveListener(node_id, handler_id))
                                    |> List.append(SetListener(node_id, event_name, json, handler_id))
                                ),
                            )

                    {
                        node_id,
                        attrs: new_attrs,
                        patches: new_patches,
                        handlers: List.set(handlers, handler_id, Ok(new_handler)),
                        deleted_handler_cache,
                    }

                Err(KeyNotFound) ->
                    render_attr({ node_id, attrs, patches, handlers, deleted_handler_cache }, attr)

        HtmlAttr(k, v) ->
            when Dict.get(attrs.html_attrs, k) is
                Ok(old_val) ->
                    Tuple(new_attrs, new_patches) =
                        if old_val == v then
                            Tuple(attrs, patches)
                        else
                            Tuple(
                                { attrs & html_attrs: Dict.insert(attrs.html_attrs, k, v) },
                                (patches |> List.append(SetAttribute(node_id, k, v))),
                            )
                    {
                        node_id,
                        attrs: new_attrs,
                        patches: new_patches,
                        handlers,
                        deleted_handler_cache,
                    }

                Err(KeyNotFound) ->
                    render_attr({ node_id, attrs, patches, handlers, deleted_handler_cache }, attr)

        DomProp(k, v) ->
            when Dict.get(attrs.dom_props, k) is
                Ok(old_val) ->
                    Tuple(new_attrs, new_patches) =
                        if old_val == v then
                            Tuple(attrs, patches)
                        else
                            Tuple(
                                { attrs & dom_props: Dict.insert(attrs.dom_props, k, v) },
                                (patches |> List.append(SetProperty(node_id, k, v))),
                            )
                    {
                        node_id,
                        attrs: new_attrs,
                        patches: new_patches,
                        handlers,
                        deleted_handler_cache,
                    }

                Err(KeyNotFound) ->
                    render_attr({ node_id, attrs, patches, handlers, deleted_handler_cache }, attr)

        Style(k, v) ->
            when Dict.get(attrs.styles, k) is
                Ok(old_val) ->
                    Tuple(new_attrs, new_patches) =
                        if old_val == v then
                            Tuple(attrs, patches)
                        else
                            Tuple(
                                { attrs & styles: Dict.insert(attrs.styles, k, v) },
                                (patches |> List.append(SetStyle(node_id, k, v))),
                            )
                    {
                        node_id,
                        attrs: new_attrs,
                        patches: new_patches,
                        handlers,
                        deleted_handler_cache,
                    }

                Err(KeyNotFound) ->
                    render_attr({ node_id, attrs, patches, handlers, deleted_handler_cache }, attr)

render_attrs : List (Attribute state), RenderedTree state, List Patch, NodeId -> { rendered_attrs : RenderedAttributes, rendered : RenderedTree state, patches : List Patch }
render_attrs = \attrs, rendered, patches, node_id ->
    init_state = {
        node_id,
        attrs: empty_rendered_attrs,
        patches,
        handlers: rendered.handlers,
        deleted_handler_cache: rendered.deleted_handler_cache,
    }
    final_state =
        List.walk(attrs, init_state, render_attr)

    {
        rendered_attrs: final_state.attrs,
        rendered: { rendered &
            handlers: final_state.handlers,
            deleted_handler_cache: final_state.deleted_handler_cache,
        },
        patches: final_state.patches,
    }

render_attr : AttrDiffState state, Attribute state -> AttrDiffState state
render_attr = \{ node_id, attrs, patches, handlers, deleted_handler_cache }, attr ->
    when attr is
        HtmlAttr(k, v) ->
            {
                node_id,
                handlers,
                deleted_handler_cache,
                attrs: { attrs & html_attrs: Dict.insert(attrs.html_attrs, k, v) },
                patches: List.append(patches, SetAttribute(node_id, k, v)),
            }

        DomProp(k, v) ->
            {
                node_id,
                handlers,
                deleted_handler_cache,
                attrs: { attrs & dom_props: Dict.insert(attrs.dom_props, k, v) },
                patches: List.append(patches, SetProperty(node_id, k, v)),
            }

        Style(k, v) ->
            {
                node_id,
                handlers,
                deleted_handler_cache,
                attrs: { attrs & styles: Dict.insert(attrs.styles, k, v) },
                patches: List.append(patches, SetStyle(node_id, k, v)),
            }

        EventListener(event_type, accessors, handler) ->
            { handler_id, new_handlers, new_deleted_handler_cache } =
                when List.last(deleted_handler_cache) is
                    Ok(id) ->
                        {
                            handler_id: id,
                            new_handlers: List.set(handlers, id, Ok(handler)),
                            new_deleted_handler_cache: List.drop_last(deleted_handler_cache, 1),
                        }

                    Err(_) ->
                        {
                            handler_id: List.len(handlers),
                            new_handlers: List.append(handlers, Ok(handler)),
                            new_deleted_handler_cache: deleted_handler_cache,
                        }
            accessors_json =
                accessors |> Encode.to_bytes(Json.json)
            patch =
                SetListener(node_id, event_type, accessors_json, handler_id)

            {
                node_id,
                attrs: { attrs & event_listeners: Dict.insert(attrs.event_listeners, event_type, { accessors, handler_id }) },
                handlers: new_handlers,
                deleted_handler_cache: new_deleted_handler_cache,
                patches: List.append(patches, patch),
            }

create_child_node :
    { rendered : RenderedTree state, patches : List Patch, ids : List NodeId },
    Html state
    -> { rendered : RenderedTree state, patches : List Patch, ids : List NodeId }
create_child_node = \{ rendered, patches, ids }, child_html ->
    { rendered: rendered_child, patches: child_patches, id } =
        create_node({ rendered, patches }, child_html)

    {
        rendered: rendered_child,
        patches: child_patches,
        ids: List.append(ids, id),
    }

# insert a node into the nodes list, assigning it a NodeId
insert_node : RenderedTree state, RenderedNode -> { rendered : RenderedTree state, id : NodeId }
insert_node = \rendered, node ->
    when List.last(rendered.deleted_node_cache) is
        Ok(id) ->
            new_rendered =
                { rendered &
                    nodes: List.set(rendered.nodes, id, Ok(node)),
                    deleted_node_cache: List.drop_last(rendered.deleted_node_cache, 1),
                }

            { rendered: new_rendered, id }

        Err(_) ->
            new_rendered =
                { rendered &
                    nodes: List.append(rendered.nodes, Ok(node)),
                }

            { rendered: new_rendered, id: List.len(rendered.nodes) }

# Predict what NodeId will be assigned next, without actually assigning it
next_node_id : RenderedTree state -> NodeId
next_node_id = \rendered ->
    when List.last(rendered.deleted_node_cache) is
        Ok(id) -> id
        Err(_) -> List.len(rendered.nodes)

# -------------------------------
#   TESTS
# -------------------------------
eq_rendered_tree : RenderedTree state, RenderedTree state -> Bool
eq_rendered_tree = \a, b ->
    (a.root == b.root)
    && (a.nodes == b.nodes)
    && (List.len(a.handlers) == List.len(b.handlers))
    && (a.deleted_node_cache == b.deleted_node_cache)
    && (a.deleted_handler_cache == b.deleted_handler_cache)

# indexNodes
expect
    html : Html {}
    html =
        Element("a", 43, [HtmlAttr("href", "https://www.roc-lang.org/")], [Text("Roc")])

    actual : { nodes : List RenderedNode, sibling_ids : List U64 }
    actual =
        index_nodes({ nodes: [], sibling_ids: [] }, html)

    expected : { nodes : List RenderedNode, sibling_ids : List U64 }
    expected = {
        nodes: [
            RenderedText("Roc"),
            RenderedElement("a", { empty_rendered_attrs & html_attrs: Dict.from_list([("href", "https://www.roc-lang.org/")]) }, [0]),
        ],
        sibling_ids: [1],
    }

    (actual.nodes == expected.nodes)
    && (actual.sibling_ids == expected.sibling_ids)

# diff
expect
    State : { answer : U32 }

    diff_state_before : DiffState State
    diff_state_before = {
        rendered: {
            root: 4,
            nodes: [
                Ok(RenderedText("The app")),
                Ok(RenderedElement("h1", empty_rendered_attrs, [0])),
                Ok(RenderedText("The answer is 42")),
                Ok(RenderedElement("div", empty_rendered_attrs, [2])),
                Ok(RenderedElement("body", empty_rendered_attrs, [1, 3])),
            ],
            deleted_node_cache: [],
            handlers: [],
            deleted_handler_cache: [],
        },
        patches: [],
    }

    # Sizes don't matter, use zero. We are not creating a HTML string so we don't care what size it would be.
    new_node : Html State
    new_node =
        Element("body", 0, [], [
            Element("h1", 0, [], [Text("The app")]),
            Element("div", 0, [], [Text("The answer is 111")]),
        ])

    expected : DiffState State
    expected = {
        rendered: {
            root: 4,
            nodes: [
                Ok(RenderedText("The app")),
                Ok(RenderedElement("h1", empty_rendered_attrs, [0])),
                Ok(RenderedText("The answer is 111")),
                Ok(RenderedElement("div", empty_rendered_attrs, [2])),
                Ok(RenderedElement("body", empty_rendered_attrs, [1, 3])),
            ],
            deleted_node_cache: [],
            handlers: [],
            deleted_handler_cache: [],
        },
        patches: [UpdateTextNode(2, "The answer is 111")],
    }

    actual : DiffState State
    actual =
        diff(diff_state_before, new_node)

    (actual.patches == expected.patches)
    && eq_rendered_tree(actual.rendered, expected.rendered)

# initClientAppHelp
expect
    State : { answer : U32 }

    init = \result ->
        when result is
            Ok(state) -> state
            Err(_) -> { answer: 0 }

    on_click_handler : Handler State
    on_click_handler =
        Normal(\state, _ -> Action.update({ answer: state.answer + 1 }))

    render : State -> Html State
    render = \state ->
        num = Num.to_str(state.answer)

        on_click_attr : Attribute State
        on_click_attr =
            EventListener("click", [], on_click_handler)

        # Sizes don't matter, use zero. We are not creating a HTML string so we don't care what size it would be.
        Element("body", 0, [], [
            Element("h1", 0, [], [Text("The app")]),
            Element("div", 0, [on_click_attr], [Text("The answer is $(num)")]),
        ])

    app : App State State
    app = {
        init,
        render,
        wasm_url: "assets/test.wasm",
    }

    init_json : List U8
    init_json =
        { answer: 42 } |> Encode.to_bytes(Json.json) # panics at mono/src/ir.rs:5739:56
    expected : { state : State, rendered : RenderedTree State, patches : List Patch }
    expected = {
        state: { answer: 42 },
        rendered: {
            root: 4,
            nodes: [
                Ok(RenderedText("The app")),
                Ok(RenderedElement("h1", empty_rendered_attrs, [0])),
                Ok(RenderedText("The answer is 42")),
                Ok(RenderedElement("div", { empty_rendered_attrs & event_listeners: Dict.from_list([("click", { accessors: [], handler_id: 0 })]) }, [2])),
                Ok(RenderedElement("body", empty_rendered_attrs, [1, 3])),
            ],
            deleted_node_cache: [],
            handlers: [Ok(on_click_handler)],
            deleted_handler_cache: [],
        },
        patches: [SetListener(3, "click", [], 0)],
    }

    actual : { state : State, rendered : RenderedTree State, patches : List Patch }
    actual =
        init_client_app_help(init_json, app)

    (actual.state == expected.state)
    && eq_rendered_tree(actual.rendered, expected.rendered)
    && (actual.patches == expected.patches)
