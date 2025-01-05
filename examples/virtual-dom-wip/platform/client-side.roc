platform "client-side"
    requires {} { app : App state init_data }
    exposes []
    packages {}
    imports [
        Html.Internal.Shared.{ App },
        Html.Internal.Client.{ PlatformState, init_client_app, dispatch_event },
    ]
    provides [main]

# Fields sorted by alignment, then alphabetically
FromHost state init_data : {
    event_handler_id : U64,
    event_json_list : List (List U8),
    event_platform_state : Box (PlatformState state init_data),
    init_json : List U8,
    is_init_event : Bool,
}

# Fields sorted by alignment, then alphabetically
ToHost state init_data : {
    platform_state : Box (PlatformState state init_data),
    event_prevent_default : Bool,
    event_stop_propagation : Bool,
}

# TODO: naming the type variables causes a type 'mismatch'
# main : FromHost state initData -> Effect (ToHost state initData) where initData implements Decoding & Encoding
main : FromHost _ _ -> Task (ToHost _ _) *
main = \from_host ->
    if from_host.is_init_event then
        init_client_app(from_host.init_json, app)
        |> Task.map(\platform_state -> {
            platform_state: Box.box(platform_state),
            event_prevent_default: Bool.false,
            event_stop_propagation: Bool.false,
        })
    else
        dispatch_event(Box.unbox(from_host.event_platform_state), from_host.event_json_list, from_host.event_handler_id)
        |> Task.map(\js_event_result -> {
            platform_state: Box.box(js_event_result.platform_state),
            event_prevent_default: js_event_result.prevent_default,
            event_stop_propagation: js_event_result.stop_propagation,
        })
