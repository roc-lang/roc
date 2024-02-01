platform "client-side"
    requires {} { app : App state initData }
    exposes []
    packages {}
    provides [main]

import HtmlInternalShared exposing [App]
import HtmlInternalClient exposing [PlatformState, initClientApp, dispatchEvent]
import Effect exposing [Effect]

# Fields sorted by alignment, then alphabetically
FromHost state initData : {
    eventHandlerId : Nat,
    eventJsonList : List (List U8),
    eventPlatformState : Box (PlatformState state initData),
    initJson : List U8,
    isInitEvent : Bool,
}

# Fields sorted by alignment, then alphabetically
ToHost state initData : {
    platformState : Box (PlatformState state initData),
    eventPreventDefault : Bool,
    eventStopPropagation : Bool,
}

# TODO: naming the type variables causes a type 'mismatch'
# main : FromHost state initData -> Effect (ToHost state initData) where initData implements Decoding & Encoding
main : FromHost _ _ -> Effect (ToHost _ _)
main = \fromHost ->
    if fromHost.isInitEvent then
        initClientApp fromHost.initJson app
        |> Effect.map \platformState -> {
            platformState: Box.box platformState,
            eventPreventDefault: Bool.false,
            eventStopPropagation: Bool.false,
        }
    else
        dispatchEvent (Box.unbox fromHost.eventPlatformState) fromHost.eventJsonList fromHost.eventHandlerId
        |> Effect.map \jsEventResult -> {
            platformState: Box.box jsEventResult.platformState,
            eventPreventDefault: jsEventResult.preventDefault,
            eventStopPropagation: jsEventResult.stopPropagation,
        }
