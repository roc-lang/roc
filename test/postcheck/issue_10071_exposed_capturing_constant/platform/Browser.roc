import HostValue exposing [HostValue]
import Capability exposing [Capability]
import Node
import Signal exposing [Signal]

Browser := [].{
    Location : { path : Str, query : Str, hash : Str }

    decode_location_payload : List(U8) -> Location
    decode_location_payload = |_| { path: "", query: "", hash: "" }

    location : Signal(Location)
    location = {
        token : Box(U64)
        token = Node.new_token({})
        location_cap = Capability.new({})
        payload_cap = Capability.new({})

        from_payload : HostValue -> HostValue
        from_payload = |payload_hv| {
            payload_bytes : List(U8)
            payload_bytes = Box.unbox(Capability.take(payload_hv, payload_cap))
            Capability.store(Box.box(decode_location_payload(payload_bytes)), location_cap)
        }

        Signal.from_expr(
            Node.SignalExpr.LocationSource(
                token,
                Box.box(from_payload),
                Capability.handle(location_cap),
                Capability.handle(payload_cap),
            ),
            location_cap,
        )
    }
}
