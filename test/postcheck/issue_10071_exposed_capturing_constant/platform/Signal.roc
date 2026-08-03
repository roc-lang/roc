import HostValue exposing [HostValue]
import Capability exposing [Capability]
import Node

Signal(a) := { expr : Box(Node.SignalExpr), cap : Capability(a) }.{
    from_expr : Node.SignalExpr, Capability(a) -> Signal(a)
    from_expr = |expr, cap| { expr: Box.box(expr), cap }

    map : Signal(a), (a -> b) -> Signal(b)
        where [
            b.is_eq : b, b -> Bool,
        ]
    map = |signal, f| {
        token : Box(U64)
        token = Node.new_token({})
        output_cap = Capability.new({})

        wrapped : HostValue -> HostValue
        wrapped = |input_hv| {
            typed_input : a
            typed_input = Box.unbox(Capability.get(input_hv, signal.cap))
            typed_output : b
            typed_output = f(typed_input)
            Capability.store(Box.box(typed_output), output_cap)
        }

        {
            expr: Box.box(
                Node.SignalExpr.Map(
                    token,
                    signal.expr,
                    Box.box(wrapped),
                    Capability.handle(output_cap),
                ),
            ),
            cap: output_cap,
        }
    }
}
