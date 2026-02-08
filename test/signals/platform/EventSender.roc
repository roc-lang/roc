import EventNode exposing [EventNode]

## EventSender is the write-only handle for firing events.
## Uses record wrapper pattern for structural lifting to work.
EventSender(a) := { node : EventNode }.{
    ## Get the inner EventNode (for platform use)
    to_node : EventSender(a) -> EventNode
    to_node = |sender| sender.node
}
