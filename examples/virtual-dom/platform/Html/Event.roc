interface Html.Event
    exposes [CyclicStructureAccessor, onClick]
    imports []

CyclicStructureAccessor : [
    ObjectField Str CyclicStructureAccessor,
    ArrayIndex Nat CyclicStructureAccessor,
    SerializableValue,
]

# TODO
onClick : List CyclicStructureAccessor, (state, List (List U8) -> Action state) -> Handler state
