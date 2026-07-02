import Catalog exposing [Catalog]

CatalogHost := [].{
    roundtrip! : Catalog.CatalogUnion => Catalog.CatalogUnion
    single_payload_roundtrip! : Catalog.SinglePayload => Catalog.SinglePayload
    single_no_payload! : {} => Catalog.SingleNoPayload
}
