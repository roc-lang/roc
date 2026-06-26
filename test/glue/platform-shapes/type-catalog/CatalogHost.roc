import Catalog exposing [Catalog]

CatalogHost := [].{
    roundtrip! : Catalog.CatalogUnion => Catalog.CatalogUnion
}
