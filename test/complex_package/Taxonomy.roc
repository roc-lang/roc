## Taxonomy module demonstrating 7-level deep nested modules.
## Biological classification hierarchy:
##   Taxonomy > Domain > Kingdom > Phylum > Class > Order > Species

Taxonomy :: [].{
    Domain :: [].{
        Kingdom :: [].{
            Phylum :: [].{
                Class :: [].{
                    Order :: [].{
                        Species := {
                            name : Str,
                            common_name : Str,
                        }.{
                            display : Species -> Str
                            display = |species|
                                "${species.name} (${species.common_name})"

                            is_eq : Species, Species -> Bool
                        }
                    }
                }
            }
        }
    }
}

expect {
    wolf = { name: "Canis lupus", common_name: "Gray Wolf" }
    Taxonomy.Domain.Kingdom.Phylum.Class.Order.Species.display(wolf) == "Canis lupus (Gray Wolf)"
}

expect {
    a = { name: "Canis lupus", common_name: "Gray Wolf" }
    b = { name: "Canis lupus", common_name: "Gray Wolf" }
    a == b
}

expect {
    a = { name: "Canis lupus", common_name: "Gray Wolf" }
    b = { name: "Felis catus", common_name: "Domestic Cat" }
    a != b
}
