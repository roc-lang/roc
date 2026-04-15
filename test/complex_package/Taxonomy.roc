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
                            new : Str, Str -> Species
                            new = |name, common_name|
                                { name, common_name }

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
    wolf = Taxonomy.Domain.Kingdom.Phylum.Class.Order.Species.new("Canis lupus", "Gray Wolf")
    Taxonomy.Domain.Kingdom.Phylum.Class.Order.Species.display(wolf) == "Canis lupus (Gray Wolf)"
}

expect {
    a = Taxonomy.Domain.Kingdom.Phylum.Class.Order.Species.new("Canis lupus", "Gray Wolf")
    b = Taxonomy.Domain.Kingdom.Phylum.Class.Order.Species.new("Canis lupus", "Gray Wolf")
    a == b
}

expect {
    a = Taxonomy.Domain.Kingdom.Phylum.Class.Order.Species.new("Canis lupus", "Gray Wolf")
    b = Taxonomy.Domain.Kingdom.Phylum.Class.Order.Species.new("Felis catus", "Domestic Cat")
    a != b
}
