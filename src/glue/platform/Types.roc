# Types for glue generation
# Contains type information extracted from the platform module

TypeId := U64

## Entry point information
EntryPoint : { name : Str, type_id : TypeId }

## Types structure for glue generation
## This is a simplified version - will be expanded with full type info later
Types : {
    ## Entry points (platform requires)
    entrypoints : List(EntryPoint),
}
