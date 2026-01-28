# Types for glue generation
# Contains type information extracted from the platform module

TypeId := U64

## Entry point information
EntryPoint : { name : Str, type_id : TypeId }

## Function information
FunctionInfo : { name : Str, type_str : Str }

## Hosted function information (includes global index)
HostedFunctionInfo : { index : U64, name : Str, type_str : Str }

## Module type information
ModuleTypeInfo : {
    functions : List(FunctionInfo),
    hosted_functions : List(HostedFunctionInfo),
    main_type : Str,
    name : Str,
}

## Types structure for glue generation
## This is a simplified version - will be expanded with full type info later
Types : {
    ## Entry points (platform requires)
    entrypoints : List(EntryPoint),
    ## Module type information
    modules : List(ModuleTypeInfo),
}
