## DRAFT of an API for editor plugins
interface Ir.Node exposes [] imports []

IntStyle :
    [
        Decimal,
        Hex,
        Octal,
        Binary,
    ]

IntVal :
    [
        I64Val I64,
        U64Val U64,
        I32Val I32,
        U32Val U32,
        I16Val I16,
        U16Val U16,
        I8Val I8,
        U8Val U8,
    ]

## This is opaque for two reasons:
## 1. It's a PoolStr under the hood, so we can't just give you a normal Str without breaking things
## 2. It's got special formatting rules, e.g. "abc" is not a valid Int string. So we want a Str -> Result IntStr [ InvalidInt ]* function anyway!
IntStr : [ @IntStr PoolStr ]

## This is opaque because it's a Variable under the hood.
## You can call something like Type.infer on it (name TBD)
## to get a tag union representing the actual type.
Type : [ @Var Variable ]

## This isn't currently how we represent this in
## Expr2 (we have separate variants for SmallStr and Str),
## but maybe we should change that. Having this be opaque
## means we can have just one Str variant inside Expr, and
## require doing mapping operations (or whatever) on this
## which can automatically determine whether it needs to
## be small or not. Also, at the same time, it can convert
## to and from a normal Str.
StrLiteral : [ @SmallStr SmallStr, @BigStr PoolStr ]

## Maybe this should be how it's represented in Expr2 as well.
Lookup : [ @ValidLookup Symbol, @InvalidLookup PoolStr ]


NodeList elem : [ @PoolVec (PoolVec elem) ]

## The goal of this is to have the exact same representation
## in memory as an Expr2 node, but to present things to a
## plugin author that they can't directly edit PoolStr etc.
## This way, if they want to do AST transformations, we can
## give them one of these Expr nodes and they can do whatever
## operations they want on it.
Expr :
    [
        SmallInt
            {
                num : IntVal,
                text : IntStr,
                type : Type,
                style : IntStyle
            },

        SmallInt
            {
                num : IntVal,
                text : IntStr,
                type : Type,
                style : IntStyle
            },

        Str StrLiteral,
        Lookup Lookup,

        List
            {
                elemType : Type,
                elems : NodeList Elems,
            }

        If
            {
                conditionType : Type, # Hopefully Bool, but could be a type mismatch!
                exprType : Type, # The entire if-expression

                # At runtime, the first element in this list
                # will have its condition evaluated. If it's
                # True, that element's ifTrue will be run.
                # Otherwise, we proceed to the next element
                # in the list and try it. (That's the first
                # "else if"). If all the conditions returned
                # False, evaluate finalElse.
                ifs :
                    NodeList
                        {
                            condition : Expr,
                            ifTrue : Expr,
                        }
                finalElse : Expr
            },

        When
            {
                conditionType : Type,
                exprType : Type,
                condition : Expr,
                branches :
                    NodeList
                        {
                            patterns : NodeList Pattern,
                            body : Expr,
                            guard : [ Guard Expr, None ]
                        }
            }

        # Defs - yikes! Now we have to expose LetRec vs LetValue vs LetFunction?
        # A concern about doing it that way: plugin authors
        # can definitely get the rules wrong, which would
        # be a disaster.
        # One idea: in Expr2 we could have plain old Defs,
        # but one of the fields in the Defs variant is
        # a pointer to a Let structure, which holds the
        # actual Let structure.
        #
        # We don't expose that field to plugin authors,
        # and whenever we let plugin authors change Defs,
        # we recompute the Let structure.
    ]
