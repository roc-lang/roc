use crate::builtins;
use crate::can::ident::TagName;
use crate::collections::{default_hasher, MutMap};
use crate::module::symbol::Symbol;
use crate::region::{Located, Region};
use crate::solve::{BuiltinAlias, SolvedType};
use crate::subs::VarId;
use std::collections::HashMap;

/// Keep this up to date by hand!
///
const NUM_BUILTIN_IMPORTS: usize = 7;

/// These can be shared between definitions, they will get instantiated when converted to Type
const TVAR1: VarId = VarId::from_u32(1);
const TVAR2: VarId = VarId::from_u32(2);

/// These can be shared between definitions, they will get instantiated when converted to Type
const FUVAR: VarId = VarId::from_u32(1000);
const UVAR1: VarId = VarId::from_u32(1001);
const UVAR2: VarId = VarId::from_u32(1002);
const UVAR3: VarId = VarId::from_u32(1003);
const UVAR4: VarId = VarId::from_u32(1004);

pub fn aliases() -> MutMap<Symbol, BuiltinAlias> {
    let mut aliases = builtins::aliases();

    let mut add_alias = |symbol, alias| {
        //        debug_assert!(
        //            !aliases.contains_key(&symbol),
        //            "Duplicate alias definition for {:?}",
        //            symbol
        //        );

        // TODO instead of using Region::zero for all of these,
        // instead use the Region where they were defined in their
        // source .roc files! This can give nicer error messages.
        aliases.insert(symbol, alias);
    };

    let single_private_tag = |symbol, targs| {
        SolvedType::TagUnion(
            vec![(TagName::Private(symbol), targs)],
            Box::new(SolvedType::EmptyTagUnion),
        )
    };

    // Attr u a : [ @Attr u a ]
    add_alias(
        Symbol::ATTR_ATTR,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![
                Located::at(Region::zero(), "u".into()),
                Located::at(Region::zero(), "a".into()),
            ],
            typ: single_private_tag(Symbol::ATTR_AT_ATTR, vec![flex(TVAR1), flex(TVAR2)]),
        },
    );

    // Int : Num Integer
    add_alias(
        Symbol::INT_INT,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: SolvedType::Apply(
                Symbol::NUM_NUM,
                vec![lift(
                    UVAR1,
                    SolvedType::Apply(Symbol::INT_INTEGER, Vec::new()),
                )],
            ),
        },
    );

    aliases
}

pub fn types() -> MutMap<Symbol, (SolvedType, Region)> {
    let mut types = HashMap::with_capacity_and_hasher(NUM_BUILTIN_IMPORTS, default_hasher());

    let mut add_type = |symbol, typ| {
        debug_assert!(
            !types.contains_key(&symbol),
            "Duplicate type definition for {:?}",
            symbol
        );

        // TODO instead of using Region::zero for all of these,
        // instead use the Region where they were defined in their
        // source .roc files! This can give nicer error messages.
        types.insert(symbol, (typ, Region::zero()));
    };

    // Num module

    // add or (+) : Attr u1 (Num a), Attr u2 (Num a) -> Attr u3 (Num a)
    add_type(
        Symbol::NUM_ADD,
        unique_function(
            vec![num_type(UVAR1, TVAR1), num_type(UVAR2, TVAR1)],
            num_type(UVAR3, TVAR1),
        ),
    );

    // sub or (-) : Num a, Num a -> Num a
    add_type(
        Symbol::NUM_SUB,
        unique_function(
            vec![num_type(UVAR1, TVAR1), num_type(UVAR2, TVAR1)],
            num_type(UVAR3, TVAR1),
        ),
    );

    // isLt or (<) : Num a, Num a -> Bool
    add_type(
        Symbol::NUM_LT,
        unique_function(
            vec![num_type(UVAR1, TVAR1), num_type(UVAR2, TVAR1)],
            bool_type(UVAR3),
        ),
    );

    // isLte or (<=) : Num a, Num a -> Bool
    add_type(
        Symbol::NUM_LE,
        unique_function(
            vec![num_type(UVAR1, TVAR1), num_type(UVAR2, TVAR1)],
            bool_type(UVAR3),
        ),
    );

    // Int module

    // highest : Int
    add_type(Symbol::INT_HIGHEST, int_type(UVAR1));

    // lowest : Int
    add_type(Symbol::INT_LOWEST, int_type(UVAR1));

    // Float module

    // div : Float, Float -> Float
    add_type(
        Symbol::FLOAT_DIV,
        SolvedType::Func(vec![float_type(), float_type()], Box::new(float_type())),
    );

    // highest : Float
    add_type(Symbol::FLOAT_HIGHEST, float_type());

    // lowest : Float
    add_type(Symbol::FLOAT_LOWEST, float_type());

    // Bool module

    // List module

    // isEmpty : Attr u (List *) -> Attr v Bool
    add_type(
        Symbol::LIST_ISEMPTY,
        unique_function(vec![list_type(UVAR1, TVAR1)], bool_type(UVAR2)),
    );

    // get : List a, Int -> Result a [ IndexOutOfBounds ]*
    let index_out_of_bounds = SolvedType::TagUnion(
        vec![(TagName::Global("IndexOutOfBounds".into()), vec![])],
        Box::new(SolvedType::Wildcard),
    );

    add_type(
        Symbol::LIST_GET,
        unique_function(
            vec![list_type(UVAR1, TVAR1), int_type(UVAR2)],
            result_type(UVAR3, flex(TVAR1), lift(UVAR4, index_out_of_bounds)),
        ),
    );

    // set : List a, Int, a -> List a
    add_type(
        Symbol::LIST_SET,
        unique_function(
            vec![list_type(UVAR1, TVAR1), int_type(UVAR2), flex(TVAR1)],
            list_type(UVAR3, TVAR1),
        ),
    );

    types
}

#[inline(always)]
fn flex(tvar: VarId) -> SolvedType {
    SolvedType::Flex(tvar)
}

#[inline(always)]
fn unique_function(args: Vec<SolvedType>, ret: SolvedType) -> SolvedType {
    SolvedType::Apply(
        Symbol::ATTR_ATTR,
        vec![flex(FUVAR), SolvedType::Func(args, Box::new(ret))],
    )
}

#[allow(dead_code)]
#[inline(always)]
fn attr_type(u: VarId, a: VarId) -> SolvedType {
    SolvedType::Apply(Symbol::ATTR_ATTR, vec![flex(u), flex(a)])
}

#[inline(always)]
fn lift(u: VarId, a: SolvedType) -> SolvedType {
    SolvedType::Apply(Symbol::ATTR_ATTR, vec![flex(u), a])
}

#[inline(always)]
fn float_type() -> SolvedType {
    SolvedType::Apply(Symbol::FLOAT_FLOAT, Vec::new())
}

#[inline(always)]
fn int_type(u: VarId) -> SolvedType {
    SolvedType::Apply(
        Symbol::ATTR_ATTR,
        vec![flex(u), SolvedType::Apply(Symbol::INT_INT, Vec::new())],
    )
}

#[inline(always)]
fn bool_type(u: VarId) -> SolvedType {
    SolvedType::Apply(
        Symbol::ATTR_ATTR,
        vec![flex(u), SolvedType::Apply(Symbol::BOOL_BOOL, Vec::new())],
    )
}

#[allow(dead_code)]
#[inline(always)]
fn str_type() -> SolvedType {
    SolvedType::Apply(Symbol::STR_STR, Vec::new())
}

#[inline(always)]
fn num_type(u: VarId, a: VarId) -> SolvedType {
    SolvedType::Apply(
        Symbol::ATTR_ATTR,
        vec![flex(u), SolvedType::Apply(Symbol::NUM_NUM, vec![flex(a)])],
    )
}

#[inline(always)]
fn result_type(u: VarId, a: SolvedType, e: SolvedType) -> SolvedType {
    SolvedType::Apply(
        Symbol::ATTR_ATTR,
        vec![
            flex(u),
            SolvedType::Apply(Symbol::RESULT_RESULT, vec![a, e]),
        ],
    )
}

#[inline(always)]
fn list_type(u: VarId, a: VarId) -> SolvedType {
    SolvedType::Apply(
        Symbol::ATTR_ATTR,
        vec![flex(u), SolvedType::Apply(Symbol::LIST_LIST, vec![flex(a)])],
    )
}
