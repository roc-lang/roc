use crate::builtins;
use crate::builtins::StdLib;
use crate::can::ident::TagName;
use crate::collections::{default_hasher, MutMap, MutSet};
use crate::module::symbol::Symbol;
use crate::region::{Located, Region};
use crate::solve::{BuiltinAlias, SolvedAtom, SolvedType};
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
const UVAR5: VarId = VarId::from_u32(1005);

fn shared() -> SolvedType {
    SolvedType::Boolean(SolvedAtom::Zero, vec![])
}

fn boolean(b: VarId) -> SolvedType {
    SolvedType::Boolean(SolvedAtom::Variable(b), vec![])
}

fn disjunction(free: VarId, rest: Vec<VarId>) -> SolvedType {
    let solved_rest = rest.into_iter().map(SolvedAtom::Variable).collect();

    SolvedType::Boolean(SolvedAtom::Variable(free), solved_rest)
}

pub fn uniqueness_stdlib() -> StdLib {
    use builtins::Mode;

    let types = types();
    let aliases = aliases();

    debug_assert!({
        let normal_types: MutSet<Symbol> = builtins::types().keys().copied().collect();
        let normal_aliases: MutSet<Symbol> = builtins::aliases().keys().copied().collect();

        let unique_types = types.keys().copied().collect();
        let unique_aliases = aliases.keys().copied().collect();

        let missing_unique_types: MutSet<Symbol> =
            normal_types.difference(&unique_types).copied().collect();
        let missing_normal_types: MutSet<Symbol> =
            unique_types.difference(&normal_types).copied().collect();

        let missing_unique_aliases: MutSet<Symbol> = normal_aliases
            .difference(&unique_aliases)
            .copied()
            .collect();
        let missing_normal_aliases: MutSet<Symbol> = unique_aliases
            .difference(&normal_aliases)
            .copied()
            .collect();

        let cond = missing_normal_types.is_empty()
            && missing_unique_types.is_empty()
            && missing_normal_aliases.is_empty()
            && missing_unique_aliases.is_empty();

        dbg!(
            missing_normal_types,
            missing_unique_types,
            missing_normal_aliases,
            missing_unique_aliases
        );

        cond
    });

    StdLib {
        mode: Mode::Uniqueness,
        types,
        aliases,
    }
}

pub fn aliases() -> MutMap<Symbol, BuiltinAlias> {
    // let mut aliases = builtins::aliases();
    let mut aliases = MutMap::default();

    let mut add_alias = |symbol, alias| {
        debug_assert!(
            !aliases.contains_key(&symbol),
            "Duplicate alias definition for {:?}",
            symbol
        );

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

    // Num : Num Integer
    add_alias(
        Symbol::NUM_NUM,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![Located::at(Region::zero(), "a".into())],
            typ: single_private_tag(Symbol::NUM_AT_NUM, vec![flex(TVAR1)]),
        },
    );

    // Integer : [ @Integer ]
    add_alias(
        Symbol::INT_INTEGER,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: single_private_tag(Symbol::INT_AT_INTEGER, Vec::new()),
        },
    );

    // FloatingPoint : [ @FloatingPoint ]
    add_alias(
        Symbol::FLOAT_FLOATINGPOINT,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: single_private_tag(Symbol::FLOAT_AT_FLOATINGPOINT, Vec::new()),
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

    // Float : Num FloatingPoint
    add_alias(
        Symbol::FLOAT_FLOAT,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: SolvedType::Apply(
                Symbol::NUM_NUM,
                vec![lift(
                    UVAR1,
                    SolvedType::Apply(Symbol::FLOAT_FLOATINGPOINT, Vec::new()),
                )],
            ),
        },
    );

    // List a : [ @List a ]
    add_alias(
        Symbol::LIST_LIST,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![Located::at(Region::zero(), "elem".into())],
            typ: single_private_tag(Symbol::LIST_AT_LIST, vec![flex(TVAR1)]),
        },
    );

    // Result a e : [ Ok a, Err e ]
    add_alias(
        Symbol::RESULT_RESULT,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![
                Located::at(Region::zero(), "a".into()),
                Located::at(Region::zero(), "e".into()),
            ],
            typ: SolvedType::TagUnion(
                vec![
                    (TagName::Global("Ok".into()), vec![flex(TVAR1)]),
                    (TagName::Global("Err".into()), vec![flex(TVAR2)]),
                ],
                Box::new(SolvedType::EmptyTagUnion),
            ),
        },
    );

    // Str : [ @Str ]
    add_alias(
        Symbol::STR_STR,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: single_private_tag(Symbol::STR_AT_STR, Vec::new()),
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

    add_type(
        Symbol::INT_DIV,
        unique_function(vec![int_type(UVAR1), int_type(UVAR2)], int_type(UVAR3)),
    );

    // Float module

    // div : Float, Float -> Float
    add_type(
        Symbol::FLOAT_DIV,
        unique_function(
            vec![float_type(UVAR1), float_type(UVAR2)],
            float_type(UVAR3),
        ),
    );

    // highest : Float
    add_type(Symbol::FLOAT_HIGHEST, float_type(UVAR1));

    // lowest : Float
    add_type(Symbol::FLOAT_LOWEST, float_type(UVAR1));

    // Bool module

    // List module

    // isEmpty : Attr u (List *) -> Attr v Bool
    add_type(
        Symbol::LIST_ISEMPTY,
        unique_function(vec![list_type(UVAR1, TVAR1)], bool_type(UVAR2)),
    );

    // length : List a -> Int
    add_type(
        Symbol::LIST_LENGTH,
        unique_function(vec![list_type(UVAR1, TVAR1)], int_type(UVAR2)),
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

    // set : Attr (w | u | v) (List (Attr u a))
    //     , Attr * Int
    //     , Attr (u | v) a
    //    -> List a
    add_type(Symbol::LIST_SET, {
        let u = UVAR1;
        let v = UVAR2;
        let w = UVAR3;
        let star1 = UVAR4;
        let star2 = UVAR5;

        let a = TVAR1;

        unique_function(
            vec![
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![
                        disjunction(w, vec![u, v]),
                        SolvedType::Apply(Symbol::LIST_LIST, vec![attr_type(u, a)]),
                    ],
                ),
                int_type(star1),
                SolvedType::Apply(Symbol::ATTR_ATTR, vec![disjunction(u, vec![v]), flex(a)]),
            ],
            SolvedType::Apply(
                Symbol::ATTR_ATTR,
                vec![
                    boolean(star2),
                    SolvedType::Apply(Symbol::LIST_LIST, vec![attr_type(u, a)]),
                ],
            ),
        )
    });

    // push : Attr (w | u | v) (List (Attr u a))
    //      , Attr (u | v) a
    //     -> Attr * (List (Attr u a))
    add_type(Symbol::LIST_PUSH, {
        let u = UVAR1;
        let v = UVAR2;
        let w = UVAR3;
        let star = UVAR4;

        let a = TVAR1;

        unique_function(
            vec![
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![
                        disjunction(w, vec![u, v]),
                        SolvedType::Apply(Symbol::LIST_LIST, vec![attr_type(u, a)]),
                    ],
                ),
                SolvedType::Apply(Symbol::ATTR_ATTR, vec![disjunction(u, vec![v]), flex(a)]),
            ],
            SolvedType::Apply(
                Symbol::ATTR_ATTR,
                vec![
                    boolean(star),
                    SolvedType::Apply(Symbol::LIST_LIST, vec![attr_type(u, a)]),
                ],
            ),
        )
    });

    // map : List a, (a -> b) -> List b
    add_type(
        Symbol::LIST_MAP,
        unique_function(
            vec![
                list_type(UVAR1, TVAR1),
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![
                        shared(),
                        SolvedType::Func(vec![flex(TVAR1)], Box::new(flex(TVAR2))),
                    ],
                ),
            ],
            list_type(UVAR2, TVAR2),
        ),
    );

    // foldr : List a, (a -> b -> b), b -> b
    add_type(
        Symbol::LIST_FOLDR,
        unique_function(
            vec![
                list_type(UVAR1, TVAR1),
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![
                        shared(),
                        SolvedType::Func(vec![flex(TVAR1), flex(TVAR2)], Box::new(flex(TVAR2))),
                    ],
                ),
                flex(TVAR2),
            ],
            flex(TVAR2),
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
fn float_type(u: VarId) -> SolvedType {
    SolvedType::Apply(
        Symbol::ATTR_ATTR,
        vec![flex(u), SolvedType::Apply(Symbol::FLOAT_FLOAT, Vec::new())],
    )
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
