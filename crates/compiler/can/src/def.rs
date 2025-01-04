use crate::abilities::AbilityMemberData;
use crate::abilities::ImplKey;
use crate::abilities::MemberVariables;
use crate::abilities::PendingMemberType;
use crate::annotation::canonicalize_annotation;
use crate::annotation::find_type_def_symbols;
use crate::annotation::make_apply_symbol;
use crate::annotation::AnnotationFor;
use crate::annotation::IntroducedVariables;
use crate::annotation::OwnedNamedOrAble;
use crate::derive;
use crate::env::Env;
use crate::env::FxMode;
use crate::expr::canonicalize_record;
use crate::expr::get_lookup_symbols;
use crate::expr::AnnotatedMark;
use crate::expr::ClosureData;
use crate::expr::Declarations;
use crate::expr::Expr::{self, *};
use crate::expr::StructAccessorData;
use crate::expr::{canonicalize_expr, Output, Recursive};
use crate::pattern::{canonicalize_def_header_pattern, BindingsFromPattern, Pattern};
use crate::procedure::QualifiedReference;
use crate::procedure::References;
use crate::scope::create_alias;
use crate::scope::SymbolLookup;
use crate::scope::{PendingAbilitiesInScope, Scope};
use roc_collections::ReferenceMatrix;
use roc_collections::VecMap;
use roc_collections::VecSet;
use roc_collections::{ImSet, MutMap, SendMap};
use roc_error_macros::internal_error;
use roc_module::ident::Ident;
use roc_module::ident::Lowercase;
use roc_module::ident::ModuleName;
use roc_module::ident::QualifiedModuleName;
use roc_module::symbol::IdentId;
use roc_module::symbol::ModuleId;
use roc_module::symbol::Symbol;
use roc_parse::ast;
use roc_parse::ast::AssignedField;
use roc_parse::ast::Defs;
use roc_parse::ast::ExtractSpaces;
use roc_parse::ast::TypeHeader;
use roc_parse::ident::Accessor;
use roc_parse::pattern::PatternType;
use roc_problem::can::ShadowKind;
use roc_problem::can::{CycleEntry, Problem, RuntimeError};
use roc_region::all::{Loc, Region};
use roc_types::subs::IllegalCycleMark;
use roc_types::subs::{VarStore, Variable};
use roc_types::types::AliasCommon;
use roc_types::types::AliasKind;
use roc_types::types::AliasVar;
use roc_types::types::IndexOrField;
use roc_types::types::LambdaSet;
use roc_types::types::MemberImpl;
use roc_types::types::OptAbleType;
use roc_types::types::{Alias, Type};
use std::fmt::Debug;
use std::fs;
use std::io::Read;
use std::path::PathBuf;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct Def {
    pub loc_pattern: Loc<Pattern>,
    pub loc_expr: Loc<Expr>,
    pub expr_var: Variable,
    pub pattern_vars: SendMap<Symbol, Variable>,
    pub annotation: Option<Annotation>,
    pub kind: DefKind,
}

impl Def {
    pub fn region(&self) -> Region {
        let head_region = match &self.annotation {
            Some(ann) => {
                if ann.region.start() < self.loc_pattern.region.start() {
                    ann.region
                } else {
                    // Happens with annotation-only bodies like foo : T, since `T` is after the
                    // pattern.
                    self.loc_pattern.region
                }
            }
            None => self.loc_pattern.region,
        };
        Region::span_across(&head_region, &self.loc_expr.region)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum DefKind {
    /// A def that introduces identifiers
    Let,
    /// A standalone statement with an fx variable
    Stmt(Variable),
    /// Ignored result, must be effectful
    Ignored(Variable),
}

impl DefKind {
    pub fn map_var<F: Fn(Variable) -> Variable>(self, f: F) -> Self {
        match self {
            DefKind::Let => DefKind::Let,
            DefKind::Stmt(v) => DefKind::Stmt(f(v)),
            DefKind::Ignored(v) => DefKind::Ignored(f(v)),
        }
    }

    pub fn from_pattern(var_store: &mut VarStore, pattern: &Loc<Pattern>) -> Self {
        if BindingsFromPattern::new(pattern)
            .peekable()
            .peek()
            .is_none()
        {
            DefKind::Ignored(var_store.fresh())
        } else {
            DefKind::Let
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Annotation {
    pub signature: Type,
    pub introduced_variables: IntroducedVariables,
    pub aliases: VecMap<Symbol, Alias>,
    pub region: Region,
}

impl Annotation {
    fn freshen(mut self, var_store: &mut VarStore) -> Self {
        let mut substitutions = MutMap::default();

        for v in self.introduced_variables.lambda_sets.iter_mut() {
            let new = var_store.fresh();
            substitutions.insert(*v, new);

            *v = new;
        }

        self.signature.substitute_variables(&substitutions);

        self
    }

    pub fn convert_to_fn(&mut self, argument_count: usize, var_store: &mut VarStore) {
        let mut arg_types = Vec::with_capacity(argument_count);

        for _ in 0..argument_count {
            let var = var_store.fresh();
            self.introduced_variables.insert_inferred(Loc::at_zero(var));

            arg_types.push(Type::Variable(var));
        }

        self.signature = Type::Function(
            arg_types,
            Box::new(Type::Variable(var_store.fresh())),
            Box::new(self.signature.clone()),
            Box::new(Type::Variable(var_store.fresh())),
        );
    }
}

#[derive(Debug)]
pub(crate) struct CanDefs {
    defs: Vec<Option<Def>>,
    dbgs: ExpectsOrDbgs,
    expects: ExpectsOrDbgs,
    def_ordering: DefOrdering,
    aliases: VecMap<Symbol, Alias>,
}

#[derive(Clone, Debug)]
pub struct ExpectsOrDbgs {
    pub conditions: Vec<Expr>,
    pub regions: Vec<Region>,
    pub preceding_comment: Vec<Region>,
}

impl ExpectsOrDbgs {
    fn with_capacity(capacity: usize) -> Self {
        Self {
            conditions: Vec::with_capacity(capacity),
            regions: Vec::with_capacity(capacity),
            preceding_comment: Vec::with_capacity(capacity),
        }
    }

    fn push(&mut self, loc_can_condition: Loc<Expr>, preceding_comment: Region) {
        self.conditions.push(loc_can_condition.value);
        self.regions.push(loc_can_condition.region);
        self.preceding_comment.push(preceding_comment);
    }
}

/// A Def that has had patterns and type annnotations canonicalized,
/// but no Expr canonicalization has happened yet. Also, it has had spaces
/// and nesting resolved, and knows whether annotations are standalone or not.
#[derive(Debug, Clone)]
enum PendingValueDef<'a> {
    /// A standalone annotation with no body
    AnnotationOnly(Loc<Pattern>, &'a Loc<ast::TypeAnnotation<'a>>),
    /// A body with no type annotation
    Body(Loc<Pattern>, &'a Loc<ast::Expr<'a>>),
    /// A body with a type annotation
    TypedBody(
        &'a Loc<ast::Pattern<'a>>,
        Loc<Pattern>,
        &'a Loc<ast::TypeAnnotation<'a>>,
        &'a Loc<ast::Expr<'a>>,
    ),
    /// Module params from an import
    ImportParams {
        symbol: Symbol,
        variable: Variable,
        loc_pattern: Loc<Pattern>,
        module_id: ModuleId,
        opt_provided: Option<ast::Collection<'a, Loc<AssignedField<'a, ast::Expr<'a>>>>>,
    },
    /// Ingested file
    IngestedFile(
        Loc<Pattern>,
        Option<Loc<ast::TypeAnnotation<'a>>>,
        Loc<ast::StrLiteral<'a>>,
    ),
    /// A standalone statement
    Stmt(&'a Loc<ast::Expr<'a>>),
}

impl PendingValueDef<'_> {
    fn loc_pattern(&self) -> Option<&Loc<Pattern>> {
        match self {
            PendingValueDef::AnnotationOnly(loc_pattern, _) => Some(loc_pattern),
            PendingValueDef::Body(loc_pattern, _) => Some(loc_pattern),
            PendingValueDef::TypedBody(_, loc_pattern, _, _) => Some(loc_pattern),
            PendingValueDef::ImportParams {
                loc_pattern,
                symbol: _,
                variable: _,
                module_id: _,
                opt_provided: _,
            } => Some(loc_pattern),
            PendingValueDef::IngestedFile(loc_pattern, _, _) => Some(loc_pattern),
            PendingValueDef::Stmt(_) => None,
        }
    }
}

#[derive(Debug, Clone)]
struct PendingAbilityMember<'a> {
    name: Loc<Symbol>,
    typ: Loc<ast::TypeAnnotation<'a>>,
}

#[derive(Debug, Clone)]
enum PendingTypeDef<'a> {
    /// A structural type alias, e.g. `Ints : List Int`
    Alias {
        name: Loc<Symbol>,
        vars: Vec<Loc<Lowercase>>,
        ann: &'a Loc<ast::TypeAnnotation<'a>>,
    },

    /// An opaque type alias, e.g. `Age := U32`.
    Opaque {
        name_str: &'a str,
        name: Loc<Symbol>,
        vars: Vec<Loc<Lowercase>>,
        ann: &'a Loc<ast::TypeAnnotation<'a>>,
        derived: Option<&'a Loc<ast::ImplementsAbilities<'a>>>,
    },

    Ability {
        name: Loc<Symbol>,
        members: Vec<PendingAbilityMember<'a>>,
    },

    /// An invalid alias, that is ignored in the rest of the pipeline
    /// e.g. a definition like `MyAlias 1 : Int`
    /// with an incorrect pattern
    InvalidAlias {
        #[allow(dead_code)]
        kind: AliasKind,
        symbol: Symbol,
        region: Region,
    },

    /// An alias with a name that shadows another symbol
    ShadowedAlias,

    /// An invalid ability, that is ignored in the rest of the pipeline.
    /// E.g. a shadowed ability, or with a bad definition.
    InvalidAbility {
        symbol: Symbol,
        region: Region,
    },

    AbilityNotOnToplevel,
    AbilityShadows,
}

impl PendingTypeDef<'_> {
    fn introduction(&self) -> Option<(Symbol, Region)> {
        match self {
            PendingTypeDef::Alias { name, ann, .. } => {
                let region = Region::span_across(&name.region, &ann.region);

                Some((name.value, region))
            }
            PendingTypeDef::Opaque {
                name_str: _,
                name,
                vars: _,
                ann,
                derived,
            } => {
                let end = derived.map(|d| d.region).unwrap_or(ann.region);
                let region = Region::span_across(&name.region, &end);

                Some((name.value, region))
            }
            PendingTypeDef::Ability { name, .. } => Some((name.value, name.region)),
            PendingTypeDef::InvalidAlias { symbol, region, .. } => Some((*symbol, *region)),
            PendingTypeDef::ShadowedAlias { .. } => None,
            PendingTypeDef::InvalidAbility { symbol, region } => Some((*symbol, *region)),
            PendingTypeDef::AbilityNotOnToplevel => None,
            PendingTypeDef::AbilityShadows => None,
        }
    }
}

// See github.com/roc-lang/roc/issues/800 for discussion of the large_enum_variant check.
#[derive(Clone, Debug)]
#[allow(clippy::large_enum_variant)]
pub enum Declaration {
    Declare(Def),
    DeclareRec(Vec<Def>, IllegalCycleMark),
    Expects(ExpectsOrDbgs),
    /// If we know a cycle is illegal during canonicalization.
    /// Otherwise we will try to detect this during solving; see [`IllegalCycleMark`].
    InvalidCycle(Vec<CycleEntry>),
}

/// Returns a topologically sorted sequence of alias/opaque names
fn sort_type_defs_before_introduction(
    referenced_symbols: VecMap<Symbol, Vec<Symbol>>,
) -> Vec<Symbol> {
    let capacity = referenced_symbols.len();
    let mut matrix = ReferenceMatrix::new(capacity);

    let (symbols, referenced) = referenced_symbols.unzip();

    for (index, references) in referenced.iter().enumerate() {
        for referenced in references {
            match symbols.iter().position(|k| k == referenced) {
                None => { /* not defined in this scope */ }
                Some(ref_index) => matrix.set_row_col(index, ref_index, true),
            }
        }
    }

    // find the strongly connected components and their relations
    matrix
        .strongly_connected_components_all()
        .groups()
        .flat_map(|(group, _)| group.iter_ones())
        .map(|index| symbols[index])
        .collect()
}

#[inline(always)]
#[allow(clippy::too_many_arguments)]
fn canonicalize_alias<'a>(
    env: &mut Env<'a>,
    output: &mut Output,
    var_store: &mut VarStore,
    scope: &mut Scope,
    pending_abilities_in_scope: &PendingAbilitiesInScope,

    name: Loc<Symbol>,
    ann: &'a Loc<ast::TypeAnnotation<'a>>,
    vars: &[Loc<Lowercase>],
    kind: AliasKind,
) -> Result<Alias, ()> {
    let symbol = name.value;
    let annotation_for = match kind {
        AliasKind::Structural => AnnotationFor::Alias,
        AliasKind::Opaque => AnnotationFor::Opaque,
    };
    let can_ann = canonicalize_annotation(
        env,
        scope,
        &ann.value,
        ann.region,
        var_store,
        pending_abilities_in_scope,
        annotation_for,
    );

    // Record all the annotation's references in output.references.lookups
    output.references.union_mut(&can_ann.references);

    let mut can_vars: Vec<Loc<AliasVar>> = Vec::with_capacity(vars.len());
    let mut is_phantom = false;

    let IntroducedVariables {
        named,
        able,
        wildcards,
        inferred,
        infer_ext_in_output,
        ..
    } = can_ann.introduced_variables;

    let mut named: Vec<_> = (named.into_iter().map(OwnedNamedOrAble::Named))
        .chain(able.into_iter().map(OwnedNamedOrAble::Able))
        .collect();
    for loc_lowercase in vars.iter() {
        let opt_index = named
            .iter()
            .position(|nv| nv.ref_name() == &loc_lowercase.value);
        match opt_index {
            Some(index) => {
                // This is a valid lowercase rigid var for the type def.
                let named_variable = named.swap_remove(index);
                let var = named_variable.variable();
                let opt_bound_abilities = named_variable.opt_abilities().map(ToOwned::to_owned);
                let name = named_variable.name();

                can_vars.push(Loc {
                    value: AliasVar {
                        name,
                        var,
                        opt_bound_abilities,
                    },
                    region: loc_lowercase.region,
                });
            }
            None => match kind {
                AliasKind::Structural => {
                    is_phantom = true;

                    env.problems.push(Problem::PhantomTypeArgument {
                        typ: symbol,
                        variable_region: loc_lowercase.region,
                        variable_name: loc_lowercase.value.clone(),
                        alias_kind: AliasKind::Structural,
                    });
                }
                AliasKind::Opaque => {
                    // Opaques can have phantom types.
                    can_vars.push(Loc {
                        value: AliasVar {
                            name: loc_lowercase.value.clone(),
                            var: var_store.fresh(),
                            opt_bound_abilities: None,
                        },
                        region: loc_lowercase.region,
                    });
                }
            },
        }
    }

    if is_phantom {
        // Bail out
        return Err(());
    }

    // Report errors for wildcards (*), underscores (_), and named vars that weren't declared.
    let mut no_problems = true;

    if let Some(loc_var) = wildcards.first() {
        env.problems.push(Problem::WildcardNotAllowed {
            typ: symbol,
            num_wildcards: wildcards.len(),
            one_occurrence: loc_var.region,
            kind,
        });

        no_problems = false;
    }

    if let Some(loc_var) = inferred.first() {
        env.problems.push(Problem::UnderscoreNotAllowed {
            typ: symbol,
            num_underscores: inferred.len(),
            one_occurrence: loc_var.region,
            kind,
        });

        no_problems = false;
    }

    if let Some(nv) = named.first() {
        env.problems.push(Problem::UndeclaredTypeVar {
            typ: symbol,
            num_unbound: named.len(),
            one_occurrence: nv.first_seen(),
            kind,
        });

        no_problems = false;
    }

    if no_problems {
        Ok(create_alias(
            symbol,
            name.region,
            can_vars.clone(),
            infer_ext_in_output,
            can_ann.typ,
            kind,
        ))
    } else {
        Err(())
    }
}

#[macro_export]
macro_rules! params_in_abilities_unimplemented {
    ($lookup:expr) => {
        match $lookup.module_params {
            None => $lookup.symbol,
            Some(_) => unimplemented!("params in abilities"),
        }
    };
}

/// Canonicalizes a claimed ability implementation like `{ eq }` or `{ eq: myEq }`.
/// Returns a mapping of the ability member to the implementation symbol.
/// If there was an error, a problem will be recorded and nothing is returned.
fn canonicalize_claimed_ability_impl<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    ability: Symbol,
    loc_impl: &Loc<ast::AssignedField<'a, ast::Expr<'a>>>,
) -> Result<(Symbol, Symbol), ()> {
    let ability_home = ability.module_id();

    match loc_impl.extract_spaces().item {
        AssignedField::LabelOnly(label) => {
            let label_str = label.value;
            let region = label.region;

            let member_symbol =
                match env.qualified_lookup_with_module_id(scope, ability_home, label_str, region) {
                    Ok(lookup) => params_in_abilities_unimplemented!(lookup),
                    Err(_) => {
                        env.problem(Problem::NotAnAbilityMember {
                            ability,
                            name: label_str.to_owned(),
                            region,
                        });

                        return Err(());
                    }
                };

            // There are two options for how the implementation symbol is defined.
            //
            // OPTION-1: The implementation identifier is the only identifier of that name in the
            //           scope. For example,
            //
            //               module []
            //
            //               Hello := {} implements [Encoding.{ toEncoder }]
            //
            //               toEncoder = \@Hello {} -> ...
            //
            //           In this case, we just do a simple lookup in the scope to find our
            //           `toEncoder` impl.
            //
            // OPTION-2: The implementation identifier is a unique shadow of the ability member,
            //           which has also been explicitly imported. For example,
            //
            //               module []
            //
            //               import Encoding exposing [toEncoder]
            //
            //               Hello := {} implements [Encoding.{ toEncoder }]
            //
            //               toEncoder = \@Hello {} -> ...
            //
            //           In this case, we allow the `F` module's `toEncoder` def to shadow
            //           `toEncoder` only to define this specialization's `toEncoder`.
            //
            // To handle both cases, try checking for a shadow first, then check for a direct
            // reference. We want to check for a direct reference second so that if there is a
            // shadow, we won't accidentally grab the imported symbol.
            let opt_impl_symbol =
                (scope.lookup_ability_member_shadow(member_symbol)).or_else(|| {
                    scope
                        .lookup_str(label_str, region)
                        .map(|s| params_in_abilities_unimplemented!(s))
                        .ok()
                });

            match opt_impl_symbol {
                // It's possible that even if we find a symbol it is still only the member
                // definition symbol, for example when the ability is defined in the same
                // module as an implementer:
                //
                //   Eq implements eq : a, a -> U64 where a implements Eq
                //
                //   A := U8 implements [Eq {eq}]
                //
                // So, do a final check that the implementation symbol is not resolved directly
                // to the member.
                Some(impl_symbol) if impl_symbol != member_symbol => {
                    Ok((member_symbol, impl_symbol))
                }
                _ => {
                    env.problem(Problem::ImplementationNotFound {
                        member: member_symbol,
                        region: label.region,
                    });
                    Err(())
                }
            }
        }
        AssignedField::RequiredValue(label, _spaces, value) => {
            let impl_ident = match value.value {
                ast::Expr::Var { module_name, ident } => {
                    if module_name.is_empty() {
                        ident
                    } else {
                        env.problem(Problem::QualifiedAbilityImpl {
                            region: value.region,
                        });
                        return Err(());
                    }
                }
                _ => {
                    env.problem(Problem::AbilityImplNotIdent {
                        region: value.region,
                    });
                    return Err(());
                }
            };
            let impl_region = value.region;

            let member_symbol = match env.qualified_lookup_with_module_id(
                scope,
                ability_home,
                label.value,
                label.region,
            ) {
                Ok(lookup) => params_in_abilities_unimplemented!(lookup),
                Err(_) => {
                    env.problem(Problem::NotAnAbilityMember {
                        ability,
                        name: label.value.to_owned(),
                        region: label.region,
                    });
                    return Err(());
                }
            };

            let impl_symbol = match scope.lookup(&impl_ident.into(), impl_region) {
                Ok(symbol) => params_in_abilities_unimplemented!(symbol),
                Err(err) => {
                    env.problem(Problem::RuntimeError(err));
                    return Err(());
                }
            };

            Ok((member_symbol, impl_symbol))
        }
        AssignedField::OptionalValue(_, _, _) => {
            env.problem(Problem::OptionalAbilityImpl {
                ability,
                region: loc_impl.region,
            });
            Err(())
        }
        AssignedField::SpaceBefore(_, _)
        | AssignedField::SpaceAfter(_, _)
        | AssignedField::IgnoredValue(_, _, _) => {
            internal_error!("unreachable")
        }
    }
}

struct SeparatedMembers {
    not_required: Vec<Symbol>,
    not_implemented: Vec<Symbol>,
}

/// Partitions ability members in a `has [ Ability {...members} ]` clause into the members the
/// opaque type claims to implement but are not part of the ability, and the ones it does not
/// implement.
fn separate_implemented_and_required_members(
    implemented: VecSet<Symbol>,
    required: VecSet<Symbol>,
) -> SeparatedMembers {
    use std::cmp::Ordering;

    let mut implemented = implemented.into_vec();
    let mut required = required.into_vec();

    implemented.sort();
    required.sort();

    let mut implemented = implemented.into_iter().peekable();
    let mut required = required.into_iter().peekable();

    let mut not_required = vec![];
    let mut not_implemented = vec![];

    loop {
        // Equal => both required and implemented
        // Less => implemented but not required
        // Greater => required but not implemented

        let ord = match (implemented.peek(), required.peek()) {
            (Some(implemented), Some(required)) => Some(implemented.cmp(required)),
            (Some(_), None) => Some(Ordering::Less),
            (None, Some(_)) => Some(Ordering::Greater),
            (None, None) => None,
        };

        match ord {
            Some(Ordering::Less) => {
                not_required.push(implemented.next().unwrap());
            }
            Some(Ordering::Greater) => {
                not_implemented.push(required.next().unwrap());
            }
            Some(Ordering::Equal) => {
                _ = implemented.next().unwrap();
                _ = required.next().unwrap();
            }
            None => break,
        }
    }

    SeparatedMembers {
        not_required,
        not_implemented,
    }
}

type DerivedDef<'a> = Loc<PendingValue<'a>>;

struct CanonicalizedOpaque<'a> {
    opaque_def: Alias,
    derived_defs: Vec<DerivedDef<'a>>,
}

#[inline(always)]
#[allow(clippy::too_many_arguments)]
fn canonicalize_opaque<'a>(
    env: &mut Env<'a>,
    output: &mut Output,
    var_store: &mut VarStore,
    scope: &mut Scope,
    pending_abilities_in_scope: &PendingAbilitiesInScope,

    name: Loc<Symbol>,
    name_str: &'a str,
    ann: &'a Loc<ast::TypeAnnotation<'a>>,
    vars: &[Loc<Lowercase>],
    has_abilities: Option<&'a Loc<ast::ImplementsAbilities<'a>>>,
) -> Result<CanonicalizedOpaque<'a>, ()> {
    let alias = canonicalize_alias(
        env,
        output,
        var_store,
        scope,
        pending_abilities_in_scope,
        name,
        ann,
        vars,
        AliasKind::Opaque,
    )?;

    let mut references = References::new();

    let mut derived_defs = Vec::new();
    if let Some(has_abilities) = has_abilities {
        let has_abilities = has_abilities.value.collection();

        let mut derived_abilities = vec![];

        for has_ability in has_abilities.items {
            let region = has_ability.region;
            let (ability, opt_impls) = match has_ability.value.extract_spaces().item {
                ast::ImplementsAbility::ImplementsAbility { ability, impls } => (ability, impls),
                _ => internal_error!("spaces not extracted"),
            };

            let ability_region = ability.region;

            // Op := {} has [Eq]
            let (ability, members) = match ability.value {
                ast::TypeAnnotation::Apply(module_name, ident, []) => {
                    match make_apply_symbol(env, region, scope, module_name, ident, &mut references)
                    {
                        Ok(ability) => {
                            let opt_members = scope
                                .abilities_store
                                .members_of_ability(ability)
                                .map(|members| members.iter().copied().collect())
                                .or_else(|| pending_abilities_in_scope.get(&ability).cloned());

                            if let Some(members) = opt_members {
                                // This is an ability we already imported into the scope,
                                // or which is also undergoing canonicalization at the moment.
                                (ability, members)
                            } else {
                                env.problem(Problem::NotAnAbility(ability_region));
                                continue;
                            }
                        }
                        Err(_) => {
                            // This is bad apply; an error will have been reported for it
                            // already.
                            continue;
                        }
                    }
                }
                _ => {
                    // Register the problem but keep going.
                    env.problem(Problem::NotAnAbility(ability_region));
                    continue;
                }
            };

            if let Some(impls) = opt_impls {
                let mut impl_map: VecMap<Symbol, Loc<MemberImpl>> = VecMap::default();

                // First up canonicalize all the claimed implementations, building a map of ability
                // member -> implementation.
                for loc_impl in impls.extract_spaces().item.items {
                    let (member, impl_symbol) =
                        match canonicalize_claimed_ability_impl(env, scope, ability, loc_impl) {
                            Ok((member, impl_symbol)) => (member, impl_symbol),
                            Err(()) => continue,
                        };

                    // Did the user claim this implementation for a specialization of a different
                    // type? e.g.
                    //
                    //   A implements [Hash {hash: my_hash}]
                    //   B implements [Hash {hash: my_hash}]
                    //
                    // If so, that's an error and we drop the impl for this opaque type.
                    let member_impl = match scope.abilities_store.impl_key(impl_symbol) {
                        Some(ImplKey {
                            opaque,
                            ability_member,
                        }) => {
                            env.problem(Problem::OverloadedSpecialization {
                                overload: loc_impl.region,
                                original_opaque: *opaque,
                                ability_member: *ability_member,
                            });
                            MemberImpl::Error
                        }
                        None => MemberImpl::Impl(impl_symbol),
                    };

                    // Did the user already claim an implementation for the ability member for this
                    // type previously? (e.g. Hash {hash: hash1, hash: hash2})
                    let opt_old_impl_symbol =
                        impl_map.insert(member, Loc::at(loc_impl.region, member_impl));

                    if let Some(old_impl_symbol) = opt_old_impl_symbol {
                        env.problem(Problem::DuplicateImpl {
                            original: old_impl_symbol.region,
                            duplicate: loc_impl.region,
                        });
                    }
                }

                // Check that the members this opaque claims to implement corresponds 1-to-1 with
                // the members the ability offers.
                let SeparatedMembers {
                    not_required,
                    not_implemented,
                } = separate_implemented_and_required_members(
                    impl_map.iter().map(|(member, _)| *member).collect(),
                    members,
                );

                if !not_required.is_empty() {
                    // Implementing something that's not required is a recoverable error, we don't
                    // need to skip association of the implemented abilities. Just remove the
                    // unneeded members.
                    for sym in not_required.iter() {
                        impl_map.remove(sym);
                    }

                    env.problem(Problem::ImplementsNonRequired {
                        region,
                        ability,
                        not_required,
                    });
                }

                if !not_implemented.is_empty() {
                    // We'll generate runtime errors for the members that are needed but
                    // unspecified.
                    for sym in not_implemented.iter() {
                        impl_map.insert(*sym, Loc::at_zero(MemberImpl::Error));
                    }

                    env.problem(Problem::DoesNotImplementAbility {
                        region,
                        ability,
                        not_implemented,
                    });
                }

                let impls = impl_map
                    .into_iter()
                    .map(|(member, def)| (member, def.value));

                scope
                    .abilities_store
                    .register_declared_implementations(name.value, impls);
            } else if let Some((_, members)) = ability.derivable_ability() {
                let num_members = members.len();

                derived_defs.reserve(num_members);

                let mut impls = Vec::with_capacity(num_members);
                for &member in members.iter() {
                    let (derived_impl, impl_pat, impl_body) =
                        derive::synthesize_member_impl(env, scope, name_str, member);

                    let derived_def = Loc::at(
                        derive::DERIVED_REGION,
                        PendingValue::Def(PendingValueDef::Body(impl_pat, impl_body)),
                    );

                    impls.push((member, MemberImpl::Impl(derived_impl)));
                    derived_defs.push(derived_def);
                }

                scope
                    .abilities_store
                    .register_declared_implementations(name.value, impls);

                derived_abilities.push(Loc::at(ability_region, ability));
            } else {
                // There was no record specified of functions to use for
                // members, but also this isn't a builtin ability, so we don't
                // know how to auto-derive it.
                env.problem(Problem::IllegalDerivedAbility(region));
            }
        }

        if !derived_abilities.is_empty() {
            // Fresh instance of this opaque to be checked for derivability during solving.
            let fresh_inst = Type::DelayedAlias(AliasCommon {
                symbol: name.value,
                type_arguments: alias
                    .type_variables
                    .iter()
                    .map(|alias_var| {
                        Loc::at(
                            alias_var.region,
                            OptAbleType {
                                typ: Type::Variable(var_store.fresh()),
                                opt_abilities: alias_var.value.opt_bound_abilities.clone(),
                            },
                        )
                    })
                    .collect(),
                lambda_set_variables: alias
                    .lambda_set_variables
                    .iter()
                    .map(|_| LambdaSet(Type::Variable(var_store.fresh())))
                    .collect(),
                infer_ext_in_output_types: alias
                    .infer_ext_in_output_variables
                    .iter()
                    .map(|_| Type::Variable(var_store.fresh()))
                    .collect(),
            });

            let old = output
                .pending_derives
                .insert(name.value, (fresh_inst, derived_abilities));

            debug_assert!(old.is_none());
        }
    }

    output.references.union_mut(&references);

    Ok(CanonicalizedOpaque {
        opaque_def: alias,
        derived_defs,
    })
}

#[inline(always)]
pub(crate) fn canonicalize_defs<'a>(
    env: &mut Env<'a>,
    mut output: Output,
    var_store: &mut VarStore,
    scope: &mut Scope,
    loc_defs: &'a mut roc_parse::ast::Defs<'a>,
    pattern_type: PatternType,
) -> (
    CanDefs,
    Output,
    MutMap<Symbol, Region>,
    Vec<IntroducedImport>,
) {
    // Canonicalizing defs while detecting shadowing involves a multi-step process:
    //
    // 1. Go through each of the patterns.
    // 2. For each identifier pattern, get the scope.symbol() for the ident. (That symbol will use the home module for its module.)
    // 3. If that symbol is already in scope, then we're about to shadow it. Error!
    // 4. Otherwise, add it to the scope immediately, so we can detect shadowing within the same
    //    pattern (e.g. (Foo a a) = ...)
    // 5. Add this canonicalized pattern and its corresponding ast::Expr to pending_exprs.
    // 5. Once every pattern has been processed and added to scope, go back and canonicalize the exprs from
    //    pending_exprs, this time building up a canonical def for each one.
    //
    // This way, whenever any expr is doing lookups, it knows everything that's in scope -
    // even defs that appear after it in the source.
    //
    // This naturally handles recursion too, because a given expr which refers
    // to itself won't be processed until after its def has been added to scope.

    let mut pending_type_defs = Vec::with_capacity(loc_defs.type_defs.len());
    let mut pending_value_defs = Vec::with_capacity(loc_defs.value_defs.len());
    let mut pending_abilities_in_scope = PendingAbilitiesInScope::default();

    // Convert the type defs into pending defs first, then all the value defs.
    // Follow this order because we need all value symbols to fully canonicalize type defs (in case
    // there are opaques that implement an ability using a value symbol). But, value symbols might
    // shadow symbols defined in a local ability def.

    for either_index in loc_defs.tags.iter() {
        if let Ok(type_index) = either_index.split() {
            let type_def = &loc_defs.type_defs[type_index.index()];
            let pending_type_def = to_pending_type_def(env, type_def, scope, pattern_type);
            if let PendingTypeDef::Ability { name, members } = &pending_type_def {
                pending_abilities_in_scope.insert(
                    name.value,
                    members.iter().map(|mem| mem.name.value).collect(),
                );
            }
            pending_type_defs.push(pending_type_def);
        }
    }

    for (index, either_index) in loc_defs.tags.iter().enumerate() {
        if let Err(value_index) = either_index.split() {
            let value_def = &loc_defs.value_defs[value_index.index()];
            let region = loc_defs.regions[index];

            let pending = to_pending_value_def(
                env,
                var_store,
                value_def,
                region,
                scope,
                &pending_abilities_in_scope,
                &mut output,
                pattern_type,
            );

            pending_value_defs.push(Loc::at(region, pending));
        }
    }

    if cfg!(debug_assertions) {
        scope.register_debug_idents();
    }

    let CanonicalizedTypeDefs {
        aliases,
        symbols_introduced,
        derived_defs,
    } = canonicalize_type_defs(
        env,
        &mut output,
        var_store,
        scope,
        &pending_abilities_in_scope,
        pending_type_defs,
    );

    // Add the derived ASTs, so that we create proper canonicalized defs for them.
    // They can go at the end, and derived defs should never reference anything other than builtin
    // ability members.
    pending_value_defs.extend(derived_defs);

    // Now that we have the scope completely assembled, and shadowing resolved,
    // we're ready to canonicalize any body exprs.
    canonicalize_value_defs(
        env,
        output,
        var_store,
        scope,
        pending_value_defs,
        pattern_type,
        aliases,
        symbols_introduced,
    )
}

#[allow(clippy::too_many_arguments)]
fn canonicalize_value_defs<'a>(
    env: &mut Env<'a>,
    mut output: Output,
    var_store: &mut VarStore,
    scope: &mut Scope,
    value_defs: Vec<Loc<PendingValue<'a>>>,
    pattern_type: PatternType,
    mut aliases: VecMap<Symbol, Alias>,
    mut symbols_introduced: MutMap<Symbol, Region>,
) -> (
    CanDefs,
    Output,
    MutMap<Symbol, Region>,
    Vec<IntroducedImport>,
) {
    // Canonicalize all the patterns, record shadowing problems, and store
    // the ast::Expr values in pending_exprs for further canonicalization
    // once we've finished assembling the entire scope.
    let mut pending_value_defs = Vec::with_capacity(value_defs.len());
    let mut pending_dbgs = Vec::with_capacity(value_defs.len());
    let mut pending_expects = Vec::with_capacity(value_defs.len());

    let mut imports_introduced = Vec::with_capacity(value_defs.len());

    for loc_pending_def in value_defs {
        match loc_pending_def.value {
            PendingValue::Def(pending_def) => {
                // Record the ast::Expr for later. We'll do another pass through these
                // once we have the entire scope assembled. If we were to canonicalize
                // the exprs right now, they wouldn't have symbols in scope from defs
                // that get would have gotten added later in the defs list!
                pending_value_defs.push(pending_def);
            }
            PendingValue::SignatureDefMismatch => { /* skip */ }
            PendingValue::Dbg(pending_dbg) => {
                pending_dbgs.push(pending_dbg);
            }
            PendingValue::Expect(pending_expect) => {
                pending_expects.push(pending_expect);
            }
            PendingValue::ModuleImport(PendingModuleImport {
                module_id,
                region,
                exposed_symbols,
                params,
            }) => {
                imports_introduced.push(IntroducedImport {
                    module_id,
                    region,
                    exposed_symbols,
                });

                pending_value_defs.push(PendingValueDef::ImportParams {
                    symbol: params.symbol,
                    variable: params.variable,
                    loc_pattern: params.loc_pattern,
                    opt_provided: params.opt_provided,
                    module_id,
                });
            }
            PendingValue::InvalidIngestedFile => { /* skip */ }
            PendingValue::ImportNameConflict => { /* skip */ }
            PendingValue::StmtAfterExpr => { /* skip */ }
        }
    }

    let mut symbol_to_index: Vec<(IdentId, u32)> = Vec::with_capacity(pending_value_defs.len());

    for (def_index, pending_def) in pending_value_defs.iter().enumerate() {
        let Some(loc_pattern) = pending_def.loc_pattern() else {
            continue;
        };

        let new_bindings = BindingsFromPattern::new(loc_pattern).peekable();

        for (s, r) in new_bindings {
            // store the top-level defs, used to ensure that closures won't capture them
            if let PatternType::TopLevelDef = pattern_type {
                env.top_level_symbols.insert(s);
            }

            symbols_introduced.insert(s, r);

            debug_assert_eq!(env.home, s.module_id());
            debug_assert!(
                !symbol_to_index.iter().any(|(id, _)| *id == s.ident_id()),
                "{s:?}"
            );

            symbol_to_index.push((s.ident_id(), def_index as u32));
        }
    }

    let capacity = pending_value_defs.len();
    let mut defs = Vec::with_capacity(capacity);
    let mut def_ordering = DefOrdering::from_symbol_to_id(env.home, symbol_to_index, capacity);

    for (def_id, pending_def) in pending_value_defs.into_iter().enumerate() {
        let temp_output = canonicalize_pending_value_def(
            env,
            pending_def,
            output,
            scope,
            var_store,
            pattern_type,
            &mut aliases,
        );

        output = temp_output.output;

        if let (PatternType::TopLevelDef, DefKind::Ignored(_)) =
            (pattern_type, temp_output.def.kind)
        {
            env.problems.push(Problem::NoIdentifiersIntroduced(
                temp_output.def.loc_pattern.region,
            ))
        }

        defs.push(Some(temp_output.def));

        def_ordering.insert_symbol_references(def_id as u32, &temp_output.references)
    }

    let mut dbgs = ExpectsOrDbgs::with_capacity(pending_dbgs.len());
    let mut expects = ExpectsOrDbgs::with_capacity(pending_expects.len());

    for pending in pending_dbgs {
        let (loc_can_condition, can_output) = canonicalize_expr(
            env,
            var_store,
            scope,
            pending.condition.region,
            &pending.condition.value,
        );

        dbgs.push(loc_can_condition, pending.preceding_comment);

        output.union(can_output);
    }

    for pending in pending_expects {
        let (loc_can_condition, can_output) = canonicalize_expr(
            env,
            var_store,
            scope,
            pending.condition.region,
            &pending.condition.value,
        );

        expects.push(loc_can_condition, pending.preceding_comment);

        output.union(can_output);
    }

    let can_defs = CanDefs {
        defs,
        dbgs,
        expects,
        def_ordering,
        aliases,
    };

    (can_defs, output, symbols_introduced, imports_introduced)
}

struct CanonicalizedTypeDefs<'a> {
    aliases: VecMap<Symbol, Alias>,
    symbols_introduced: MutMap<Symbol, Region>,
    derived_defs: Vec<DerivedDef<'a>>,
}

fn canonicalize_type_defs<'a>(
    env: &mut Env<'a>,
    output: &mut Output,
    var_store: &mut VarStore,
    scope: &mut Scope,
    pending_abilities_in_scope: &PendingAbilitiesInScope,
    pending_type_defs: Vec<PendingTypeDef<'a>>,
) -> CanonicalizedTypeDefs<'a> {
    enum TypeDef<'a> {
        Alias(
            Loc<Symbol>,
            Vec<Loc<Lowercase>>,
            &'a Loc<ast::TypeAnnotation<'a>>,
        ),
        Opaque(
            &'a str,
            Loc<Symbol>,
            Vec<Loc<Lowercase>>,
            &'a Loc<ast::TypeAnnotation<'a>>,
            Option<&'a Loc<ast::ImplementsAbilities<'a>>>,
        ),
        Ability(Loc<Symbol>, Vec<PendingAbilityMember<'a>>),
    }

    let mut type_defs = MutMap::default();
    let mut referenced_type_symbols = VecMap::default();

    // Determine which idents we introduced in the course of this process.
    let mut symbols_introduced = MutMap::default();

    for pending_def in pending_type_defs.into_iter() {
        if let Some((symbol, region)) = pending_def.introduction() {
            symbols_introduced.insert(symbol, region);
        }

        match pending_def {
            PendingTypeDef::Alias { name, vars, ann } => {
                let referenced_symbols = find_type_def_symbols(scope, &ann.value);

                referenced_type_symbols.insert(name.value, referenced_symbols);

                type_defs.insert(name.value, TypeDef::Alias(name, vars, ann));
            }
            PendingTypeDef::Opaque {
                name_str,
                name,
                vars,
                ann,
                derived,
            } => {
                let referenced_symbols = find_type_def_symbols(scope, &ann.value);

                referenced_type_symbols.insert(name.value, referenced_symbols);
                // Don't need to insert references for derived types, because these can only contain
                // builtin abilities, and hence do not affect the type def sorting. We'll insert
                // references of usages when canonicalizing the derives.

                type_defs.insert(
                    name.value,
                    TypeDef::Opaque(name_str, name, vars, ann, derived),
                );
            }
            PendingTypeDef::Ability { name, members } => {
                let mut referenced_symbols = Vec::with_capacity(2);

                for member in members.iter() {
                    // Add the referenced type symbols of each member function. We need to make
                    // sure those are processed first before we resolve the whole ability
                    // definition.
                    referenced_symbols.extend(find_type_def_symbols(scope, &member.typ.value));
                }

                referenced_type_symbols.insert(name.value, referenced_symbols);
                type_defs.insert(name.value, TypeDef::Ability(name, members));
            }
            PendingTypeDef::InvalidAlias { .. }
            | PendingTypeDef::InvalidAbility { .. }
            | PendingTypeDef::AbilityShadows
            | PendingTypeDef::ShadowedAlias { .. }
            | PendingTypeDef::AbilityNotOnToplevel => { /* ignore */ }
        }
    }

    let sorted = sort_type_defs_before_introduction(referenced_type_symbols);
    let mut aliases = VecMap::default();
    let mut abilities = MutMap::default();
    let mut all_derived_defs = Vec::new();

    for type_name in sorted {
        match type_defs.remove(&type_name).unwrap() {
            TypeDef::Alias(name, vars, ann) => {
                let alias = canonicalize_alias(
                    env,
                    output,
                    var_store,
                    scope,
                    pending_abilities_in_scope,
                    name,
                    ann,
                    &vars,
                    AliasKind::Structural,
                );

                if let Ok(alias) = alias {
                    aliases.insert(name.value, alias);
                }
            }

            TypeDef::Opaque(name_str, name, vars, ann, derived) => {
                let alias_and_derives = canonicalize_opaque(
                    env,
                    output,
                    var_store,
                    scope,
                    pending_abilities_in_scope,
                    name,
                    name_str,
                    ann,
                    &vars,
                    derived,
                );

                if let Ok(CanonicalizedOpaque {
                    opaque_def,
                    derived_defs,
                }) = alias_and_derives
                {
                    aliases.insert(name.value, opaque_def);
                    all_derived_defs.extend(derived_defs);
                }
            }

            TypeDef::Ability(name, members) => {
                // For now we enforce that aliases cannot reference abilities, so let's wait to
                // resolve ability definitions until aliases are resolved and in scope below.
                abilities.insert(name.value, members);
            }
        }
    }

    // Now that we know the alias dependency graph, we can try to insert recursion variables
    // where aliases are recursive tag unions, or detect illegal recursions.
    let aliases = correct_mutual_recursive_type_alias(env, aliases, var_store);

    for (symbol, alias) in aliases.iter() {
        scope.add_alias(
            *symbol,
            alias.region,
            alias.type_variables.clone(),
            alias.infer_ext_in_output_variables.clone(),
            alias.typ.clone(),
            alias.kind,
        );
    }

    // Resolve all pending abilities, to add them to scope.
    resolve_abilities(
        env,
        output,
        var_store,
        scope,
        abilities,
        pending_abilities_in_scope,
    );

    CanonicalizedTypeDefs {
        aliases,
        symbols_introduced,
        derived_defs: all_derived_defs,
    }
}

/// Resolve all pending abilities, to add them to scope.
#[allow(clippy::too_many_arguments)]
fn resolve_abilities(
    env: &mut Env,
    output: &mut Output,
    var_store: &mut VarStore,
    scope: &mut Scope,
    abilities: MutMap<Symbol, Vec<PendingAbilityMember>>,
    pending_abilities_in_scope: &PendingAbilitiesInScope,
) {
    for (ability, members) in abilities {
        let mut can_members = Vec::with_capacity(members.len());

        for PendingAbilityMember {
            name:
                Loc {
                    value: member_sym,
                    region: member_name_region,
                },
            typ,
        } in members
        {
            let member_annot = canonicalize_annotation(
                env,
                scope,
                &typ.value,
                typ.region,
                var_store,
                pending_abilities_in_scope,
                AnnotationFor::Value,
            );

            // Record all the annotation's references in output.references.lookups
            output.references.union_mut(&member_annot.references);

            // What variables in the annotation are bound to the parent ability, and what variables
            // are bound to some other ability?
            let (variables_bound_to_ability, _variables_bound_to_other_abilities): (
                Vec<_>,
                Vec<_>,
            ) = member_annot
                .introduced_variables
                .able
                .iter()
                .partition(|av| av.abilities.contains(&ability));

            let var_bound_to_ability = match variables_bound_to_ability.as_slice() {
                [one] => one.variable,
                [] => {
                    // There are no variables bound to the parent ability - then this member doesn't
                    // need to be a part of the ability.
                    env.problem(Problem::AbilityMemberMissingImplementsClause {
                        member: member_sym,
                        ability,
                        region: member_name_region,
                    });
                    // Pretend the member isn't a part of the ability
                    continue;
                }
                [..] => {
                    // There is more than one variable bound to the member signature, so something like
                    //   Eq implements eq : a, b -> Bool where a implements Eq, b implements Eq
                    // We have no way of telling what type implements a particular instance of Eq in
                    // this case (a or b?), so disallow it.
                    let span_has_clauses = Region::across_all(
                        variables_bound_to_ability.iter().map(|v| &v.first_seen),
                    );
                    let bound_var_names = variables_bound_to_ability
                        .iter()
                        .map(|v| v.name.clone())
                        .collect();
                    env.problem(Problem::AbilityMemberMultipleBoundVars {
                        member: member_sym,
                        ability,
                        span_implements_clauses: span_has_clauses,
                        bound_var_names,
                    });
                    // Pretend the member isn't a part of the ability
                    continue;
                }
            };

            // The introduced variables are good; add them to the output.
            output
                .introduced_variables
                .union(&member_annot.introduced_variables);

            let iv = member_annot.introduced_variables;

            let variables = MemberVariables {
                able_vars: iv.collect_able(),
                rigid_vars: iv.collect_rigid(),
                flex_vars: iv.collect_flex(),
            };

            let signature = {
                let mut signature = member_annot.typ;
                signature
                    .instantiate_lambda_sets_as_unspecialized(var_bound_to_ability, member_sym);
                signature
            };

            can_members.push((
                member_sym,
                AbilityMemberData {
                    parent_ability: ability,
                    region: member_name_region,
                    typ: PendingMemberType::Local {
                        variables,
                        signature,
                        signature_var: var_store.fresh(),
                    },
                },
            ));
        }

        // Store what symbols a type must define implementations for to have this ability.
        scope.abilities_store.register_ability(ability, can_members);
    }
}

#[derive(Debug)]
struct DefOrdering {
    home: ModuleId,
    symbol_to_id: Vec<(IdentId, u32)>,

    // an length x length matrix indicating who references who
    references: ReferenceMatrix,

    // references without looking into closure bodies.
    // Used to spot definitely-wrong recursion
    direct_references: ReferenceMatrix,
}

impl DefOrdering {
    fn from_symbol_to_id(
        home: ModuleId,
        symbol_to_id: Vec<(IdentId, u32)>,
        capacity: usize,
    ) -> Self {
        // NOTE: because of `Pair a b = someDef` patterns, we can have more symbols than defs
        // but because `_ = someDef` we can also have more defs than symbols

        Self {
            home,
            symbol_to_id,
            references: ReferenceMatrix::new(capacity),
            direct_references: ReferenceMatrix::new(capacity),
        }
    }

    fn insert_symbol_references(&mut self, def_id: u32, def_references: &DefReferences) {
        match def_references {
            DefReferences::Value(references) => {
                let it = references.value_lookups();

                for referenced in it {
                    if let Some(ref_id) = self.get_id(*referenced) {
                        self.references
                            .set_row_col(def_id as usize, ref_id as usize, true);

                        self.direct_references
                            .set_row_col(def_id as usize, ref_id as usize, true);
                    }
                }
            }
            DefReferences::Function(references) => {
                let it = references.value_lookups().chain(references.calls());

                for referenced in it {
                    if let Some(ref_id) = self.get_id(*referenced) {
                        self.references
                            .set_row_col(def_id as usize, ref_id as usize, true);
                    }
                }
            }
            DefReferences::AnnotationWithoutBody => {
                // annotatations without bodies don't reference any other definitions
            }
        }
    }

    fn get_id(&self, symbol: Symbol) -> Option<u32> {
        if symbol.module_id() != self.home {
            return None;
        }

        let target = symbol.ident_id();

        for (ident_id, def_id) in self.symbol_to_id.iter() {
            if target == *ident_id {
                return Some(*def_id);
            }
        }

        None
    }

    fn get_symbol(&self, id: usize) -> Option<Symbol> {
        for (ident_id, def_id) in self.symbol_to_id.iter() {
            if id as u32 == *def_id {
                return Some(Symbol::new(self.home, *ident_id));
            }
        }

        None
    }
}

#[inline(always)]
pub(crate) fn sort_top_level_can_defs(
    env: &mut Env<'_>,
    scope: &mut Scope,
    var_store: &mut VarStore,
    defs: CanDefs,
    mut output: Output,
    exposed_symbols: &VecSet<Symbol>,
) -> (Declarations, Output) {
    let CanDefs {
        defs,
        dbgs: _,
        expects,
        def_ordering,
        aliases,
    } = defs;

    // TODO: inefficient, but I want to make this what CanDefs contains in the future
    let mut defs: Vec<_> = defs.into_iter().map(|x| x.unwrap()).collect();

    // symbols are put in declarations in dependency order, from "main" up, so
    //
    // x = 3
    // y = x + 1
    //
    // will get ordering [ y, x ]
    let mut declarations = Declarations::with_capacity(defs.len());

    // because of the ordering of declarations, expects should come first because they are
    // independent, but can rely on all other top-level symbols in the module
    let it = expects
        .conditions
        .into_iter()
        .zip(expects.regions)
        .zip(expects.preceding_comment);

    for ((condition, region), preceding_comment) in it {
        // an `expect` does not have a user-defined name, but we'll need a name to call the expectation
        let name = scope.gen_unique_symbol();

        declarations.push_expect(preceding_comment, name, Loc::at(region, condition));
    }

    for (symbol, alias) in aliases.into_iter() {
        output.aliases.insert(symbol, alias);
    }

    // We first perform SCC based on any reference, both variable usage and calls
    // considering both value definitions and function bodies. This will spot any
    // recursive relations between any 2 definitions.
    let sccs = def_ordering.references.strongly_connected_components_all();

    sccs.reorder(&mut defs);

    for (group, is_initial) in sccs.groups().rev() {
        match group.count_ones() {
            1 => {
                // a group with a single Def, nice and simple
                let def = defs.pop().unwrap();
                let index = group.first_one().unwrap();

                if def_ordering.references.get_row_col(index, index) {
                    // push the "header" for this group of recursive definitions
                    let cycle_mark = IllegalCycleMark::new(var_store);
                    declarations.push_recursive_group(1, cycle_mark);

                    // then push the definition
                    let (symbol, specializes) = match def.loc_pattern.value {
                        Pattern::Identifier(symbol) => (symbol, None),

                        Pattern::AbilityMemberSpecialization { ident, specializes } => {
                            (ident, Some(specializes))
                        }

                        _ => {
                            internal_error!("destructures cannot participate in a recursive group; it's always a type error")
                        }
                    };

                    let host_annotation = if exposed_symbols.contains(&symbol) {
                        def.annotation
                            .clone()
                            .map(|a| (var_store.fresh(), a.freshen(var_store)))
                    } else {
                        None
                    };

                    if is_initial && !exposed_symbols.contains(&symbol) {
                        env.problem(Problem::DefsOnlyUsedInRecursion(1, def.region()));
                    }

                    match def.loc_expr.value {
                        Closure(closure_data) => {
                            declarations.push_recursive_def(
                                Loc::at(def.loc_pattern.region, symbol),
                                Loc::at(def.loc_expr.region, closure_data),
                                def.expr_var,
                                def.annotation,
                                host_annotation,
                                specializes,
                            );
                        }
                        _ => {
                            declarations.push_value_def(
                                Loc::at(def.loc_pattern.region, symbol),
                                def.loc_expr,
                                def.expr_var,
                                def.annotation,
                                host_annotation,
                                specializes,
                            );
                        }
                    }
                } else {
                    match def.loc_pattern.value {
                        Pattern::Identifier(symbol) => {
                            let host_annotation = if exposed_symbols.contains(&symbol) {
                                def.annotation
                                    .clone()
                                    .map(|a| (var_store.fresh(), a.freshen(var_store)))
                            } else {
                                None
                            };

                            match def.loc_expr.value {
                                Closure(closure_data) => {
                                    declarations.push_function_def(
                                        Loc::at(def.loc_pattern.region, symbol),
                                        Loc::at(def.loc_expr.region, closure_data),
                                        def.expr_var,
                                        def.annotation,
                                        host_annotation,
                                        None,
                                    );
                                }
                                _ => {
                                    declarations.push_value_def(
                                        Loc::at(def.loc_pattern.region, symbol),
                                        def.loc_expr,
                                        def.expr_var,
                                        def.annotation,
                                        host_annotation,
                                        None,
                                    );
                                }
                            }
                        }
                        Pattern::AbilityMemberSpecialization {
                            ident: symbol,
                            specializes,
                        } => {
                            let host_annotation = if exposed_symbols.contains(&symbol) {
                                def.annotation
                                    .clone()
                                    .map(|a| (var_store.fresh(), a.freshen(var_store)))
                            } else {
                                None
                            };

                            match def.loc_expr.value {
                                Closure(closure_data) => {
                                    declarations.push_function_def(
                                        Loc::at(def.loc_pattern.region, symbol),
                                        Loc::at(def.loc_expr.region, closure_data),
                                        def.expr_var,
                                        def.annotation,
                                        host_annotation,
                                        Some(specializes),
                                    );
                                }
                                _ => {
                                    declarations.push_value_def(
                                        Loc::at(def.loc_pattern.region, symbol),
                                        def.loc_expr,
                                        def.expr_var,
                                        def.annotation,
                                        host_annotation,
                                        Some(specializes),
                                    );
                                }
                            }
                        }
                        _ => {
                            declarations.push_destructure_def(
                                def.loc_pattern,
                                def.loc_expr,
                                def.expr_var,
                                def.annotation,
                                def.pattern_vars.into_iter().collect(),
                            );
                        }
                    }
                }
            }
            group_length => {
                let group_defs = defs.split_off(defs.len() - group_length);

                // push the "header" for this group of recursive definitions
                let cycle_mark = IllegalCycleMark::new(var_store);
                declarations.push_recursive_group(group_length as u16, cycle_mark);

                let mut group_is_initial = is_initial;
                let mut whole_region = None;

                // then push the definitions of this group
                for def in group_defs {
                    let (symbol, specializes) = match def.loc_pattern.value {
                        Pattern::Identifier(symbol) => (symbol, None),

                        Pattern::AbilityMemberSpecialization { ident, specializes } => {
                            (ident, Some(specializes))
                        }

                        _ => {
                            internal_error!("destructures cannot participate in a recursive group; it's always a type error")
                        }
                    };

                    group_is_initial = group_is_initial && !exposed_symbols.contains(&symbol);
                    whole_region = match whole_region {
                        None => Some(def.region()),
                        Some(r) => Some(Region::span_across(&r, &def.region())),
                    };

                    let host_annotation = if exposed_symbols.contains(&symbol) {
                        def.annotation
                            .clone()
                            .map(|a| (var_store.fresh(), a.freshen(var_store)))
                    } else {
                        None
                    };

                    match def.loc_expr.value {
                        Closure(closure_data) => {
                            declarations.push_recursive_def(
                                Loc::at(def.loc_pattern.region, symbol),
                                Loc::at(def.loc_expr.region, closure_data),
                                def.expr_var,
                                def.annotation,
                                host_annotation,
                                specializes,
                            );
                        }
                        _ => {
                            declarations.push_value_def(
                                Loc::at(def.loc_pattern.region, symbol),
                                def.loc_expr,
                                def.expr_var,
                                def.annotation,
                                host_annotation,
                                specializes,
                            );
                        }
                    }
                }

                if group_is_initial {
                    env.problem(Problem::DefsOnlyUsedInRecursion(
                        group_length,
                        whole_region.unwrap(),
                    ));
                }
            }
        }
    }

    (declarations, output)
}

#[inline(always)]
pub(crate) fn sort_can_defs(
    env: &mut Env<'_>,
    var_store: &mut VarStore,
    defs: CanDefs,
    mut output: Output,
) -> (Vec<Declaration>, Output) {
    let CanDefs {
        mut defs,
        dbgs,
        expects,
        def_ordering,
        aliases,
    } = defs;

    for (symbol, alias) in aliases.into_iter() {
        output.aliases.insert(symbol, alias);
    }

    macro_rules! take_def {
        ($index:expr) => {
            match defs[$index].take() {
                Some(def) => def,
                None => {
                    // NOTE: a `_ = someDef` can mean we don't have a symbol here
                    let symbol = def_ordering.get_symbol($index);

                    roc_error_macros::internal_error!("def not available {:?}", symbol)
                }
            }
        };
    }

    // We first perform SCC based on any reference, both variable usage and calls
    // considering both value definitions and function bodies. This will spot any
    // recursive relations between any 2 definitions.
    let sccs = def_ordering.references.strongly_connected_components_all();

    let mut declarations = Vec::with_capacity(defs.len());

    for (group, is_initial) in sccs.groups() {
        if group.count_ones() == 1 {
            // a group with a single Def, nice and simple
            let index = group.iter_ones().next().unwrap();

            let def = take_def!(index);
            let is_specialization = matches!(
                def.loc_pattern.value,
                Pattern::AbilityMemberSpecialization { .. }
            );

            let declaration = if def_ordering.references.get_row_col(index, index) {
                debug_assert!(!is_specialization, "Self-recursive specializations can only be determined during solving - but it was determined for {def:?} now, that's a bug!");

                if is_initial
                    && !def
                        .pattern_vars
                        .keys()
                        .any(|sym| output.references.has_value_lookup(*sym))
                {
                    // This defs is only used in recursion with itself.
                    env.problem(Problem::DefsOnlyUsedInRecursion(1, def.region()));
                }

                // this function calls itself, and must be typechecked as a recursive def
                Declaration::DeclareRec(vec![mark_def_recursive(def)], IllegalCycleMark::empty())
            } else {
                Declaration::Declare(def)
            };

            declarations.push(declaration);
        } else {
            // There is something recursive going on between the Defs of this group.
            // Now we use the direct_references to see if it is clearly invalid recursion, e.g.
            //
            // x = y
            // y = x
            //
            // We allow indirect recursion (behind a lambda), e.g.
            //
            // boom = \{} -> boom {}
            //
            // In general we cannot spot faulty recursion (halting problem), so this is our
            // purely-syntactic heuristic. We'll have a second attempt once we know the types in
            // the cycle.
            let direct_sccs = def_ordering
                .direct_references
                .strongly_connected_components_subset(group);

            debug_assert!(
                !group.iter_ones().any(|index| matches!(defs[index].as_ref().unwrap().loc_pattern.value, Pattern::AbilityMemberSpecialization{..})),
                "A specialization is involved in a recursive cycle - this should not be knowable until solving");

            let declaration = if direct_sccs.groups().count() == 1 {
                // all defs are part of the same direct cycle, that is invalid!
                let mut entries = Vec::with_capacity(group.count_ones());

                for index in group.iter_ones() {
                    let def = take_def!(index);
                    let symbol = def_ordering.get_symbol(index).unwrap();

                    entries.push(make_cycle_entry(symbol, &def))
                }

                let problem = Problem::RuntimeError(RuntimeError::CircularDef(entries.clone()));
                env.problem(problem);

                Declaration::InvalidCycle(entries)
            } else {
                let rec_defs: Vec<Def> = group
                    .iter_ones()
                    .map(|index| mark_def_recursive(take_def!(index)))
                    .collect();

                if is_initial
                    && !rec_defs.iter().any(|def| {
                        def.pattern_vars
                            .keys()
                            .any(|sym| output.references.has_value_lookup(*sym))
                    })
                {
                    // These defs are only used in mutual recursion with themselves.
                    let region = Region::span_across(
                        &rec_defs.first().unwrap().region(),
                        &rec_defs.last().unwrap().region(),
                    );
                    env.problem(Problem::DefsOnlyUsedInRecursion(rec_defs.len(), region));
                }

                Declaration::DeclareRec(rec_defs, IllegalCycleMark::new(var_store))
            };

            declarations.push(declaration);
        }
    }

    if !dbgs.conditions.is_empty() {
        declarations.push(Declaration::Expects(dbgs));
    }

    if !expects.conditions.is_empty() {
        declarations.push(Declaration::Expects(expects));
    }

    (declarations, output)
}

fn mark_def_recursive(mut def: Def) -> Def {
    if let Closure(ClosureData {
        recursive: recursive @ Recursive::NotRecursive,
        ..
    }) = &mut def.loc_expr.value
    {
        *recursive = Recursive::Recursive
    }

    def
}

fn make_cycle_entry(symbol: Symbol, def: &Def) -> CycleEntry {
    CycleEntry {
        symbol,
        symbol_region: def.loc_pattern.region,
        expr_region: def.loc_expr.region,
    }
}

fn pattern_to_vars_by_symbol(
    vars_by_symbol: &mut SendMap<Symbol, Variable>,
    pattern: &Pattern,
    expr_var: Variable,
) {
    use Pattern::*;
    match pattern {
        Identifier(symbol) | Shadowed(_, _, symbol) => {
            vars_by_symbol.insert(*symbol, expr_var);
        }

        As(subpattern, symbol) => {
            vars_by_symbol.insert(*symbol, expr_var);

            pattern_to_vars_by_symbol(vars_by_symbol, &subpattern.value, expr_var);
        }

        AbilityMemberSpecialization {
            ident,
            specializes: _,
        } => {
            vars_by_symbol.insert(*ident, expr_var);
        }

        AppliedTag { arguments, .. } => {
            for (var, nested) in arguments {
                pattern_to_vars_by_symbol(vars_by_symbol, &nested.value, *var);
            }
        }

        UnwrappedOpaque {
            argument, opaque, ..
        } => {
            let (var, nested) = &**argument;
            pattern_to_vars_by_symbol(vars_by_symbol, &nested.value, *var);
            vars_by_symbol.insert(*opaque, expr_var);
        }

        TupleDestructure { destructs, .. } => {
            for destruct in destructs {
                pattern_to_vars_by_symbol(
                    vars_by_symbol,
                    &destruct.value.typ.1.value,
                    destruct.value.typ.0,
                );
            }
        }

        RecordDestructure { destructs, .. } => {
            for destruct in destructs {
                vars_by_symbol.insert(destruct.value.symbol, destruct.value.var);
            }
        }

        List {
            patterns, elem_var, ..
        } => {
            for pat in patterns.patterns.iter() {
                pattern_to_vars_by_symbol(vars_by_symbol, &pat.value, *elem_var);
            }
        }

        NumLiteral(..)
        | IntLiteral(..)
        | FloatLiteral(..)
        | StrLiteral(_)
        | SingleQuote(..)
        | Underscore
        | MalformedPattern(_, _)
        | UnsupportedPattern(_)
        | OpaqueNotInScope(..) => {}
    }
}

fn single_can_def(
    loc_can_pattern: Loc<Pattern>,
    loc_can_expr: Loc<Expr>,
    expr_var: Variable,
    opt_loc_annotation: Option<Loc<crate::annotation::Annotation>>,
    pattern_vars: SendMap<Symbol, Variable>,
    kind: DefKind,
) -> Def {
    let def_annotation = opt_loc_annotation.map(|loc_annotation| Annotation {
        signature: loc_annotation.value.typ,
        introduced_variables: loc_annotation.value.introduced_variables,
        aliases: loc_annotation.value.aliases,
        region: loc_annotation.region,
    });

    Def {
        expr_var,
        loc_pattern: loc_can_pattern,
        loc_expr: Loc {
            region: loc_can_expr.region,
            value: loc_can_expr.value,
        },
        pattern_vars,
        annotation: def_annotation,
        kind,
    }
}

// Functions' references don't count in defs.
// See 3d5a2560057d7f25813112dfa5309956c0f9e6a9 and its
// parent commit for the bug this fixed!
enum DefReferences {
    /// A value may not reference itself
    Value(References),

    /// If the def is a function, different rules apply (it can call itself)
    Function(References),

    /// An annotation without a body references no other defs
    AnnotationWithoutBody,
}

struct DefOutput {
    output: Output,
    def: Def,
    references: DefReferences,
}

// TODO trim down these arguments!
#[allow(clippy::too_many_arguments)]
#[allow(clippy::cognitive_complexity)]
fn canonicalize_pending_value_def<'a>(
    env: &mut Env<'a>,
    pending_def: PendingValueDef<'a>,
    mut output: Output,
    scope: &mut Scope,
    var_store: &mut VarStore,
    pattern_type: PatternType,
    aliases: &mut VecMap<Symbol, Alias>,
) -> DefOutput {
    use PendingValueDef::*;

    // All abilities should be resolved by the time we're canonicalizing value defs.
    let pending_abilities_in_scope = &Default::default();

    let output = match pending_def {
        AnnotationOnly(loc_can_pattern, loc_ann) => {
            // Make types for the body expr, even if we won't end up having a body.
            let expr_var = var_store.fresh();
            let mut vars_by_symbol = SendMap::default();

            // annotation sans body cannot introduce new rigids that are visible in other annotations
            // but the rigids can show up in type error messages, so still register them
            let type_annotation = canonicalize_annotation(
                env,
                scope,
                &loc_ann.value,
                loc_ann.region,
                var_store,
                pending_abilities_in_scope,
                AnnotationFor::Value,
            );

            // Record all the annotation's references in output.references.lookups
            type_annotation.add_to(
                aliases,
                &mut output.references,
                &mut output.introduced_variables,
            );

            pattern_to_vars_by_symbol(&mut vars_by_symbol, &loc_can_pattern.value, expr_var);

            let arity = type_annotation.typ.arity();

            let problem = match &loc_can_pattern.value {
                Pattern::Identifier(symbol) => RuntimeError::NoImplementationNamed {
                    def_symbol: *symbol,
                },
                Pattern::Shadowed(region, loc_ident, _new_symbol) => RuntimeError::Shadowing {
                    original_region: *region,
                    shadow: loc_ident.clone(),
                    kind: ShadowKind::Variable,
                },
                _ => RuntimeError::NoImplementation,
            };

            // Fabricate a body for this annotation, that will error at runtime
            let value = Expr::RuntimeError(problem);
            let is_closure = arity > 0;
            let loc_can_expr = if !is_closure {
                Loc {
                    value,
                    region: loc_ann.region,
                }
            } else {
                let symbol = match &loc_can_pattern.value {
                    Pattern::Identifier(symbol) => *symbol,
                    _ => scope.gen_unique_symbol(),
                };

                // generate a fake pattern for each argument. this makes signatures
                // that are functions only crash when they are applied.
                let mut underscores = Vec::with_capacity(arity);

                for _ in 0..arity {
                    let underscore: Loc<Pattern> = Loc {
                        value: Pattern::Underscore,
                        region: Region::zero(),
                    };

                    underscores.push((
                        var_store.fresh(),
                        AnnotatedMark::known_exhaustive(),
                        underscore,
                    ));
                }

                let body_expr = Loc {
                    value,
                    region: loc_ann.region,
                };

                Loc {
                    value: Closure(ClosureData {
                        function_type: var_store.fresh(),
                        closure_type: var_store.fresh(),
                        return_type: var_store.fresh(),
                        fx_type: var_store.fresh(),
                        early_returns: scope.early_returns.clone(),
                        name: symbol,
                        captured_symbols: Vec::new(),
                        recursive: Recursive::NotRecursive,
                        arguments: underscores,
                        loc_body: Box::new(body_expr),
                    }),
                    region: loc_ann.region,
                }
            };

            let def = single_can_def(
                loc_can_pattern,
                loc_can_expr,
                expr_var,
                Some(Loc::at(loc_ann.region, type_annotation)),
                vars_by_symbol.clone(),
                DefKind::Let,
            );

            DefOutput {
                output,
                references: DefReferences::AnnotationWithoutBody,
                def,
            }
        }

        TypedBody(_loc_pattern, loc_can_pattern, loc_ann, loc_expr) => {
            let type_annotation = canonicalize_annotation(
                env,
                scope,
                &loc_ann.value,
                loc_ann.region,
                var_store,
                pending_abilities_in_scope,
                AnnotationFor::Value,
            );

            // Record all the annotation's references in output.references.lookups
            type_annotation.add_to(
                aliases,
                &mut output.references,
                &mut output.introduced_variables,
            );

            canonicalize_pending_body(
                env,
                output,
                scope,
                var_store,
                loc_can_pattern,
                loc_expr,
                Some(Loc::at(loc_ann.region, type_annotation)),
                DefKind::Let,
            )
        }
        Body(loc_can_pattern, loc_expr) => {
            //
            let def_kind = DefKind::from_pattern(var_store, &loc_can_pattern);

            canonicalize_pending_body(
                env,
                output,
                scope,
                var_store,
                loc_can_pattern,
                loc_expr,
                None,
                def_kind,
            )
        }
        Stmt(loc_expr) => {
            let fx_var = var_store.fresh();
            canonicalize_pending_body(
                env,
                output,
                scope,
                var_store,
                Loc::at(loc_expr.region, Pattern::Underscore),
                loc_expr,
                None,
                DefKind::Stmt(fx_var),
            )
        }
        ImportParams {
            symbol,
            variable,
            loc_pattern,
            module_id,
            opt_provided,
        } => {
            // Insert a reference to the record so that we don't report it as unused
            // If the whole module is unused, we'll report that separately
            output.references.insert_value_lookup(
                SymbolLookup::no_params(symbol),
                QualifiedReference::Unqualified,
            );

            let (opt_var_record, references) = match opt_provided {
                Some(params) => {
                    let (record, can_output) =
                        canonicalize_record(env, var_store, scope, loc_pattern.region, params);

                    let references = can_output.references.clone();
                    output.union(can_output);

                    (Some((variable, Box::new(record))), references)
                }
                None => (None, References::new()),
            };

            let loc_expr = Loc::at(
                loc_pattern.region,
                Expr::ImportParams(module_id, loc_pattern.region, opt_var_record),
            );

            let def = single_can_def(
                loc_pattern,
                loc_expr,
                var_store.fresh(),
                None,
                SendMap::default(),
                DefKind::Let,
            );

            DefOutput {
                output,
                references: DefReferences::Value(references),
                def,
            }
        }
        IngestedFile(loc_pattern, opt_loc_ann, path_literal) => {
            let relative_path =
                if let ast::StrLiteral::PlainLine(ingested_path) = path_literal.value {
                    ingested_path
                } else {
                    todo!(
                    "Only plain strings are supported. Other cases should be made impossible here"
                );
                };

            let mut file_path: PathBuf = env.module_path.into();
            // Remove the header file name and push the new path.
            file_path.pop();
            file_path.push(relative_path);

            let mut bytes = vec![];

            let expr = match fs::File::open(&file_path)
                .and_then(|mut file| file.read_to_end(&mut bytes))
            {
                Ok(_) => Expr::IngestedFile(file_path.into(), Arc::new(bytes), var_store.fresh()),
                Err(e) => {
                    env.problems.push(Problem::FileProblem {
                        filename: file_path.to_path_buf(),
                        error: e.kind(),
                    });

                    Expr::RuntimeError(RuntimeError::ReadIngestedFileError {
                        filename: file_path.to_path_buf(),
                        error: e.kind(),
                        region: path_literal.region,
                    })
                }
            };

            let loc_expr = Loc::at(path_literal.region, expr);

            let opt_loc_can_ann = if let Some(loc_ann) = opt_loc_ann {
                let can_ann = canonicalize_annotation(
                    env,
                    scope,
                    &loc_ann.value,
                    loc_ann.region,
                    var_store,
                    pending_abilities_in_scope,
                    AnnotationFor::Value,
                );

                output.references.union_mut(&can_ann.references);

                Some(Loc::at(loc_ann.region, can_ann))
            } else {
                None
            };

            let def = single_can_def(
                loc_pattern,
                loc_expr,
                var_store.fresh(),
                opt_loc_can_ann,
                SendMap::default(),
                DefKind::Let,
            );

            DefOutput {
                output,
                references: DefReferences::Value(References::new()),
                def,
            }
        }
    };

    // Disallow ability specializations that aren't on the toplevel (note: we might loosen this
    // restriction later on).
    if pattern_type != PatternType::TopLevelDef {
        if let Loc {
            value: Pattern::AbilityMemberSpecialization { specializes, .. },
            region,
        } = output.def.loc_pattern
        {
            env.problem(Problem::NestedSpecialization(specializes, region));
        }
    }

    output
}

// TODO trim down these arguments!
#[allow(clippy::too_many_arguments)]
#[allow(clippy::cognitive_complexity)]
fn canonicalize_pending_body<'a>(
    env: &mut Env<'a>,
    mut output: Output,
    scope: &mut Scope,
    var_store: &mut VarStore,

    loc_can_pattern: Loc<Pattern>,
    loc_expr: &'a Loc<ast::Expr>,

    opt_loc_annotation: Option<Loc<crate::annotation::Annotation>>,
    kind: DefKind,
) -> DefOutput {
    let mut loc_value = &loc_expr.value;

    while let ast::Expr::ParensAround(value) = loc_value {
        loc_value = value;
    }

    let expr_var = var_store.fresh();

    // We treat closure definitions `foo = \a, b -> ...` differently from other body expressions,
    // because they need more bookkeeping (for tail calls, closure captures, etc.)
    //
    // Only defs of the form `foo = ...` can be closure declarations or self tail calls.
    let (loc_can_expr, def_references) = {
        match (&loc_can_pattern.value, &loc_value) {
            (
                Pattern::Identifier(defined_symbol)
                | Pattern::AbilityMemberSpecialization {
                    ident: defined_symbol,
                    ..
                },
                ast::Expr::Closure(arguments, body),
            ) => {
                // bookkeeping for tail-call detection.
                let outer_tailcallable = env.tailcallable_symbol;
                env.tailcallable_symbol = Some(*defined_symbol);

                let (mut closure_data, can_output) = crate::expr::canonicalize_closure(
                    env,
                    var_store,
                    scope,
                    arguments,
                    body,
                    Some(*defined_symbol),
                );

                // reset the tailcallable_symbol
                env.tailcallable_symbol = outer_tailcallable;

                // The closure is self tail recursive iff it tail calls itself (by defined name).
                let is_recursive = if !can_output.tail_calls.is_empty()
                    && can_output
                        .tail_calls
                        .iter()
                        .all(|tail_symbol| tail_symbol == defined_symbol)
                {
                    Recursive::TailRecursive
                } else {
                    Recursive::NotRecursive
                };

                closure_data.recursive = is_recursive;
                closure_data.name = *defined_symbol;

                let loc_can_expr = Loc::at(loc_expr.region, Expr::Closure(closure_data));

                let def_references = DefReferences::Function(can_output.references.clone());
                output.union(can_output);

                (loc_can_expr, def_references)
            }

            // Turn f = .foo into f = \rcd -[f]-> rcd.foo
            (
                Pattern::Identifier(defined_symbol)
                | Pattern::AbilityMemberSpecialization {
                    ident: defined_symbol,
                    ..
                },
                ast::Expr::AccessorFunction(field),
            ) => {
                let field = match field {
                    Accessor::RecordField(field) => IndexOrField::Field((*field).into()),
                    Accessor::TupleIndex(index) => IndexOrField::Index(index.parse().unwrap()),
                };
                let (loc_can_expr, can_output) = (
                    Loc::at(
                        loc_expr.region,
                        RecordAccessor(StructAccessorData {
                            name: *defined_symbol,
                            function_var: var_store.fresh(),
                            record_var: var_store.fresh(),
                            ext_var: var_store.fresh(),
                            closure_var: var_store.fresh(),
                            field_var: var_store.fresh(),
                            field,
                        }),
                    ),
                    Output::default(),
                );
                let def_references = DefReferences::Value(can_output.references.clone());
                output.union(can_output);

                (loc_can_expr, def_references)
            }

            _ => {
                let (loc_can_expr, can_output) =
                    canonicalize_expr(env, var_store, scope, loc_expr.region, &loc_expr.value);

                let def_references = DefReferences::Value(can_output.references.clone());
                output.union(can_output);

                (loc_can_expr, def_references)
            }
        }
    };

    let mut vars_by_symbol = SendMap::default();

    pattern_to_vars_by_symbol(&mut vars_by_symbol, &loc_can_pattern.value, expr_var);

    let def = single_can_def(
        loc_can_pattern,
        loc_can_expr,
        expr_var,
        opt_loc_annotation,
        vars_by_symbol,
        kind,
    );

    DefOutput {
        output,
        references: def_references,
        def,
    }
}

#[inline(always)]
pub fn can_defs_with_return<'a>(
    env: &mut Env<'a>,
    var_store: &mut VarStore,
    scope: &mut Scope,
    loc_defs: &'a mut Defs<'a>,
    loc_ret: &'a Loc<ast::Expr<'a>>,
) -> (Expr, Output) {
    let (unsorted, defs_output, symbols_introduced, imports_introduced) = canonicalize_defs(
        env,
        Output::default(),
        var_store,
        scope,
        loc_defs,
        PatternType::DefExpr,
    );

    // The def as a whole is a tail call iff its return expression is a tail call.
    // Use its output as a starting point because its tail_call already has the right answer!
    let (ret_expr, mut output) =
        canonicalize_expr(env, var_store, scope, loc_ret.region, &loc_ret.value);

    output
        .introduced_variables
        .union(&defs_output.introduced_variables);

    // Sort the defs with the output of the return expression - we'll use this to catch unused defs
    // due only to recursion.
    let (declarations, mut output) = sort_can_defs(env, var_store, unsorted, output);

    output.references.union_mut(&defs_output.references);

    // Now that we've collected all the references, check to see if any of the new idents
    // we defined went unused by the return expression or any other def.
    for (symbol, region) in symbols_introduced {
        if !output.references.has_type_or_value_lookup(symbol)
            && !scope.abilities_store.is_specialization_name(symbol)
        {
            env.problem(Problem::UnusedDef(symbol, region));
        }
    }

    report_unused_imports(imports_introduced, &output.references, env, scope);

    let mut loc_expr: Loc<Expr> = ret_expr;

    for declaration in declarations.into_iter().rev() {
        loc_expr = decl_to_let(declaration, loc_expr);
    }

    (loc_expr.value, output)
}

pub fn report_unused_imports(
    imports_introduced: Vec<IntroducedImport>,
    references: &References,
    env: &mut Env<'_>,
    scope: &mut Scope,
) {
    for import in imports_introduced {
        if references.has_module_lookup(import.module_id) {
            for (symbol, region) in &import.exposed_symbols {
                if !references.has_unqualified_type_or_value_lookup(*symbol)
                    && !scope.abilities_store.is_specialization_name(*symbol)
                {
                    env.problem(Problem::UnusedImport(*symbol, *region));
                }
            }
        } else {
            env.problem(Problem::UnusedModuleImport(import.module_id, import.region));
        }
    }
}

fn decl_to_let(decl: Declaration, loc_ret: Loc<Expr>) -> Loc<Expr> {
    match decl {
        Declaration::Declare(def) => {
            let region = Region::span_across(&def.loc_pattern.region, &loc_ret.region);
            let expr = Expr::LetNonRec(Box::new(def), Box::new(loc_ret));
            Loc::at(region, expr)
        }
        Declaration::DeclareRec(defs, cycle_mark) => {
            let region = Region::span_across(&defs[0].loc_pattern.region, &loc_ret.region);
            let expr = Expr::LetRec(defs, Box::new(loc_ret), cycle_mark);
            Loc::at(region, expr)
        }
        Declaration::InvalidCycle(entries) => {
            Loc::at_zero(Expr::RuntimeError(RuntimeError::CircularDef(entries)))
        }
        Declaration::Expects(expects) => {
            let mut loc_ret = loc_ret;

            let conditions = expects.conditions.into_iter().rev();
            let condition_regions = expects.regions.into_iter().rev();
            let expect_regions = expects.preceding_comment.into_iter().rev();

            let it = expect_regions.zip(condition_regions).zip(conditions);

            for ((expect_region, condition_region), condition) in it {
                let region = Region::span_across(&expect_region, &loc_ret.region);
                let lookups_in_cond = get_lookup_symbols(&condition);

                let expr = Expr::Expect {
                    loc_condition: Box::new(Loc::at(condition_region, condition)),
                    loc_continuation: Box::new(loc_ret),
                    lookups_in_cond,
                };

                loc_ret = Loc::at(region, expr);
            }

            loc_ret
        }
    }
}

fn to_pending_alias_or_opaque<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    name: &'a Loc<&'a str>,
    vars: &'a [Loc<ast::Pattern<'a>>],
    ann: &'a Loc<ast::TypeAnnotation<'a>>,
    opt_derived: Option<&'a Loc<ast::ImplementsAbilities<'a>>>,
    kind: AliasKind,
) -> PendingTypeDef<'a> {
    let region = Region::span_across(&name.region, &ann.region);

    match scope.introduce_without_shadow_symbol(&Ident::from(name.value), region) {
        Ok(symbol) => {
            let mut can_rigids: Vec<Loc<Lowercase>> = Vec::with_capacity(vars.len());

            for loc_var in vars.iter() {
                match loc_var.value {
                    ast::Pattern::Identifier { ident: name, .. }
                        if name.chars().next().unwrap().is_lowercase() =>
                    {
                        let lowercase = Lowercase::from(name);
                        can_rigids.push(Loc {
                            value: lowercase,
                            region: loc_var.region,
                        });
                    }
                    _ => {
                        // any other pattern in this position is a syntax error.
                        let problem = Problem::InvalidAliasRigid {
                            alias_name: symbol,
                            region: loc_var.region,
                        };
                        env.problems.push(problem);

                        return PendingTypeDef::InvalidAlias {
                            kind,
                            symbol,
                            region,
                        };
                    }
                }
            }

            let name_str = name.value;
            let name = Loc {
                region: name.region,
                value: symbol,
            };

            match kind {
                AliasKind::Structural => PendingTypeDef::Alias {
                    name,
                    vars: can_rigids,
                    ann,
                },
                AliasKind::Opaque => PendingTypeDef::Opaque {
                    name_str,
                    name,
                    vars: can_rigids,
                    ann,
                    derived: opt_derived,
                },
            }
        }

        Err((original_sym, original_region, loc_shadowed_symbol)) => {
            let shadow_kind = match kind {
                AliasKind::Structural => ShadowKind::Alias(original_sym),
                AliasKind::Opaque => ShadowKind::Opaque(original_sym),
            };

            env.problem(Problem::Shadowing {
                original_region,
                shadow: loc_shadowed_symbol,
                kind: shadow_kind,
            });

            PendingTypeDef::ShadowedAlias
        }
    }
}

fn to_pending_type_def<'a>(
    env: &mut Env<'a>,
    def: &'a ast::TypeDef<'a>,
    scope: &mut Scope,
    pattern_type: PatternType,
) -> PendingTypeDef<'a> {
    use ast::TypeDef::*;

    match def {
        Alias {
            header: TypeHeader { name, vars },
            ann,
        } => to_pending_alias_or_opaque(env, scope, name, vars, ann, None, AliasKind::Structural),
        Opaque {
            header: TypeHeader { name, vars },
            typ: ann,
            derived,
        } => to_pending_alias_or_opaque(
            env,
            scope,
            name,
            vars,
            ann,
            derived.as_ref(),
            AliasKind::Opaque,
        ),

        Ability {
            header, members, ..
        } if pattern_type != PatternType::TopLevelDef => {
            let header_region = header.region();
            let region = Region::span_across(
                &header_region,
                &members.last().map(|m| m.region()).unwrap_or(header_region),
            );
            env.problem(Problem::AbilityNotOnToplevel { region });

            PendingTypeDef::AbilityNotOnToplevel
        }

        Ability {
            header: TypeHeader { name, vars },
            members,
            loc_implements: _,
        } => {
            let name = match scope
                .introduce_without_shadow_symbol(&Ident::from(name.value), name.region)
            {
                Ok(symbol) => Loc::at(name.region, symbol),
                Err((original_symbol, original_region, shadowed_symbol)) => {
                    env.problem(Problem::Shadowing {
                        original_region,
                        shadow: shadowed_symbol,
                        kind: ShadowKind::Ability(original_symbol),
                    });
                    return PendingTypeDef::AbilityShadows;
                }
            };

            if !vars.is_empty() {
                // Disallow ability type arguments, at least for now.
                let variables_region = Region::across_all(vars.iter().map(|v| &v.region));

                env.problem(Problem::AbilityHasTypeVariables {
                    name: name.value,
                    variables_region,
                });
                return PendingTypeDef::InvalidAbility {
                    symbol: name.value,
                    region: name.region,
                };
            }

            let mut named_members = Vec::with_capacity(members.len());

            for member in *members {
                let name_region = member.name.region;
                let member_name = member.name.extract_spaces().item;

                let member_sym = match scope.introduce(member_name.into(), name_region) {
                    Ok(sym) => sym,
                    Err((shadowed_symbol, shadow, _new_symbol)) => {
                        env.problem(roc_problem::can::Problem::Shadowing {
                            original_region: shadowed_symbol.region,
                            shadow,
                            kind: ShadowKind::Variable,
                        });
                        // Pretend the member isn't a part of the ability
                        continue;
                    }
                };

                named_members.push(PendingAbilityMember {
                    name: Loc::at(name_region, member_sym),
                    typ: member.typ,
                });

                if pattern_type == PatternType::TopLevelDef {
                    env.top_level_symbols.insert(member_sym);
                }
            }

            PendingTypeDef::Ability {
                name,
                members: named_members,
            }
        }
    }
}

enum PendingValue<'a> {
    Def(PendingValueDef<'a>),
    Dbg(PendingExpectOrDbg<'a>),
    Expect(PendingExpectOrDbg<'a>),
    ModuleImport(PendingModuleImport<'a>),
    SignatureDefMismatch,
    InvalidIngestedFile,
    ImportNameConflict,
    StmtAfterExpr,
}

struct PendingExpectOrDbg<'a> {
    condition: &'a Loc<ast::Expr<'a>>,
    preceding_comment: Region,
}

struct PendingModuleImport<'a> {
    module_id: ModuleId,
    region: Region,
    exposed_symbols: Vec<(Symbol, Region)>,
    params: PendingModuleImportParams<'a>,
}

struct PendingModuleImportParams<'a> {
    symbol: Symbol,
    variable: Variable,
    loc_pattern: Loc<Pattern>,
    opt_provided: Option<ast::Collection<'a, Loc<AssignedField<'a, ast::Expr<'a>>>>>,
}

pub struct IntroducedImport {
    module_id: ModuleId,
    region: Region,
    exposed_symbols: Vec<(Symbol, Region)>,
}

#[allow(clippy::too_many_arguments)]
fn to_pending_value_def<'a>(
    env: &mut Env<'a>,
    var_store: &mut VarStore,
    def: &'a ast::ValueDef<'a>,
    region: Region,
    scope: &mut Scope,
    pending_abilities_in_scope: &PendingAbilitiesInScope,
    output: &mut Output,
    pattern_type: PatternType,
) -> PendingValue<'a> {
    use ast::ValueDef::*;

    match def {
        Annotation(loc_pattern, loc_ann) => {
            // This takes care of checking for shadowing and adding idents to scope.
            let loc_can_pattern = canonicalize_def_header_pattern(
                env,
                var_store,
                scope,
                pending_abilities_in_scope,
                output,
                pattern_type,
                &loc_pattern.value,
                loc_pattern.region,
            );

            PendingValue::Def(PendingValueDef::AnnotationOnly(loc_can_pattern, loc_ann))
        }
        Body(loc_pattern, loc_expr) => {
            // This takes care of checking for shadowing and adding idents to scope.
            let loc_can_pattern = canonicalize_def_header_pattern(
                env,
                var_store,
                scope,
                pending_abilities_in_scope,
                output,
                pattern_type,
                &loc_pattern.value,
                loc_pattern.region,
            );

            PendingValue::Def(PendingValueDef::Body(loc_can_pattern, loc_expr))
        }

        AnnotatedBody {
            ann_pattern,
            ann_type,
            lines_between: _,
            body_pattern,
            body_expr,
        } => {
            if ann_pattern.value.equivalent(&body_pattern.value) {
                // NOTE: Pick the body pattern, picking the annotation one is
                // incorrect in the presence of optional record fields!
                //
                // { x, y } : { x : Int, y ? Bool }*
                // { x, y ? False } = rec
                //
                // This takes care of checking for shadowing and adding idents to scope.
                let loc_can_pattern = canonicalize_def_header_pattern(
                    env,
                    var_store,
                    scope,
                    pending_abilities_in_scope,
                    output,
                    pattern_type,
                    &body_pattern.value,
                    body_pattern.region,
                );

                PendingValue::Def(PendingValueDef::TypedBody(
                    body_pattern,
                    loc_can_pattern,
                    ann_type,
                    body_expr,
                ))
            } else {
                // the pattern of the annotation does not match the pattern of the body direc
                env.problems.push(Problem::SignatureDefMismatch {
                    annotation_pattern: ann_pattern.region,
                    def_pattern: body_pattern.region,
                });

                // TODO: Should we instead build some PendingValueDef::InvalidAnnotatedBody ? This would
                // remove the `Option` on this function (and be probably more reliable for further
                // problem/error reporting)
                PendingValue::SignatureDefMismatch
            }
        }

        Dbg {
            condition,
            preceding_comment,
        } => PendingValue::Dbg(PendingExpectOrDbg {
            condition,
            preceding_comment: *preceding_comment,
        }),

        Expect {
            condition,
            preceding_comment,
        } => PendingValue::Expect(PendingExpectOrDbg {
            condition,
            preceding_comment: *preceding_comment,
        }),

        ModuleImport(module_import) => {
            let qualified_module_name: QualifiedModuleName = module_import.name.value.into();
            let module_name = qualified_module_name.module.clone();
            let pq_module_name = qualified_module_name.into_pq_module_name(env.opt_shorthand);

            let module_id = env
                .qualified_module_ids
                .get_id(&pq_module_name)
                .expect("Module id should have been added in load");

            let name_with_alias = match module_import.alias {
                Some(alias) => ModuleName::from(alias.item.value.as_str()),
                None => module_name.clone(),
            };

            // Generate a symbol for the module params def
            // We do this even if params weren't provided so that solve can report if they are missing
            let params_sym = scope.gen_unique_symbol();
            let params_region = module_import
                .params
                .map(|p| p.params.region)
                .unwrap_or(region);
            let params_var = var_store.fresh();
            let params = PendingModuleImportParams {
                symbol: params_sym,
                variable: params_var,
                loc_pattern: Loc::at(params_region, Pattern::Identifier(params_sym)),
                opt_provided: module_import.params.map(|p| p.params.value),
            };
            let provided_params = if module_import.params.is_some() {
                // Only add params to scope if they are provided
                Some((params_var, params_sym))
            } else {
                None
            };

            if let Err(existing_import) =
                scope
                    .modules
                    .insert(name_with_alias.clone(), module_id, provided_params, region)
            {
                env.problems.push(Problem::ImportNameConflict {
                    name: name_with_alias,
                    is_alias: module_import.alias.is_some(),
                    new_module_id: module_id,
                    new_import_region: region,
                    existing_import,
                });

                return PendingValue::ImportNameConflict;
            };

            let exposed_names = module_import
                .exposed
                .map(|kw| kw.item.items)
                .unwrap_or_default();

            if exposed_names.is_empty()
                && !env.home.is_builtin()
                && module_id.is_automatically_imported()
            {
                env.problems
                    .push(Problem::ExplicitBuiltinImport(module_id, region));
            }

            let exposed_ids = env
                .dep_idents
                .get(&module_id)
                .expect("Module id should have been added in load");

            let mut exposed_symbols = Vec::with_capacity(exposed_names.len());

            for loc_name in exposed_names {
                let exposed_name = loc_name.value.item();
                let name = exposed_name.as_str();
                let ident = Ident::from(name);

                match exposed_ids.get_id(name) {
                    Some(ident_id) => {
                        let symbol = Symbol::new(module_id, ident_id);
                        exposed_symbols.push((symbol, loc_name.region));

                        if let Err((_shadowed_symbol, existing_symbol_region)) =
                            scope.import_symbol(ident, symbol, loc_name.region)
                        {
                            if symbol.is_automatically_imported() {
                                env.problem(Problem::ExplicitBuiltinTypeImport(
                                    symbol,
                                    loc_name.region,
                                ));
                            } else {
                                env.problem(Problem::ImportShadowsSymbol {
                                    region: loc_name.region,
                                    new_symbol: symbol,
                                    existing_symbol_region,
                                })
                            }
                        }
                    }
                    None => env.problem(Problem::RuntimeError(RuntimeError::ValueNotExposed {
                        module_name: module_name.clone(),
                        ident,
                        region: loc_name.region,
                        exposed_values: exposed_ids.exposed_values(),
                    })),
                }
            }

            PendingValue::ModuleImport(PendingModuleImport {
                module_id,
                region,
                exposed_symbols,
                params,
            })
        }
        IngestedFileImport(ingested_file) => {
            let loc_name = ingested_file.name.item;

            let symbol = match scope.introduce(loc_name.value.into(), loc_name.region) {
                Ok(symbol) => symbol,
                Err((original, shadow, _)) => {
                    env.problem(Problem::Shadowing {
                        original_region: original.region,
                        shadow,
                        kind: ShadowKind::Variable,
                    });

                    return PendingValue::InvalidIngestedFile;
                }
            };

            let loc_pattern = Loc::at(loc_name.region, Pattern::Identifier(symbol));

            PendingValue::Def(PendingValueDef::IngestedFile(
                loc_pattern,
                ingested_file.annotation.map(|ann| ann.annotation),
                ingested_file.path,
            ))
        }
        StmtAfterExpr => PendingValue::StmtAfterExpr,
        Stmt(expr) => {
            if env.fx_mode == FxMode::Task {
                internal_error!("a Stmt was not desugared correctly, should have been converted to a Body(...) in desguar")
            }

            PendingValue::Def(PendingValueDef::Stmt(expr))
        }
    }
}

/// Make aliases recursive
fn correct_mutual_recursive_type_alias(
    env: &mut Env,
    original_aliases: VecMap<Symbol, Alias>,
    var_store: &mut VarStore,
) -> VecMap<Symbol, Alias> {
    let capacity = original_aliases.len();
    let mut matrix = ReferenceMatrix::new(capacity);

    let (symbols_introduced, mut aliases) = original_aliases.unzip();

    for (index, alias) in aliases.iter().enumerate() {
        for referenced in alias.typ.symbols() {
            match symbols_introduced.iter().position(|k| referenced == *k) {
                None => { /* ignore */ }
                Some(ref_id) => matrix.set_row_col(index, ref_id, true),
            }
        }
    }

    let mut solved_aliases = bitvec::vec::BitVec::<usize>::repeat(false, capacity);

    let sccs = matrix.strongly_connected_components_all();

    // scratchpad to store aliases that are modified in the current iteration.
    // Only used when there is are more than one alias in a group. See below why
    // this is needed.
    let scratchpad_capacity = sccs
        .groups()
        .map(|(r, _)| r.count_ones())
        .max()
        .unwrap_or_default();
    let mut scratchpad = Vec::with_capacity(scratchpad_capacity);

    for (cycle, _is_initial) in sccs.groups() {
        debug_assert!(cycle.count_ones() > 0);

        // We need to instantiate the alias with any symbols in the currrent module it
        // depends on.
        //
        // the `strongly_connected_components` returns SCCs in a topologically sorted order:
        // SCC_0 has those aliases that don't rely on any other, SCC_1 has only those that rely on SCC_1, etc.
        //
        // Hence, we only need to worry about symbols in the current SCC or any prior one.
        // It cannot be using any of the others, and we've already instantiated aliases coming from other modules.
        let mut to_instantiate = solved_aliases | cycle;

        // Make sure we report only one error for the cycle, not an error for every
        // alias in the cycle.
        let mut can_still_report_error = true;

        for index in cycle.iter_ones() {
            // Don't try to instantiate the alias itself in its own definition.
            to_instantiate.set(index, false);

            // Within a recursive group, we must instantiate all aliases like how they came to the
            // loop. e.g. given
            //
            // A : [ConsA B, NilA]
            // B : [ConsB A, NilB]
            //
            // Our goal is
            //
            // A : [ConsA [ConsB A, NilB], NilA]
            // B : [ConsB [ConsA B, NilA], NilB]
            //
            // But if we would first instantiate B into A, then use the updated A to instantiate B,
            // we get
            //
            // A : [ConsA [ConsB A, NilB], NilA]
            // B : [ConsB [ConsA [ConsB A, NilB], NilA], NilB]
            //
            // Which is incorrect. We do need the instantiated version however.
            // e.g. if in a next group we have:
            //
            // C : A
            //
            // Then we must use the instantiated version
            //
            // C : [ConsA [ConsB A, NilB], NilA]
            //
            // So, we cannot replace the original version of A with its instantiated version
            // while we process A's group. We have to store the instantiated version until the
            // current group is done, then move it to the `aliases` array. That is what the scratchpad is for.
            let alias = if cycle.count_ones() == 1 {
                // an optimization: we can modify the alias in the `aliases` list directly
                // because it is the only alias in the group.
                &mut aliases[index]
            } else {
                scratchpad.push((index, aliases[index].clone()));

                &mut scratchpad.last_mut().unwrap().1
            };

            // Now, `alias` is possibly a mutable borrow from the `aliases` vector. But we also want
            // to immutably borrow other elements from that vector to instantiate them into `alias`.
            // The borrow checker disallows that.
            //
            // So we get creative: we swap out the element we want to modify with a dummy. We can
            // then freely modify the type we moved out, and the `to_instantiate` mask
            // makes sure that our dummy is not used.

            let alias_region = alias.region;
            let mut alias_type = Type::EmptyRec;

            std::mem::swap(&mut alias_type, &mut alias.typ);

            let can_instantiate_symbol = |s| match symbols_introduced.iter().position(|i| *i == s) {
                Some(s_index) if to_instantiate[s_index] => aliases.get(s_index),
                _ => None,
            };

            let mut new_lambda_sets = ImSet::default();
            let mut new_recursion_variables = ImSet::default();
            let mut new_infer_ext_vars = ImSet::default();
            alias_type.instantiate_aliases(
                alias_region,
                &can_instantiate_symbol,
                var_store,
                &mut new_lambda_sets,
                &mut new_recursion_variables,
                &mut new_infer_ext_vars,
            );

            let alias = if cycle.count_ones() > 1 {
                &mut scratchpad.last_mut().unwrap().1
            } else {
                &mut aliases[index]
            };

            // swap the type back
            std::mem::swap(&mut alias_type, &mut alias.typ);

            // We can instantiate this alias in future iterations
            to_instantiate.set(index, true);

            // add any lambda sets that the instantiation created to the current alias
            alias.lambda_set_variables.extend(
                new_lambda_sets
                    .iter()
                    .map(|var| LambdaSet(Type::Variable(*var))),
            );

            // add any new recursion variables
            alias.recursion_variables.extend(new_recursion_variables);

            // add any new infer-in-output extension variables that the instantiation created to the current alias
            alias
                .infer_ext_in_output_variables
                .extend(new_infer_ext_vars);

            // Now mark the alias recursive, if it needs to be.
            let rec = symbols_introduced[index];
            let is_self_recursive = cycle.count_ones() == 1 && matrix.get_row_col(index, index);
            let is_mutually_recursive = cycle.count_ones() > 1;

            if is_self_recursive || is_mutually_recursive {
                let _made_recursive = make_tag_union_of_alias_recursive(
                    env,
                    rec,
                    alias,
                    vec![],
                    var_store,
                    &mut can_still_report_error,
                );
            }
        }

        // the current group has instantiated. Now we can move the updated aliases to the `aliases` vector
        for (index, alias) in scratchpad.drain(..) {
            aliases[index] = alias;
        }

        // The cycle we just instantiated and marked recursive may still be an illegal cycle, if
        // all the types in the cycle are narrow newtypes. We can't figure this out until now,
        // because we need all the types to be deeply instantiated.
        let all_are_narrow = cycle.iter_ones().all(|index| {
            let typ = &aliases[index].typ;
            matches!(typ, Type::RecursiveTagUnion(..)) && typ.is_narrow()
        });

        if all_are_narrow {
            // This cycle is illegal!
            let mut indices = cycle.iter_ones();
            let first_index = indices.next().unwrap();

            let rest: Vec<Symbol> = indices.map(|i| symbols_introduced[i]).collect();

            let alias_name = symbols_introduced[first_index];
            let alias = aliases.get_mut(first_index).unwrap();

            mark_cyclic_alias(
                env,
                &mut alias.typ,
                alias_name,
                alias.kind,
                alias.region,
                rest,
                can_still_report_error,
            )
        }

        // We've instantiated all we could, so all instantiatable aliases are solved now
        solved_aliases = to_instantiate;
    }

    // Safety: both vectors are equal length and there are no duplicates
    unsafe { VecMap::zip(symbols_introduced, aliases) }
}

fn make_tag_union_of_alias_recursive(
    env: &mut Env,
    alias_name: Symbol,
    alias: &mut Alias,
    others: Vec<Symbol>,
    var_store: &mut VarStore,
    can_report_cyclic_error: &mut bool,
) -> Result<(), ()> {
    let alias_args = alias
        .type_variables
        .iter()
        .map(|l| Type::Variable(l.value.var));

    let alias_opt_able_vars = alias.type_variables.iter().map(|l| OptAbleType {
        typ: Type::Variable(l.value.var),
        opt_abilities: l.value.opt_bound_abilities.clone(),
    });

    let lambda_set_vars = alias.lambda_set_variables.iter();
    let infer_ext_in_output_variables = alias
        .infer_ext_in_output_variables
        .iter()
        .map(|v| Type::Variable(*v));

    let made_recursive = make_tag_union_recursive_help(
        env,
        Loc::at(alias.header_region(), alias_name),
        alias_args,
        alias_opt_able_vars,
        lambda_set_vars,
        infer_ext_in_output_variables,
        alias.kind,
        alias.region,
        others,
        &mut alias.typ,
        var_store,
        can_report_cyclic_error,
    );

    match made_recursive {
        MakeTagUnionRecursive::Cyclic => Ok(()),
        MakeTagUnionRecursive::MadeRecursive { recursion_variable } => {
            alias.recursion_variables.clear();
            alias.recursion_variables.insert(recursion_variable);

            Ok(())
        }
        MakeTagUnionRecursive::InvalidRecursion => Err(()),
    }
}

enum MakeTagUnionRecursive {
    Cyclic,
    MadeRecursive { recursion_variable: Variable },
    InvalidRecursion,
}

/// Attempt to make a tag union recursive at the position of `recursive_alias`; for example,
///
/// ```roc
/// [Cons a (ConsList a), Nil] as ConsList a
/// ```
///
/// can be made recursive at the position "ConsList a" with a fresh recursive variable, say r1:
///
/// ```roc
/// [Cons a r1, Nil] as r1
/// ```
///
/// Returns `Err` if the tag union is recursive, but there is no structure-preserving recursion
/// variable for it. This can happen when the type is a nested datatype, for example in either of
///
/// ```roc
/// Nested a : [Chain a (Nested (List a)), Term]
/// DuoList a b : [Cons a (DuoList b a), Nil]
/// ```
///
/// When `Err` is returned, a problem will be added to `env`.
#[allow(clippy::too_many_arguments)]
fn make_tag_union_recursive_help<'a, 'b>(
    env: &mut Env<'a>,
    recursive_alias: Loc<Symbol>,
    alias_args: impl Iterator<Item = Type>,
    alias_opt_able_vars: impl Iterator<Item = OptAbleType>,
    lambda_set_variables: impl Iterator<Item = &'b LambdaSet>,
    infer_ext_in_output_variables: impl Iterator<Item = Type>,
    alias_kind: AliasKind,
    region: Region,
    others: Vec<Symbol>,
    typ: &'b mut Type,
    var_store: &mut VarStore,
    can_report_cyclic_error: &mut bool,
) -> MakeTagUnionRecursive {
    use MakeTagUnionRecursive::*;

    let symbol = recursive_alias.value;
    let alias_region = recursive_alias.region;

    match typ {
        Type::TagUnion(tags, ext) => {
            let recursion_variable = var_store.fresh();
            let type_arguments: Vec<_> = alias_args.collect();

            let mut pending_typ =
                Type::RecursiveTagUnion(recursion_variable, tags.to_vec(), ext.clone());

            let substitution = match alias_kind {
                // Inline recursion var directly wherever the alias is used.
                AliasKind::Structural => Type::Variable(recursion_variable),
                // Wrap the recursion var in the opaque wherever it's used to avoid leaking the
                // inner type out as structural.
                AliasKind::Opaque => Type::Alias {
                    symbol,
                    type_arguments: alias_opt_able_vars.collect(),
                    lambda_set_variables: lambda_set_variables.cloned().collect(),
                    infer_ext_in_output_types: infer_ext_in_output_variables.collect(),
                    actual: Box::new(Type::Variable(recursion_variable)),
                    kind: AliasKind::Opaque,
                },
            };

            let substitution_result =
                pending_typ.substitute_alias(symbol, &type_arguments, &substitution);
            match substitution_result {
                Ok(()) => {
                    // We can substitute the alias presence for the variable exactly.
                    *typ = pending_typ;

                    MadeRecursive { recursion_variable }
                }
                Err(differing_recursion_region) => {
                    env.problems.push(Problem::NestedDatatype {
                        alias: symbol,
                        def_region: alias_region,
                        differing_recursion_region,
                    });

                    InvalidRecursion
                }
            }
        }
        Type::RecursiveTagUnion(recursion_variable, _, _) => MadeRecursive {
            recursion_variable: *recursion_variable,
        },
        Type::Alias {
            actual,
            type_arguments,
            lambda_set_variables,
            infer_ext_in_output_types,
            kind,
            ..
        } => {
            // NB: We need to collect the type arguments to shut off rustc's closure type
            // instantiator. Otherwise we get unfortunate errors like
            //   reached the recursion limit while instantiating `make_tag_union_recursive_help::<...n/src/def.rs:1879:65: 1879:77]>>`
            #[allow(clippy::needless_collect)]
            let alias_args: Vec<Type> = type_arguments.iter().map(|ta| ta.typ.clone()).collect();
            let recursive_alias = Loc::at_zero(symbol);

            // try to make `actual` recursive
            make_tag_union_recursive_help(
                env,
                recursive_alias,
                alias_args.into_iter(),
                type_arguments.iter().cloned(),
                lambda_set_variables.iter(),
                infer_ext_in_output_types.iter().cloned(),
                *kind,
                region,
                others,
                actual,
                var_store,
                can_report_cyclic_error,
            )
        }
        _ => {
            // take care to report a cyclic alias only once (not once for each alias in the cycle)
            mark_cyclic_alias(
                env,
                typ,
                symbol,
                alias_kind,
                region,
                others,
                *can_report_cyclic_error,
            );
            *can_report_cyclic_error = false;

            Cyclic
        }
    }
}

fn mark_cyclic_alias(
    env: &mut Env,
    typ: &mut Type,
    symbol: Symbol,
    alias_kind: AliasKind,
    region: Region,
    others: Vec<Symbol>,
    report: bool,
) {
    *typ = Type::Error;

    if report {
        let problem = Problem::CyclicAlias(symbol, region, others, alias_kind);
        env.problems.push(problem);
    }
}
