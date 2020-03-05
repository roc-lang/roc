use crate::subs::Content::{self, *};
use crate::subs::{Descriptor, FlatType, Mark, OptVariable, Subs, Variable};
use crate::types::{Mismatch, Problem};
use crate::uniqueness::boolean_algebra::{Atom, Bool};
use roc_collections::all::{relative_complement, union, MutMap, SendSet};
use roc_module::ident::{Lowercase, TagName};
use roc_module::symbol::Symbol;
use std::hash::Hash;

macro_rules! mismatch {
    () => {{
        if cfg!(debug_assertions) {
            println!(
                "Mismatch in {} Line {} Column {}",
                file!(),
                line!(),
                column!()
            );
        }
        vec![Mismatch::TypeMismatch]
    }};
}

type Pool = Vec<Variable>;

struct Context {
    first: Variable,
    first_desc: Descriptor,
    second: Variable,
    second_desc: Descriptor,
}

pub struct RecordStructure {
    pub fields: MutMap<Lowercase, Variable>,
    pub ext: Variable,
}

#[derive(Debug)]
struct TagUnionStructure {
    tags: MutMap<TagName, Vec<Variable>>,
    ext: Variable,
}

pub struct Unified {
    pub vars: Pool,
    pub mismatches: Vec<Problem>,
}

type Outcome = Vec<Mismatch>;

#[inline(always)]
pub fn unify(subs: &mut Subs, var1: Variable, var2: Variable) -> Unified {
    let mut vars = Vec::new();
    let mismatches = unify_pool(subs, &mut vars, var1, var2)
        .into_iter()
        .map(|problem| {
            let type1 = subs.var_to_error_type(var1);
            let type2 = subs.var_to_error_type(var2);

            subs.union(var1, var2, Content::Error.into());

            Problem::Mismatch(problem, type1, type2)
        })
        .collect();

    Unified { vars, mismatches }
}

#[inline(always)]
pub fn unify_pool(subs: &mut Subs, pool: &mut Pool, var1: Variable, var2: Variable) -> Outcome {
    if subs.equivalent(var1, var2) {
        Vec::new()
    } else {
        let ctx = Context {
            first: var1,
            first_desc: subs.get(var1),
            second: var2,
            second_desc: subs.get(var2),
        };

        unify_context(subs, pool, ctx)
    }
}

fn unify_context(subs: &mut Subs, pool: &mut Pool, ctx: Context) -> Outcome {
    // println!( "{:?} {:?} ~ {:?} {:?}", ctx.first, ctx.first_desc.content, ctx.second, ctx.second_desc.content);
    match &ctx.first_desc.content {
        FlexVar(opt_name) => unify_flex(subs, pool, &ctx, opt_name, &ctx.second_desc.content),
        RigidVar(name) => unify_rigid(subs, &ctx, name, &ctx.second_desc.content),
        Structure(flat_type) => {
            unify_structure(subs, pool, &ctx, flat_type, &ctx.second_desc.content)
        }
        Alias(symbol, args, real_var) => unify_alias(subs, pool, &ctx, *symbol, args, *real_var),
        Error => {
            // Error propagates. Whatever we're comparing it to doesn't matter!
            merge(subs, &ctx, Error)
        }
    }
}

#[inline(always)]
fn unify_alias(
    subs: &mut Subs,
    pool: &mut Pool,
    ctx: &Context,
    symbol: Symbol,
    args: &[(Lowercase, Variable)],
    real_var: Variable,
) -> Outcome {
    let other_content = &ctx.second_desc.content;

    match other_content {
        FlexVar(_) => {
            // Alias wins
            merge(subs, &ctx, Alias(symbol, args.to_owned(), real_var))
        }
        RigidVar(_) => unify_pool(subs, pool, real_var, ctx.second),
        Alias(other_symbol, other_args, other_real_var) => {
            if symbol == *other_symbol {
                if args.len() == other_args.len() {
                    let mut problems = Vec::new();
                    for ((_, l_var), (_, r_var)) in args.iter().zip(other_args.iter()) {
                        problems.extend(unify_pool(subs, pool, *l_var, *r_var));
                    }

                    problems.extend(merge(subs, &ctx, other_content.clone()));

                    problems
                } else {
                    mismatch!()
                }
            } else {
                unify_pool(subs, pool, real_var, *other_real_var)
            }
        }
        Structure(_) => unify_pool(subs, pool, real_var, ctx.second),
        Error => merge(subs, ctx, Error),
    }
}

#[inline(always)]
fn unify_structure(
    subs: &mut Subs,
    pool: &mut Pool,
    ctx: &Context,
    flat_type: &FlatType,
    other: &Content,
) -> Outcome {
    match other {
        FlexVar(_) => {
            // TODO special-case boolean here
            match flat_type {
                FlatType::Boolean(Bool(Atom::Variable(var), _rest)) => {
                    unify_pool(subs, pool, *var, ctx.second)
                }
                _ => {
                    // If the other is flex, Structure wins!
                    merge(subs, ctx, Structure(flat_type.clone()))
                }
            }
        }
        RigidVar(_) => {
            // Type mismatch! Rigid can only unify with flex.
            mismatch!()
        }

        Structure(ref other_flat_type) => {
            // Unify the two flat types
            unify_flat_type(subs, pool, ctx, flat_type, other_flat_type)
        }
        Alias(_, _, real_var) => unify_pool(subs, pool, ctx.first, *real_var),
        Error => merge(subs, ctx, Error),
    }
}

/// Like intersection_with, except for MutMap and specialized to return
/// a tuple. Also, only clones the values that will be actually returned,
/// rather than cloning everything.
fn get_shared<K, V>(map1: &MutMap<K, V>, map2: &MutMap<K, V>) -> MutMap<K, (V, V)>
where
    K: Clone + Eq + Hash,
    V: Clone,
{
    let mut answer = MutMap::default();

    for (key, right_value) in map2 {
        match std::collections::HashMap::get(map1, &key) {
            None => (),
            Some(left_value) => {
                answer.insert(key.clone(), (left_value.clone(), right_value.clone()));
            }
        }
    }

    answer
}

fn unify_record(
    subs: &mut Subs,
    pool: &mut Pool,
    ctx: &Context,
    rec1: RecordStructure,
    rec2: RecordStructure,
) -> Outcome {
    let fields1 = rec1.fields;
    let fields2 = rec2.fields;
    let shared_fields = get_shared(&fields1, &fields2);
    // NOTE: don't use `difference` here. In contrast to Haskell, im's `difference` is symmetric
    let unique_fields1 = relative_complement(&fields1, &fields2);
    let unique_fields2 = relative_complement(&fields2, &fields1);

    if unique_fields1.is_empty() {
        if unique_fields2.is_empty() {
            let ext_problems = unify_pool(subs, pool, rec1.ext, rec2.ext);
            let other_fields = MutMap::default();
            let mut field_problems =
                unify_shared_fields(subs, pool, ctx, shared_fields, other_fields, rec1.ext);

            field_problems.extend(ext_problems);

            field_problems
        } else {
            let flat_type = FlatType::Record(unique_fields2, rec2.ext);
            let sub_record = fresh(subs, pool, ctx, Structure(flat_type));
            let ext_problems = unify_pool(subs, pool, rec1.ext, sub_record);
            let other_fields = MutMap::default();
            let mut field_problems =
                unify_shared_fields(subs, pool, ctx, shared_fields, other_fields, sub_record);

            field_problems.extend(ext_problems);

            field_problems
        }
    } else if unique_fields2.is_empty() {
        let flat_type = FlatType::Record(unique_fields1, rec1.ext);
        let sub_record = fresh(subs, pool, ctx, Structure(flat_type));
        let ext_problems = unify_pool(subs, pool, sub_record, rec2.ext);
        let other_fields = MutMap::default();
        let mut field_problems =
            unify_shared_fields(subs, pool, ctx, shared_fields, other_fields, sub_record);

        field_problems.extend(ext_problems);

        field_problems
    } else {
        let other_fields = union(unique_fields1.clone(), &unique_fields2);

        let ext = fresh(subs, pool, ctx, Content::FlexVar(None));
        let flat_type1 = FlatType::Record(unique_fields1, rec1.ext);
        let flat_type2 = FlatType::Record(unique_fields2, rec2.ext);

        let sub1 = fresh(subs, pool, ctx, Structure(flat_type1));
        let sub2 = fresh(subs, pool, ctx, Structure(flat_type2));

        let rec1_problems = unify_pool(subs, pool, rec1.ext, sub2);
        let rec2_problems = unify_pool(subs, pool, sub1, rec2.ext);

        let mut field_problems =
            unify_shared_fields(subs, pool, ctx, shared_fields, other_fields, ext);

        field_problems.reserve(rec1_problems.len() + rec2_problems.len());
        field_problems.extend(rec1_problems);
        field_problems.extend(rec2_problems);

        field_problems
    }
}

fn unify_shared_fields(
    subs: &mut Subs,
    pool: &mut Pool,
    ctx: &Context,
    shared_fields: MutMap<Lowercase, (Variable, Variable)>,
    other_fields: MutMap<Lowercase, Variable>,
    ext: Variable,
) -> Outcome {
    let mut matching_fields = MutMap::default();
    let num_shared_fields = shared_fields.len();

    for (name, (actual, expected)) in shared_fields {
        let problems = unify_pool(subs, pool, actual, expected);

        if problems.is_empty() {
            matching_fields.insert(name, actual);
        }
    }

    if num_shared_fields == matching_fields.len() {
        let flat_type = FlatType::Record(union(matching_fields, &other_fields), ext);

        merge(subs, ctx, Structure(flat_type))
    } else {
        mismatch!()
    }
}

fn unify_tag_union(
    subs: &mut Subs,
    pool: &mut Pool,
    ctx: &Context,
    rec1: TagUnionStructure,
    rec2: TagUnionStructure,
    recursion: (Option<Variable>, Option<Variable>),
) -> Outcome {
    let tags1 = rec1.tags;
    let tags2 = rec2.tags;
    let shared_tags = get_shared(&tags1, &tags2);
    // NOTE: don't use `difference` here. In contrast to Haskell, im's `difference` is symmetric
    let unique_tags1 = relative_complement(&tags1, &tags2);
    let unique_tags2 = relative_complement(&tags2, &tags1);

    let recursion_var = match recursion {
        (None, None) => None,
        (Some(v), None) | (None, Some(v)) => Some(v),
        (Some(v1), Some(v2)) => {
            unify_pool(subs, pool, v1, v2);
            Some(v1)
        }
    };

    if unique_tags1.is_empty() {
        if unique_tags2.is_empty() {
            let ext_problems = unify_pool(subs, pool, rec1.ext, rec2.ext);
            let mut tag_problems = unify_shared_tags(
                subs,
                pool,
                ctx,
                shared_tags,
                MutMap::default(),
                rec1.ext,
                recursion_var,
            );

            tag_problems.extend(ext_problems);

            tag_problems
        } else {
            let flat_type = FlatType::TagUnion(unique_tags2, rec2.ext);
            let sub_record = fresh(subs, pool, ctx, Structure(flat_type));
            let ext_problems = unify_pool(subs, pool, rec1.ext, sub_record);
            let mut tag_problems = unify_shared_tags(
                subs,
                pool,
                ctx,
                shared_tags,
                MutMap::default(),
                sub_record,
                recursion_var,
            );

            tag_problems.extend(ext_problems);

            tag_problems
        }
    } else if unique_tags2.is_empty() {
        let flat_type = FlatType::TagUnion(unique_tags1, rec1.ext);
        let sub_record = fresh(subs, pool, ctx, Structure(flat_type));
        let ext_problems = unify_pool(subs, pool, sub_record, rec2.ext);
        let mut tag_problems = unify_shared_tags(
            subs,
            pool,
            ctx,
            shared_tags,
            MutMap::default(),
            sub_record,
            recursion_var,
        );

        tag_problems.extend(ext_problems);

        tag_problems
    } else {
        let other_tags = union(unique_tags1.clone(), &unique_tags2);

        let ext = fresh(subs, pool, ctx, Content::FlexVar(None));
        let flat_type1 = FlatType::TagUnion(unique_tags1, rec1.ext);
        let flat_type2 = FlatType::TagUnion(unique_tags2, rec2.ext);

        let sub1 = fresh(subs, pool, ctx, Structure(flat_type1));
        let sub2 = fresh(subs, pool, ctx, Structure(flat_type2));

        let rec1_problems = unify_pool(subs, pool, rec1.ext, sub2);
        let rec2_problems = unify_pool(subs, pool, sub1, rec2.ext);

        let mut tag_problems =
            unify_shared_tags(subs, pool, ctx, shared_tags, other_tags, ext, recursion_var);

        tag_problems.reserve(rec1_problems.len() + rec2_problems.len());
        tag_problems.extend(rec1_problems);
        tag_problems.extend(rec2_problems);

        tag_problems
    }
}

fn unify_shared_tags(
    subs: &mut Subs,
    pool: &mut Pool,
    ctx: &Context,
    shared_tags: MutMap<TagName, (Vec<Variable>, Vec<Variable>)>,
    other_tags: MutMap<TagName, Vec<Variable>>,
    ext: Variable,
    recursion_var: Option<Variable>,
) -> Outcome {
    let mut matching_tags = MutMap::default();
    let num_shared_tags = shared_tags.len();

    for (name, (actual_vars, expected_vars)) in shared_tags {
        let mut matching_vars = Vec::with_capacity(actual_vars.len());

        let actual_len = actual_vars.len();
        let expected_len = expected_vars.len();

        for (actual, expected) in actual_vars.into_iter().zip(expected_vars.into_iter()) {
            let problems = unify_pool(subs, pool, actual, expected);

            if problems.is_empty() {
                matching_vars.push(actual);
            }
        }

        // only do this check after unification so the error message has more info
        if actual_len == expected_len && actual_len == matching_vars.len() {
            matching_tags.insert(name, matching_vars);
        }
    }

    if num_shared_tags == matching_tags.len() {
        let flat_type = if let Some(rec) = recursion_var {
            FlatType::RecursiveTagUnion(rec, union(matching_tags, &other_tags), ext)
        } else {
            FlatType::TagUnion(union(matching_tags, &other_tags), ext)
        };

        merge(subs, ctx, Structure(flat_type))
    } else {
        mismatch!()
    }
}

#[inline(always)]
fn unify_flat_type(
    subs: &mut Subs,
    pool: &mut Pool,
    ctx: &Context,
    left: &FlatType,
    right: &FlatType,
) -> Outcome {
    use crate::subs::FlatType::*;

    match (left, right) {
        (EmptyRecord, EmptyRecord) => merge(subs, ctx, Structure(left.clone())),

        (Record(fields, ext), EmptyRecord) if fields.is_empty() => {
            unify_pool(subs, pool, *ext, ctx.second)
        }

        (EmptyRecord, Record(fields, ext)) if fields.is_empty() => {
            unify_pool(subs, pool, ctx.first, *ext)
        }

        (Record(fields1, ext1), Record(fields2, ext2)) => {
            let rec1 = gather_fields(subs, fields1.clone(), *ext1);
            let rec2 = gather_fields(subs, fields2.clone(), *ext2);

            unify_record(subs, pool, ctx, rec1, rec2)
        }

        (EmptyTagUnion, EmptyTagUnion) => merge(subs, ctx, Structure(left.clone())),

        (TagUnion(tags, ext), EmptyTagUnion) if tags.is_empty() => {
            unify_pool(subs, pool, *ext, ctx.second)
        }

        (EmptyTagUnion, TagUnion(tags, ext)) if tags.is_empty() => {
            unify_pool(subs, pool, ctx.first, *ext)
        }

        (TagUnion(tags1, ext1), TagUnion(tags2, ext2)) => {
            let union1 = gather_tags(subs, tags1.clone(), *ext1);
            let union2 = gather_tags(subs, tags2.clone(), *ext2);

            unify_tag_union(subs, pool, ctx, union1, union2, (None, None))
        }

        (TagUnion(tags1, ext1), RecursiveTagUnion(recursion_var, tags2, ext2)) => {
            let union1 = gather_tags(subs, tags1.clone(), *ext1);
            let union2 = gather_tags(subs, tags2.clone(), *ext2);

            unify_tag_union(
                subs,
                pool,
                ctx,
                union1,
                union2,
                (None, Some(*recursion_var)),
            )
        }

        (RecursiveTagUnion(rec1, tags1, ext1), RecursiveTagUnion(rec2, tags2, ext2)) => {
            let union1 = gather_tags(subs, tags1.clone(), *ext1);
            let union2 = gather_tags(subs, tags2.clone(), *ext2);

            unify_tag_union(subs, pool, ctx, union1, union2, (Some(*rec1), Some(*rec2)))
        }

        (Boolean(Bool(free1, rest1)), Boolean(Bool(free2, rest2))) => {
            // unify the free variables
            let (new_free, mut free_var_problems) = unify_free_atoms(subs, pool, *free1, *free2);

            let combined_rest: SendSet<Atom> = rest1
                .clone()
                .into_iter()
                .chain(rest2.clone().into_iter())
                .collect::<SendSet<Atom>>();

            let mut combined = if let Err(false) = chase_atom(subs, new_free) {
                // if the container is shared, all elements must be shared too
                for atom in combined_rest {
                    let (_, atom_problems) = unify_free_atoms(subs, pool, atom, Atom::Zero);
                    free_var_problems.extend(atom_problems);
                }
                Bool(Atom::Zero, SendSet::default())
            } else {
                Bool(new_free, combined_rest)
            };

            combined.apply_subs(subs);

            // force first and second to equal this new variable
            let content = Content::Structure(FlatType::Boolean(combined));
            merge(subs, ctx, content);

            free_var_problems
        }

        (Apply(l_symbol, l_args), Apply(r_symbol, r_args)) if l_symbol == r_symbol => {
            let problems = unify_zip(subs, pool, l_args.iter(), r_args.iter());

            if problems.is_empty() {
                merge(subs, ctx, Structure(Apply(*r_symbol, (*r_args).clone())))
            } else {
                problems
            }
        }
        (Func(l_args, l_ret), Func(r_args, r_ret)) if l_args.len() == r_args.len() => {
            let arg_problems = unify_zip(subs, pool, l_args.iter(), r_args.iter());
            let ret_problems = unify_pool(subs, pool, *l_ret, *r_ret);

            if arg_problems.is_empty() && ret_problems.is_empty() {
                merge(subs, ctx, Structure(Func((*r_args).clone(), *r_ret)))
            } else {
                let mut problems = ret_problems;

                problems.extend(arg_problems);

                problems
            }
        }
        (_other1, _other2) => {
            // Can't unify other1 and other2
            // dbg!(&_other1, &_other2);
            mismatch!()
        }
    }
}

fn chase_atom(subs: &mut Subs, atom: Atom) -> Result<Variable, bool> {
    match atom {
        Atom::Zero => Err(false),
        Atom::One => Err(true),
        Atom::Variable(var) => match subs.get(var).content {
            Content::Structure(FlatType::Boolean(Bool(first, rest))) => {
                debug_assert!(rest.is_empty());
                chase_atom(subs, first)
            }
            _ => Ok(var),
        },
    }
}

fn unify_free_atoms(subs: &mut Subs, pool: &mut Pool, b1: Atom, b2: Atom) -> (Atom, Vec<Mismatch>) {
    match (b1, b2) {
        (Atom::Variable(v1), Atom::Variable(v2)) => {
            (Atom::Variable(v1), unify_pool(subs, pool, v1, v2))
        }
        (Atom::Variable(var), other) | (other, Atom::Variable(var)) => {
            subs.set_content(
                var,
                Content::Structure(FlatType::Boolean(Bool(other, SendSet::default()))),
            );

            (other, vec![])
        }
        (Atom::Zero, Atom::Zero) => (Atom::Zero, vec![]),
        (Atom::One, Atom::One) => (Atom::One, vec![]),
        _ => unreachable!(
            "invalid boolean unification. Because we never infer One, this should never happen!"
        ),
    }
}

fn unify_zip<'a, I>(subs: &mut Subs, pool: &mut Pool, left_iter: I, right_iter: I) -> Outcome
where
    I: Iterator<Item = &'a Variable>,
{
    let mut problems = Vec::new();

    let it = left_iter.zip(right_iter);

    for (&l_var, &r_var) in it {
        problems.extend(unify_pool(subs, pool, l_var, r_var));
    }

    problems
}

#[inline(always)]
fn unify_rigid(subs: &mut Subs, ctx: &Context, name: &Lowercase, other: &Content) -> Outcome {
    match other {
        FlexVar(_) => {
            // If the other is flex, rigid wins!
            merge(subs, ctx, RigidVar(name.clone()))
        }
        RigidVar(_) | Structure(_) | Alias(_, _, _) => {
            // Type mismatch! Rigid can only unify with flex, even if the
            // rigid names are the same.
            mismatch!()
        }
        Error => {
            // Error propagates.
            merge(subs, ctx, Error)
        }
    }
}

#[inline(always)]
fn unify_flex(
    subs: &mut Subs,
    pool: &mut Pool,
    ctx: &Context,
    opt_name: &Option<Lowercase>,
    other: &Content,
) -> Outcome {
    match other {
        FlexVar(None) => {
            // If both are flex, and only left has a name, keep the name around.
            merge(subs, ctx, FlexVar(opt_name.clone()))
        }

        Structure(FlatType::Boolean(Bool(Atom::Variable(var), _rest))) => {
            unify_pool(subs, pool, ctx.first, *var)
        }

        FlexVar(Some(_)) | RigidVar(_) | Structure(_) | Alias(_, _, _) => {
            // TODO special-case boolean here
            // In all other cases, if left is flex, defer to right.
            // (This includes using right's name if both are flex and named.)
            merge(subs, ctx, other.clone())
        }

        Error => merge(subs, ctx, Error),
    }
}

pub fn gather_fields(
    subs: &mut Subs,
    fields: MutMap<Lowercase, Variable>,
    var: Variable,
) -> RecordStructure {
    use crate::subs::FlatType::*;

    match subs.get(var).content {
        Structure(Record(sub_fields, sub_ext)) => {
            gather_fields(subs, union(fields, &sub_fields), sub_ext)
        }

        Alias(_, _, var) => {
            // TODO according to elm/compiler: "TODO may be dropping useful alias info here"
            gather_fields(subs, fields, var)
        }

        _ => RecordStructure { fields, ext: var },
    }
}

fn gather_tags(
    subs: &mut Subs,
    tags: MutMap<TagName, Vec<Variable>>,
    var: Variable,
) -> TagUnionStructure {
    use crate::subs::FlatType::*;

    match subs.get(var).content {
        Structure(TagUnion(sub_tags, sub_ext)) => {
            gather_tags(subs, union(tags, &sub_tags), sub_ext)
        }

        Alias(_, _, var) => {
            // TODO according to elm/compiler: "TODO may be dropping useful alias info here"
            gather_tags(subs, tags, var)
        }

        _ => TagUnionStructure { tags, ext: var },
    }
}

fn merge(subs: &mut Subs, ctx: &Context, content: Content) -> Outcome {
    let rank = ctx.first_desc.rank.min(ctx.second_desc.rank);
    let desc = Descriptor {
        content,
        rank,
        mark: Mark::NONE,
        copy: OptVariable::NONE,
    };

    subs.union(ctx.first, ctx.second, desc);

    Vec::new()
}

fn register(subs: &mut Subs, desc: Descriptor, pool: &mut Pool) -> Variable {
    let var = subs.fresh(desc);

    pool.push(var);

    var
}

fn fresh(subs: &mut Subs, pool: &mut Pool, ctx: &Context, content: Content) -> Variable {
    register(
        subs,
        Descriptor {
            content,
            rank: ctx.first_desc.rank.min(ctx.second_desc.rank),
            mark: Mark::NONE,
            copy: OptVariable::NONE,
        },
        pool,
    )
}
