use crate::can::ident::{Lowercase, ModuleName, Uppercase};
use crate::collections::ImMap;
use crate::subs::Content::{self, *};
use crate::subs::{Descriptor, FlatType, Mark, Subs, Variable};
use crate::types::{Mismatch, Problem};
use std::cmp::Ordering;

type Pool = Vec<Variable>;

struct Context {
    first: Variable,
    first_desc: Descriptor,
    second: Variable,
    second_desc: Descriptor,
}

struct RecordStructure {
    fields: ImMap<Lowercase, Variable>,
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

            subs.union(var1, var2, Descriptor::error());

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
    match &ctx.first_desc.content {
        FlexVar(opt_name) => unify_flex(subs, &ctx, opt_name, &ctx.second_desc.content),
        RigidVar(name) => unify_rigid(subs, &ctx, name, &ctx.second_desc.content),
        Structure(flat_type) => {
            unify_structure(subs, pool, &ctx, flat_type, &ctx.second_desc.content)
        }
        Alias(home, name, args, real_var) => {
            unify_alias(subs, pool, &ctx, home, name, args, *real_var)
        }
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
    home: &ModuleName,
    name: &Uppercase,
    args: &[(Lowercase, Variable)],
    real_var: Variable,
) -> Outcome {
    let other_content = &ctx.second_desc.content;

    match other_content {
        FlexVar(_) => {
            // Alias wins
            merge(
                subs,
                &ctx,
                Alias(home.clone(), name.clone(), args.to_owned(), real_var),
            )
        }
        RigidVar(_) => unify_pool(subs, pool, real_var, ctx.second),
        Alias(other_home, other_name, other_args, other_real_var) => {
            if name == other_name && home == other_home {
                match args.len().cmp(&other_args.len()) {
                    Ordering::Greater => vec![Mismatch::ExtraArguments {
                        expected: other_args.len(),
                        actual: args.len(),
                    }],
                    Ordering::Less => vec![Mismatch::MissingArguments {
                        expected: other_args.len(),
                        actual: args.len(),
                    }],
                    Ordering::Equal => {
                        for ((_, l_var), (_, r_var)) in args.iter().zip(other_args.iter()) {
                            unify_pool(subs, pool, *l_var, *r_var);
                        }

                        merge(subs, &ctx, other_content.clone())
                    }
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
            // If the other is flex, Structure wins!
            merge(subs, ctx, Structure(flat_type.clone()))
        }
        RigidVar(_) => {
            // Type mismatch! Rigid can only unify with flex.
            mismatch()
        }
        Structure(ref other_flat_type) => {
            // Unify the two flat types
            unify_flat_type(subs, pool, ctx, flat_type, other_flat_type)
        }
        Alias(_, _, _, real_var) => unify_pool(subs, pool, ctx.first, *real_var),
        Error => merge(subs, ctx, Error),
    }
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
    let shared_fields = fields1
        .clone()
        .intersection_with(fields2.clone(), |one, two| (one, two));
    let unique_fields1 = fields1.clone().difference(fields2.clone());
    let unique_fields2 = fields2.difference(fields1);

    if unique_fields1.is_empty() {
        if unique_fields2.is_empty() {
            let ext_problems = unify_pool(subs, pool, rec1.ext, rec2.ext);
            let mut field_problems =
                unify_shared_fields(subs, pool, ctx, shared_fields, ImMap::default(), rec1.ext);

            field_problems.extend(ext_problems);

            field_problems
        } else {
            let flat_type = FlatType::Record(unique_fields2, rec2.ext);
            let sub_record = fresh(subs, pool, ctx, Structure(flat_type));
            let ext_problems = unify_pool(subs, pool, rec1.ext, sub_record);
            let mut field_problems =
                unify_shared_fields(subs, pool, ctx, shared_fields, ImMap::default(), sub_record);

            field_problems.extend(ext_problems);

            field_problems
        }
    } else if unique_fields2.is_empty() {
        let flat_type = FlatType::Record(unique_fields1, rec1.ext);
        let sub_record = fresh(subs, pool, ctx, Structure(flat_type));
        let ext_problems = unify_pool(subs, pool, sub_record, rec2.ext);
        let mut field_problems =
            unify_shared_fields(subs, pool, ctx, shared_fields, ImMap::default(), sub_record);

        field_problems.extend(ext_problems);

        field_problems
    } else {
        let other_fields = unique_fields1.clone().union(unique_fields2.clone());
        let ext = fresh(subs, pool, ctx, Content::FlexVar(None));
        let flat_type1 = FlatType::Record(unique_fields1, rec1.ext);
        let sub1 = fresh(subs, pool, ctx, Structure(flat_type1));
        let flat_type2 = FlatType::Record(unique_fields2, rec2.ext);
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
    shared_fields: ImMap<Lowercase, (Variable, Variable)>,
    other_fields: ImMap<Lowercase, Variable>,
    ext: Variable,
) -> Outcome {
    let mut matching_fields = ImMap::default();
    let num_shared_fields = shared_fields.len();

    for (name, (actual, expected)) in shared_fields {
        let problems = unify_pool(subs, pool, actual, expected);

        if problems.is_empty() {
            matching_fields.insert(name, actual);
        }
    }

    if num_shared_fields == matching_fields.len() {
        let flat_type = FlatType::Record(matching_fields.union(other_fields), ext);

        merge(subs, ctx, Structure(flat_type))
    } else {
        mismatch()
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
        (
            Apply {
                module_name: l_module_name,
                name: l_type_name,
                args: l_args,
            },
            Apply {
                module_name: r_module_name,
                name: r_type_name,
                args: r_args,
            },
        ) if l_module_name == r_module_name && l_type_name == r_type_name => {
            let problems = unify_zip(subs, pool, l_args.iter(), r_args.iter());

            if problems.is_empty() {
                merge(
                    subs,
                    ctx,
                    Structure(Apply {
                        module_name: (*r_module_name).clone(),
                        name: (*r_type_name).clone(),
                        args: (*r_args).clone(),
                    }),
                )
            } else {
                problems
            }
        }
        (Func(l_args, l_ret), Func(r_args, r_ret)) => match l_args.len().cmp(&r_args.len()) {
            Ordering::Greater => vec![Mismatch::ExtraArguments {
                expected: l_args.len(),
                actual: r_args.len(),
            }],
            Ordering::Less => vec![Mismatch::MissingArguments {
                expected: l_args.len(),
                actual: r_args.len(),
            }],
            Ordering::Equal => {
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
        },
        _ => mismatch(),
    }
}

fn unify_zip<'a, I>(subs: &mut Subs, pool: &mut Pool, left_iter: I, right_iter: I) -> Outcome
where
    I: Iterator<Item = &'a Variable>,
{
    let mut problems = Vec::new();

    for (&l_var, &r_var) in left_iter.zip(right_iter) {
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
        RigidVar(_) | Structure(_) => {
            // Type mismatch! Rigid can only unify with flex, even if the
            // rigid names are the same.
            mismatch()
        }
        Alias(_, _, _, _) => {
            panic!("TODO unify_rigid Alias");
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
    ctx: &Context,
    opt_name: &Option<Lowercase>,
    other: &Content,
) -> Outcome {
    match other {
        FlexVar(None) => {
            // If both are flex, and only left has a name, keep the name around.
            merge(subs, ctx, FlexVar(opt_name.clone()))
        }
        FlexVar(Some(_)) | RigidVar(_) | Structure(_) | Alias(_, _, _, _) => {
            // In all other cases, if left is flex, defer to right.
            // (This includes using right's name if both are flex and named.)
            merge(subs, ctx, other.clone())
        }
        Error => merge(subs, ctx, Error),
    }
}

fn gather_fields(
    subs: &mut Subs,
    fields: ImMap<Lowercase, Variable>,
    var: Variable,
) -> RecordStructure {
    use crate::subs::FlatType::*;

    match subs.get(var).content {
        Structure(Record(sub_fields, sub_ext)) => {
            gather_fields(subs, fields.union(sub_fields), sub_ext)
        }

        Alias(_, _, _, var) => {
            // TODO according to elm/compiler: "TODO may be dropping useful alias info here"
            gather_fields(subs, fields, var)
        }

        _ => RecordStructure { fields, ext: var },
    }
}

fn merge(subs: &mut Subs, ctx: &Context, content: Content) -> Outcome {
    let rank = ctx.first_desc.rank.min(ctx.second_desc.rank);
    let desc = Descriptor {
        content,
        rank,
        mark: Mark::none(),
        copy: None,
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
            mark: Mark::none(),
            copy: None,
        },
        pool,
    )
}

#[inline(always)]
fn mismatch() -> Outcome {
    vec![Mismatch::TypeMismatch]
}
