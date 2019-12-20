use crate::can::ident::{Lowercase, ModuleName, Uppercase};
use crate::collections::ImMap;
use crate::subs::Content::{self, *};
use crate::subs::{Descriptor, FlatType, Mark, Subs, Variable};
use crate::types::Problem;
use std::cmp::Ordering;

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

pub type Problems = Vec<Problem>;
pub type Pool = Vec<Variable>;

#[inline(always)]
pub fn unify(subs: &mut Subs, problems: &mut Problems, var1: Variable, var2: Variable) -> Pool {
    let mut pool = Vec::new();

    unify_pool(subs, &mut pool, problems, var1, var2);

    pool
}

#[inline(always)]
pub fn unify_pool(
    subs: &mut Subs,
    pool: &mut Pool,
    problems: &mut Problems,
    var1: Variable,
    var2: Variable,
) {
    if !subs.equivalent(var1, var2) {
        let ctx = Context {
            first: var1,
            first_desc: subs.get(var1),
            second: var2,
            second_desc: subs.get(var2),
        };

        unify_context(subs, pool, problems, ctx);
    }
}

fn unify_context(subs: &mut Subs, pool: &mut Pool, problems: &mut Problems, ctx: Context) {
    match &ctx.first_desc.content {
        FlexVar(opt_name) => unify_flex(subs, problems, &ctx, opt_name, &ctx.second_desc.content),
        RigidVar(name) => unify_rigid(subs, problems, &ctx, name, &ctx.second_desc.content),
        Structure(flat_type) => unify_structure(
            subs,
            pool,
            problems,
            &ctx,
            flat_type,
            &ctx.second_desc.content,
        ),
        Alias(home, name, args, real_var) => {
            unify_alias(subs, pool, problems, &ctx, home, name, args, *real_var)
        }
        Error(problem) => {
            // Error propagates. Whatever we're comparing it to doesn't matter!
            merge(subs, &ctx, Error(problem.clone()));
            problems.push(problem.clone());
        }
    }
}

// TODO trim down this arg list
#[allow(clippy::too_many_arguments)]
#[inline(always)]
fn unify_alias(
    subs: &mut Subs,
    pool: &mut Pool,
    problems: &mut Problems,
    ctx: &Context,
    home: &ModuleName,
    name: &Uppercase,
    args: &[(Lowercase, Variable)],
    real_var: Variable,
) {
    let other_content = &ctx.second_desc.content;

    match other_content {
        FlexVar(_) => {
            // Alias wins
            merge(
                subs,
                &ctx,
                Alias(home.clone(), name.clone(), args.to_owned(), real_var),
            );
        }
        RigidVar(_) => unify_pool(subs, pool, problems, real_var, ctx.second),
        Alias(other_home, other_name, other_args, other_real_var) => {
            if name == other_name && home == other_home {
                match args.len().cmp(&other_args.len()) {
                    Ordering::Greater => {
                        let problem = Problem::ExtraArguments;

                        merge(subs, &ctx, Error(problem.clone()));
                        problems.push(problem);
                    }
                    Ordering::Less => {
                        let problem = Problem::MissingArguments;

                        merge(subs, &ctx, Error(problem.clone()));
                        problems.push(problem);
                    }
                    Ordering::Equal => {
                        for ((_, l_var), (_, r_var)) in args.iter().zip(other_args.iter()) {
                            unify_pool(subs, pool, problems, *l_var, *r_var);
                        }
                        merge(subs, &ctx, other_content.clone());
                    }
                }
            } else {
                unify_pool(subs, pool, problems, real_var, *other_real_var)
            }
        }
        Structure(_) => unify_pool(subs, pool, problems, real_var, ctx.second),
        Error(problem) => {
            merge(subs, ctx, Error(problem.clone()));
            problems.push(problem.clone());
        }
    }
}

#[inline(always)]
fn unify_structure(
    subs: &mut Subs,
    pool: &mut Pool,
    problems: &mut Problems,
    ctx: &Context,
    flat_type: &FlatType,
    other: &Content,
) {
    match other {
        FlexVar(_) => {
            // If the other is flex, Structure wins!
            merge(subs, ctx, Structure(flat_type.clone()));
        }
        RigidVar(_) => {
            let problem = Problem::GenericMismatch;
            // Type mismatch! Rigid can only unify with flex.
            merge(subs, ctx, Error(problem.clone()));
            problems.push(problem);
        }
        Structure(ref other_flat_type) => {
            // Unify the two flat types
            unify_flat_type(subs, pool, problems, ctx, flat_type, other_flat_type)
        }
        Alias(_, _, _, real_var) => unify_pool(subs, pool, problems, ctx.first, *real_var),
        Error(problem) => {
            // Error propagates.
            merge(subs, ctx, Error(problem.clone()));
            problems.push(problem.clone());
        }
    }
}

fn unify_record(
    subs: &mut Subs,
    pool: &mut Pool,
    problems: &mut Problems,
    ctx: &Context,
    rec1: RecordStructure,
    rec2: RecordStructure,
) {
    let fields1 = rec1.fields;
    let fields2 = rec2.fields;
    let shared_fields = fields1
        .clone()
        .intersection_with(fields2.clone(), |one, two| (one, two));
    let unique_fields1 = fields1.clone().difference(fields2.clone());
    let unique_fields2 = fields2.difference(fields1);

    if unique_fields1.is_empty() {
        if unique_fields2.is_empty() {
            unify_pool(subs, pool, problems, rec1.ext, rec2.ext);
            unify_shared_fields(
                subs,
                pool,
                problems,
                ctx,
                shared_fields,
                ImMap::default(),
                rec1.ext,
            )
        } else {
            let flat_type = FlatType::Record(unique_fields2, rec2.ext);
            let sub_record = fresh(subs, pool, ctx, Structure(flat_type));

            unify_pool(subs, pool, problems, rec1.ext, sub_record);

            unify_shared_fields(
                subs,
                pool,
                problems,
                ctx,
                shared_fields,
                ImMap::default(),
                sub_record,
            );
        }
    } else if unique_fields2.is_empty() {
        let flat_type = FlatType::Record(unique_fields1, rec1.ext);
        let sub_record = fresh(subs, pool, ctx, Structure(flat_type));

        unify_pool(subs, pool, problems, sub_record, rec2.ext);

        unify_shared_fields(
            subs,
            pool,
            problems,
            ctx,
            shared_fields,
            ImMap::default(),
            sub_record,
        );
    } else {
        let other_fields = unique_fields1.clone().union(unique_fields2.clone());
        let ext = fresh(subs, pool, ctx, Content::FlexVar(None));
        let flat_type1 = FlatType::Record(unique_fields1, rec1.ext);
        let sub1 = fresh(subs, pool, ctx, Structure(flat_type1));
        let flat_type2 = FlatType::Record(unique_fields2, rec2.ext);
        let sub2 = fresh(subs, pool, ctx, Structure(flat_type2));

        unify_pool(subs, pool, problems, rec1.ext, sub2);
        unify_pool(subs, pool, problems, sub1, rec2.ext);

        unify_shared_fields(subs, pool, problems, ctx, shared_fields, other_fields, ext);
    }
}

fn unify_shared_fields(
    subs: &mut Subs,
    pool: &mut Pool,
    problems: &mut Problems,
    ctx: &Context,
    shared_fields: ImMap<Lowercase, (Variable, Variable)>,
    other_fields: ImMap<Lowercase, Variable>,
    ext: Variable,
) {
    let mut matching_fields = ImMap::default();
    let num_shared_fields = shared_fields.len();

    for (name, (actual, expected)) in shared_fields {
        let prev_problem_count = problems.len();

        // TODO another way to do this might be to pass around a problems vec
        // and check to see if its length increased after doing this unification.
        unify_pool(subs, pool, problems, actual, expected);

        if problems.len() == prev_problem_count {
            matching_fields.insert(name, actual);
        }
    }

    if num_shared_fields == matching_fields.len() {
        let flat_type = FlatType::Record(matching_fields.union(other_fields), ext);

        merge(subs, ctx, Structure(flat_type));
    } else {
        let problem = Problem::GenericMismatch;

        // Type mismatch! Rigid can only unify with flex.
        merge(subs, ctx, Error(problem.clone()));
        problems.push(problem);
    }
}

#[inline(always)]
fn unify_flat_type(
    subs: &mut Subs,
    pool: &mut Pool,
    problems: &mut Problems,
    ctx: &Context,
    left: &FlatType,
    right: &FlatType,
) {
    use crate::subs::FlatType::*;

    match (left, right) {
        (EmptyRecord, EmptyRecord) => {
            merge(subs, ctx, Structure(left.clone()));
        }

        (Record(fields, ext), EmptyRecord) if fields.is_empty() => {
            unify_pool(subs, pool, problems, *ext, ctx.second)
        }

        (EmptyRecord, Record(fields, ext)) if fields.is_empty() => {
            unify_pool(subs, pool, problems, ctx.first, *ext)
        }

        (Record(fields1, ext1), Record(fields2, ext2)) => {
            let rec1 = gather_fields(subs, problems, fields1.clone(), *ext1);
            let rec2 = gather_fields(subs, problems, fields2.clone(), *ext2);

            unify_record(subs, pool, problems, ctx, rec1, rec2)
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
            unify_zip(subs, pool, problems, l_args.iter(), r_args.iter());

            merge(
                subs,
                ctx,
                Structure(Apply {
                    module_name: (*r_module_name).clone(),
                    name: (*r_type_name).clone(),
                    args: (*r_args).clone(),
                }),
            );
        }
        (Func(l_args, l_ret), Func(r_args, r_ret)) => match l_args.len().cmp(&r_args.len()) {
            Ordering::Greater => merge(subs, ctx, Error(Problem::ExtraArguments)),
            Ordering::Less => merge(subs, ctx, Error(Problem::MissingArguments)),
            Ordering::Equal => {
                unify_zip(subs, pool, problems, l_args.iter(), r_args.iter());
                unify_pool(subs, pool, problems, *l_ret, *r_ret);
                merge(subs, ctx, Structure(Func((*r_args).clone(), *r_ret)));
            }
        },
        _ => {
            let problem = Problem::GenericMismatch;

            merge(subs, ctx, Error(problem.clone()));
            problems.push(problem);
        }
    }
}

fn unify_zip<'a, I>(
    subs: &mut Subs,
    pool: &mut Pool,
    problems: &mut Problems,
    left_iter: I,
    right_iter: I,
) where
    I: Iterator<Item = &'a Variable>,
{
    for (&l_var, &r_var) in left_iter.zip(right_iter) {
        unify_pool(subs, pool, problems, l_var, r_var);
    }
}

#[inline(always)]
fn unify_rigid(
    subs: &mut Subs,
    problems: &mut Problems,
    ctx: &Context,
    name: &Lowercase,
    other: &Content,
) {
    match other {
        FlexVar(_) => {
            // If the other is flex, rigid wins!
            merge(subs, ctx, RigidVar(name.clone()));
        }
        RigidVar(_) | Structure(_) => {
            // Type mismatch! Rigid can only unify with flex, even if the
            // rigid names are the same.
            merge(subs, ctx, Error(Problem::GenericMismatch));
        }
        Alias(_, _, _, _) => {
            panic!("TODO unify_rigid Alias");
        }
        Error(problem) => {
            // Error propagates.
            merge(subs, ctx, Error(problem.clone()));
            problems.push(problem.clone());
        }
    }
}

#[inline(always)]
fn unify_flex(
    subs: &mut Subs,
    problems: &mut Problems,
    ctx: &Context,
    opt_name: &Option<Lowercase>,
    other: &Content,
) {
    match other {
        FlexVar(None) => {
            // If both are flex, and only left has a name, keep the name around.
            merge(subs, ctx, FlexVar(opt_name.clone()));
        }
        FlexVar(Some(_)) | RigidVar(_) | Structure(_) | Alias(_, _, _, _) => {
            // In all other cases, if left is flex, defer to right.
            // (This includes using right's name if both are flex and named.)
            merge(subs, ctx, other.clone());
        }
        Error(problem) => {
            merge(subs, ctx, Error(problem.clone()));
            problems.push(problem.clone());
        }
    }
}

fn gather_fields(
    subs: &mut Subs,
    problems: &mut Problems,
    fields: ImMap<Lowercase, Variable>,
    var: Variable,
) -> RecordStructure {
    use crate::subs::FlatType::*;

    match subs.get(var).content {
        Structure(Record(sub_fields, sub_ext)) => {
            gather_fields(subs, problems, fields.union(sub_fields), sub_ext)
        }

        Alias(_, _, _, var) => {
            // TODO according to elm/compiler: "TODO may be dropping useful alias info here"
            gather_fields(subs, problems, fields, var)
        }

        _ => RecordStructure { fields, ext: var },
    }
}

fn merge(subs: &mut Subs, ctx: &Context, content: Content) {
    let rank = ctx.first_desc.rank.min(ctx.second_desc.rank);
    let desc = Descriptor {
        content,
        rank,
        mark: Mark::none(),
        copy: None,
    };

    subs.union(ctx.first, ctx.second, desc);
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
