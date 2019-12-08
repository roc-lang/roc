use crate::can::ident::{Lowercase, ModuleName, Uppercase};
use crate::collections::ImMap;
use crate::subs::Content::{self, *};
use crate::subs::{Descriptor, FlatType, Mark, Subs, Variable};
use crate::types::Problem;

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

type UnifyResult = Result<(), Problem>;

type Problems = Vec<Problem>;

#[inline(always)]
pub fn unify(subs: &mut Subs, var1: Variable, var2: Variable) -> UnifyResult {
    if subs.equivalent(var1, var2) {
        Ok(())
    } else {
        let ctx = Context {
            first: var1,
            first_desc: subs.get(var1),
            second: var2,
            second_desc: subs.get(var2),
        };

        unify_context(subs, ctx)
    }
}

fn unify_context(subs: &mut Subs, ctx: Context) -> UnifyResult {
    match &ctx.first_desc.content {
        FlexVar(opt_name) => unify_flex(subs, &ctx, opt_name, &ctx.second_desc.content),
        RigidVar(name) => unify_rigid(subs, &ctx, name, &ctx.second_desc.content),
        Structure(flat_type) => unify_structure(subs, &ctx, flat_type, &ctx.second_desc.content),
        Alias(home, name, args, real_var) => unify_alias(subs, &ctx, home, name, args, real_var),
        Error(problem) => {
            // Error propagates. Whatever we're comparing it to doesn't matter!
            merge(subs, &ctx, Error(problem.clone()));

            Err(problem.clone())
        }
    }
}

#[inline(always)]
fn unify_alias(
    subs: &mut Subs,
    ctx: &Context,
    home: &ModuleName,
    name: &Uppercase,
    args: &Vec<(Lowercase, Variable)>,
    real_var: &Variable,
) -> UnifyResult {
    let other_content = &ctx.second_desc.content;

    match other_content {
        FlexVar(_) => {
            // Alias wins
            merge(
                subs,
                &ctx,
                Alias(home.clone(), name.clone(), args.clone(), *real_var),
            );

            Ok(())
        }
        RigidVar(_) => unify(subs, *real_var, ctx.second),
        Alias(other_home, other_name, other_args, other_real_var) => {
            if name == other_name && home == other_home {
                if args.len() == other_args.len() {
                    let mut answer = Ok(());

                    for ((_, l_var), (_, r_var)) in args.iter().zip(other_args.iter()) {
                        let result = unify(subs, *l_var, *r_var);

                        answer = answer.and_then(|()| result);
                    }

                    merge(subs, &ctx, other_content.clone());

                    answer
                } else if args.len() > other_args.len() {
                    let problem = Problem::ExtraArguments;

                    merge(subs, &ctx, Error(problem.clone()));

                    Err(problem)
                } else {
                    let problem = Problem::MissingArguments;

                    merge(subs, &ctx, Error(problem.clone()));

                    Err(problem)
                }
            } else {
                unify(subs, *real_var, *other_real_var)
            }
        }
        Structure(_) => unify(subs, *real_var, ctx.second),
        Error(problem) => {
            merge(subs, ctx, Error(problem.clone()));

            Err(problem.clone())
        }
    }
}

#[inline(always)]
fn unify_structure(
    subs: &mut Subs,
    ctx: &Context,
    flat_type: &FlatType,
    other: &Content,
) -> UnifyResult {
    match other {
        FlexVar(_) => {
            // If the other is flex, Structure wins!
            merge(subs, ctx, Structure(flat_type.clone()));

            Ok(())
        }
        RigidVar(_) => {
            let problem = Problem::GenericMismatch;
            // Type mismatch! Rigid can only unify with flex.
            merge(subs, ctx, Error(problem.clone()));

            Err(problem)
        }
        Structure(ref other_flat_type) => {
            // Unify the two flat types
            unify_flat_type(subs, ctx, flat_type, other_flat_type)
        }
        Alias(_, _, _, real_var) => unify(subs, ctx.first, *real_var),
        Error(problem) => {
            // Error propagates.
            merge(subs, ctx, Error(problem.clone()));

            Err(problem.clone())
        }
    }
}

fn unify_record(
    subs: &mut Subs,
    ctx: &Context,
    rec1: RecordStructure,
    rec2: RecordStructure,
) -> UnifyResult {
    let fields1 = rec1.fields;
    let fields2 = rec2.fields;
    let shared_fields = fields1
        .clone()
        .intersection_with(fields2.clone(), |one, two| (one, two));
    let unique_fields1 = fields1.clone().difference(fields2.clone());
    let unique_fields2 = fields2.difference(fields1);

    if unique_fields1.is_empty() {
        if unique_fields2.is_empty() {
            unify(subs, rec1.ext, rec2.ext);
            unify_shared_fields(subs, ctx, shared_fields, ImMap::default(), rec1.ext)
        } else {
            // subRecord <- fresh context (Structure (Record1 uniqueFields2 ext2))
            // subUnify ext1 subRecord
            // unifySharedFields context sharedFields Map.empty subRecord
            panic!("TODO 1");
        }
    } else if unique_fields2.is_empty() {
        // subRecord <- fresh context (Structure (Record1 uniqueFields1 ext1))
        // subUnify subRecord ext2
        // unifySharedFields context sharedFields Map.empty subRecord
        panic!("TODO 2");
    } else {
        // let otherFields = Map.union uniqueFields1 uniqueFields2
        // ext <- fresh context Type.unnamedFlexVar
        // sub1 <- fresh context (Structure (Record1 uniqueFields1 ext))
        // sub2 <- fresh context (Structure (Record1 uniqueFields2 ext))
        // subUnify ext1 sub2
        // subUnify sub1 ext2
        // unifySharedFields context sharedFields otherFields ext
        //
        panic!("TODO 3");
    }
}

fn unify_shared_fields(
    subs: &mut Subs,
    ctx: &Context,
    shared_fields: ImMap<Lowercase, (Variable, Variable)>,
    other_fields: ImMap<Lowercase, Variable>,
    ext: Variable,
) -> UnifyResult {
    let mut matching_fields = ImMap::default();
    let num_shared_fields = shared_fields.len();

    for (name, (actual, expected)) in shared_fields {
        // TODO another way to do this might be to pass around a problems vec
        // and check to see if its length increased after doing this unification.
        if unify(subs, actual, expected).is_ok() {
            matching_fields.insert(name, actual);
        }
    }

    if num_shared_fields == matching_fields.len() {
        let flat_type = FlatType::Record(matching_fields.union(other_fields), ext);

        merge(subs, ctx, Structure(flat_type));

        Ok(())
    } else {
        let problem = Problem::GenericMismatch;

        // Type mismatch! Rigid can only unify with flex.
        merge(subs, ctx, Error(problem.clone()));

        Err(problem)
    }
}

#[inline(always)]
fn unify_flat_type(
    subs: &mut Subs,
    ctx: &Context,
    left: &FlatType,
    right: &FlatType,
) -> UnifyResult {
    use crate::subs::FlatType::*;

    match (left, right) {
        (EmptyRecord, EmptyRecord) => {
            merge(subs, ctx, Structure(left.clone()));

            Ok(())
        }

        (Record(fields, ext), EmptyRecord) if fields.is_empty() => unify(subs, *ext, ctx.second),

        (EmptyRecord, Record(fields, ext)) if fields.is_empty() => unify(subs, ctx.first, *ext),

        (Record(fields1, ext1), Record(fields2, ext2)) => {
            let rec1 = gather_fields(subs, fields1.clone(), *ext1);
            let rec2 = gather_fields(subs, fields2.clone(), *ext2);

            unify_record(subs, ctx, rec1, rec2)
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
            unify_zip(subs, l_args.iter(), r_args.iter());

            merge(
                subs,
                ctx,
                Structure(Apply {
                    module_name: (*r_module_name).clone(),
                    name: (*r_type_name).clone(),
                    args: (*r_args).clone(),
                }),
            );

            Ok(())
        }
        (Func(l_args, l_ret), Func(r_args, r_ret)) => {
            if l_args.len() == r_args.len() {
                unify_zip(subs, l_args.iter(), r_args.iter());
                let answer = unify(subs, *l_ret, *r_ret);

                merge(subs, ctx, Structure(Func((*r_args).clone(), *r_ret)));

                answer
            } else if l_args.len() > r_args.len() {
                merge(subs, ctx, Error(Problem::ExtraArguments));

                Ok(())
            } else {
                merge(subs, ctx, Error(Problem::MissingArguments));

                Ok(())
            }
        }
        _ => {
            let problem = Problem::GenericMismatch;

            merge(subs, ctx, Error(problem.clone()));

            Err(problem)
        }
    }
}

fn unify_zip<'a, I>(subs: &mut Subs, left_iter: I, right_iter: I)
where
    I: Iterator<Item = &'a Variable>,
{
    for (&l_var, &r_var) in left_iter.zip(right_iter) {
        unify(subs, l_var, r_var);
    }
}

#[inline(always)]
fn unify_rigid(subs: &mut Subs, ctx: &Context, name: &str, other: &Content) -> UnifyResult {
    match other {
        FlexVar(_) => {
            // If the other is flex, rigid wins!
            merge(subs, ctx, RigidVar(name.into()));

            Ok(())
        }
        RigidVar(_) | Structure(_) => {
            // Type mismatch! Rigid can only unify with flex, even if the
            // rigid names are the same.
            merge(subs, ctx, Error(Problem::GenericMismatch));

            Ok(())
        }
        Alias(_, _, _, _) => {
            panic!("TODO unify_rigid Alias");

            Ok(())
        }
        Error(problem) => {
            // Error propagates.
            merge(subs, ctx, Error(problem.clone()));

            Err(problem.clone())
        }
    }
}

#[inline(always)]
fn unify_flex(
    subs: &mut Subs,
    ctx: &Context,
    opt_name: &Option<Box<str>>,
    other: &Content,
) -> UnifyResult {
    match other {
        FlexVar(None) => {
            // If both are flex, and only left has a name, keep the name around.
            merge(subs, ctx, FlexVar(opt_name.clone()));

            Ok(())
        }
        FlexVar(Some(_)) | RigidVar(_) | Structure(_) | Alias(_, _, _, _) => {
            // In all other cases, if left is flex, defer to right.
            // (This includes using right's name if both are flex and named.)
            merge(subs, ctx, other.clone());

            Ok(())
        }
        Error(problem) => {
            merge(subs, ctx, Error(problem.clone()));

            Err(problem.clone())
        }
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
