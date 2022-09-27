//! Fix fixpoints of recursive types.

use roc_error_macros::internal_error;
use roc_module::symbol::ModuleId;
use roc_types::subs::{Content, FlatType, GetSubsSlice, Subs, Variable};

struct Update {
    source_of_truth: Variable,
    update_var: Variable,
}

fn find_chain(subs: &Subs, left: Variable, right: Variable) -> impl Iterator<Item = Update> {
    let left = subs.get_root_key_without_compacting(left);
    let right = subs.get_root_key_without_compacting(right);

    let needle = (left, right);

    enum ClobberSide {
        Left,
        Right,
    }
    use ClobberSide::*;

    let (search_left, search_right, clobber_side) = match (
        subs.get_content_without_compacting(left),
        subs.get_content_without_compacting(right),
    ) {
        (Content::RecursionVar { .. }, Content::RecursionVar { .. }) => internal_error!(
            "two recursion variables at the same level can be unified without fixpoint-fixing"
        ),
        // Unwrap one of the recursion variables to their structure, so that we don't end up
        // immediately staring at the base case in `help`.
        (Content::RecursionVar { structure, .. }, _) => (*structure, right, Right),
        (_, Content::RecursionVar { structure, .. }) => (left, *structure, Left),
        _ => internal_error!(
            "fixpoint-fixing requires a recursion variable and a non-recursion variable"
        ),
    };

    let chain = help(subs, needle, search_left, search_right)
        .expect("chain must exist if fixpoints are being fixed!");
    // Suppose we started with
    //   (type1, <rec>)
    // where <rec> recurses to type2. At this point, the chain should look like
    //   (type1, <rec>), ..., (type1, type2)
    // We'll verify that <rec> appears on the side we'll be choosing to clobber. Then, we don't
    // want to explicitly update the recursion var, so we'll just update everything past the first
    // item of the chain.
    assert_eq!(chain.first().unwrap(), &needle);

    let updates_iter = chain
        .into_iter()
        // Skip the first element to avoid rewritting <rec> => type1 explicitly!
        .skip(0)
        // Set up the iterator so the right-hand side contains the variable we want to clobber with
        // the content of the left-hand side; that is, the left-hand side becomes the
        // source-of-truth.
        .map(move |(left, right)| {
            let (source_of_truth, update_var) = match clobber_side {
                Left => (right, left),
                Right => (left, right),
            };
            Update {
                source_of_truth,
                update_var,
            }
        });

    return updates_iter;

    fn help(
        subs: &Subs,
        needle: (Variable, Variable),
        left: Variable,
        right: Variable,
    ) -> Result<Vec<(Variable, Variable)>, ()> {
        let left = subs.get_root_key_without_compacting(left);
        let right = subs.get_root_key_without_compacting(right);

        if (left, right) == needle {
            return Ok(vec![needle]);
        }

        dbg!((left, right));

        use Content::*;
        use FlatType::*;
        match (
            subs.get_content_without_compacting(left),
            subs.get_content_without_compacting(right),
        ) {
            (FlexVar(..), FlexVar(..))
            | (RigidVar(..), RigidVar(..))
            | (RigidAbleVar(..), RigidAbleVar(..))
            | (FlexAbleVar(..), FlexAbleVar(..))
            | (Error, Error)
            | (RangedNumber(..), RangedNumber(..)) => Err(()),
            (RecursionVar { .. }, RecursionVar { .. }) => internal_error!("not expected"),
            (RecursionVar { structure, .. }, _) => {
                // By construction, the recursion variables will be adjusted to be equal after
                // the transformation, so we can immediately look at the inner variable. We only
                // need to adjust head constructors.
                // help(subs, needle, *structure, right)
                let mut chain = help(subs, needle, *structure, right)?;
                //chain.push((left, right));
                Ok(chain)
            }
            (_, RecursionVar { structure, .. }) => {
                dbg!("HERE");
                let mut chain = help(subs, needle, left, *structure)?;
                //chain.push((left, right));
                Ok(chain)
            }
            (LambdaSet(..), _) | (_, LambdaSet(..)) => {
                // NB: I've failed to construct a way for two lambda sets to be recursive and not
                // equal. My argument is that, for a lambda set to be recursive, it must be
                // owned by one of the closures it passes through. But a lambda set for a closure
                // is unique, so equivalent (recursive) lambda sets must be equal.
                //
                // As such they should never be involved in fixpoint fixing. I may be wrong,
                // though.
                Err(())
            }
            (Alias(_, _, left_inner, _), _) => {
                // Aliases can be different as long as we adjust their real variables.
                help(subs, needle, *left_inner, right)
            }
            (_, Alias(_, _, right_inner, _)) => {
                // Aliases can be different as long as we adjust their real variables.
                help(subs, needle, left, *right_inner)
            }
            (Structure(left_s), Structure(right_s)) => match (left_s, right_s) {
                (Apply(left_sym, left_vars), Apply(right_sym, right_vars)) => {
                    assert_eq!(left_sym, right_sym);
                    let mut chain = short_circuit(
                        subs,
                        needle,
                        subs.get_subs_slice(*left_vars).iter(),
                        subs.get_subs_slice(*right_vars).iter(),
                    )?;
                    chain.push((left, right));
                    Ok(chain)
                }
                (
                    Func(left_args, _left_clos, left_ret),
                    Func(right_args, _right_clos, right_ret),
                ) => {
                    // lambda sets are ignored; see the comment in the LambdaSet case above.
                    let check_args = |_| {
                        short_circuit(
                            subs,
                            needle,
                            subs.get_subs_slice(*left_args).iter(),
                            subs.get_subs_slice(*right_args).iter(),
                        )
                    };
                    let mut chain =
                        help(subs, needle, *left_ret, *right_ret).or_else(check_args)?;
                    chain.push((left, right));
                    Ok(chain)
                }
                (Record(left_fields, left_ext), Record(right_fields, right_ext)) => {
                    let mut left_it = left_fields.sorted_iterator(subs, *left_ext);
                    let mut right_it = right_fields.sorted_iterator(subs, *right_ext);
                    let mut chain = loop {
                        match (left_it.next(), right_it.next()) {
                            (Some((left_field, left_v)), Some((right_field, right_v))) => {
                                assert_eq!(left_field, right_field, "fields do not unify");
                                if let Ok(chain) =
                                    help(subs, needle, left_v.into_inner(), right_v.into_inner())
                                {
                                    break Ok(chain);
                                }
                            }
                            (None, None) => break Err(()),
                            _ => internal_error!("fields differ; does not unify"),
                        }
                    }?;
                    chain.push((left, right));
                    Ok(chain)
                }
                (
                    FunctionOrTagUnion(_left_tag_name, left_sym, left_var),
                    FunctionOrTagUnion(_right_tag_name, right_sym, right_var),
                ) => {
                    assert_eq!(left_sym, right_sym);
                    let mut chain = help(subs, needle, *left_var, *right_var)?;
                    chain.push((left, right));
                    Ok(chain)
                }
                (TagUnion(left_tags, left_ext), TagUnion(right_tags, right_ext))
                | (
                    RecursiveTagUnion(_, left_tags, left_ext),
                    RecursiveTagUnion(_, right_tags, right_ext),
                )
                | (TagUnion(left_tags, left_ext), RecursiveTagUnion(_, right_tags, right_ext))
                | (RecursiveTagUnion(_, left_tags, left_ext), TagUnion(right_tags, right_ext)) => {
                    let (left_it, _) = left_tags.sorted_iterator_and_ext(subs, *left_ext);
                    let (right_it, _) = right_tags.sorted_iterator_and_ext(subs, *right_ext);
                    assert_eq!(
                        left_it.len(),
                        right_it.len(),
                        "tag lengths differ; does not unify"
                    );

                    for ((left_tag, left_args), (right_tag, right_args)) in left_it.zip(right_it) {
                        assert_eq!(left_tag, right_tag);
                        if let Ok(mut chain) =
                            short_circuit(subs, needle, left_args.iter(), right_args.iter())
                        {
                            chain.push((left, right));
                            return Ok(chain);
                        }
                    }

                    Err(())
                }
                (EmptyRecord, EmptyRecord)
                | (EmptyTagUnion, EmptyTagUnion)
                | (Erroneous(_), Erroneous(_)) => Err(()),
                _ => internal_error!(
                    "structures {:?} and {:?} are not comparable; they do not unify!",
                    roc_types::subs::SubsFmtContent(&Structure(*left_s), subs),
                    roc_types::subs::SubsFmtContent(&Structure(*right_s), subs)
                ),
            },
            _ => internal_error!("types are not comparable; they do not unify!"),
        }
    }

    fn short_circuit<'a, T, U>(
        subs: &Subs,
        needle: (Variable, Variable),
        left_iter: T,
        right_iter: U,
    ) -> Result<Vec<(Variable, Variable)>, ()>
    where
        T: ExactSizeIterator<Item = &'a Variable>,
        U: ExactSizeIterator<Item = &'a Variable>,
    {
        assert_eq!(left_iter.len(), right_iter.len(), "types do not unify");

        for (left, right) in left_iter.zip(right_iter) {
            if let Ok(chain) = help(subs, needle, *left, *right) {
                return Ok(chain);
            }
        }

        Err(())
    }
}

#[must_use]
pub fn fix_fixpoint(subs: &mut Subs, left: Variable, right: Variable) -> Vec<Variable> {
    let updates = find_chain(subs, left, right);
    let mut new = vec![];
    for Update {
        source_of_truth,
        update_var,
    } in updates
    {
        let before_uset = (
            subs.get_root_key_without_compacting(source_of_truth),
            roc_types::subs::SubsFmtContent(
                subs.get_content_without_compacting(source_of_truth),
                subs,
            ),
            subs.get_root_key_without_compacting(update_var),
            roc_types::subs::SubsFmtContent(subs.get_content_without_compacting(update_var), subs),
        );
        dbg!(before_uset);
        //let source_of_truth_desc = subs.get_without_compacting(source_of_truth);
        // subs.union(source_of_truth, update_var, source_of_truth_desc);
        new.push((source_of_truth, update_var));
    }

    for &(source_of_truth, update_var) in new.iter() {
        let source_of_truth_desc = subs.get_without_compacting(source_of_truth);
        subs.union(source_of_truth, update_var, source_of_truth_desc);
        let after_uset = (
            subs.get_root_key_without_compacting(source_of_truth),
            roc_types::subs::SubsFmtContent(
                subs.get_content_without_compacting(source_of_truth),
                subs,
            ),
            subs.get_root_key_without_compacting(update_var),
            roc_types::subs::SubsFmtContent(subs.get_content_without_compacting(update_var), subs),
        );
        dbg!(after_uset);
    }

    dbg!(
        "UPDATED",
        subs.get_root_key_without_compacting(left),
        roc_types::subs::SubsFmtContent(subs.get_content_without_compacting(left), subs,),
        subs.get_root_key_without_compacting(right),
        roc_types::subs::SubsFmtContent(subs.get_content_without_compacting(right), subs,),
    );

    for &(source_of_truth, update_var) in new.iter() {
        let uset = (
            subs.get_root_key_without_compacting(source_of_truth),
            roc_types::subs::SubsFmtContent(
                subs.get_content_without_compacting(source_of_truth),
                subs,
            ),
            subs.get_root_key_without_compacting(update_var),
            roc_types::subs::SubsFmtContent(subs.get_content_without_compacting(update_var), subs),
        );
        dbg!(uset);
    }

    new.into_iter()
        .flat_map(|(_, update_var)| [update_var])
        .collect()
}
