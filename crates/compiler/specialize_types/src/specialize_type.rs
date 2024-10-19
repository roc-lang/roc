/// Given a Subs that's been populated from type inference, and a Variable,
/// ensure that Variable is monomorphic by going through and creating
/// specializations of that type wherever necessary.
///
/// This only operates at the type level. It does not create new function implementations (for example).
use crate::{
    debug_info::DebugInfo,
    mono_type::{MonoTypeId, MonoTypes},
    MonoFieldId, MonoType,
};
use roc_collections::{Push, VecMap};
use roc_module::ident::{Lowercase, TagName};
use roc_types::subs::{
    Content, FlatType, RecordFields, Subs, TagExt, TupleElems, UnionLabels, UnionTags, Variable,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Problem {
    // Compiler bugs; these should never happen!
    TagUnionExtWasNotTagUnion,
    RecordExtWasNotRecord,
    TupleExtWasNotTuple,
    /// This can be either an integer specializing to a fractional number type (or vice versa),
    /// or the type paramter specializing to a non-numeric type (e.g. Num Str), which should
    /// have been caught during type-checking and changed to an Error type.
    NumSpecializedToWrongType(
        Option<MonoType>, // `None` means it specialized to Unit
    ),
}

/// For MonoTypes that are records, store their field indices.
pub type RecordFieldIds = VecMap<MonoTypeId, VecMap<Lowercase, MonoFieldId>>;

/// For MonoTypes that are tuples, store their element indices.
/// (These are not necessarily the same as their position in the monomorphized tuple,
/// because we may have deleted some zero-sized types in the middle - yet expressions
/// will still refer to e.g. `tuple.1`, so we still need to know which element `.1`
/// referred to originally before we deleted things.
pub type TupleElemIds = VecMap<MonoTypeId, VecMap<u16, MonoFieldId>>;

/// Variables that have already been monomorphized.
pub struct MonoCache {
    inner: VecMap<Variable, MonoTypeId>,
}

impl MonoCache {
    pub fn from_subs(subs: &Subs) -> Self {
        Self {
            inner: VecMap::with_capacity(subs.len()),
        }
    }

    /// Returns None if it monomorphizes to a type that should be eliminated
    /// (e.g. a zero-sized type like empty record, empty tuple, a record of just those, etc.)
    pub fn monomorphize_var(
        &mut self,
        subs: &Subs,
        mono_types: &mut MonoTypes,
        field_indices: &mut RecordFieldIds,
        elem_indices: &mut TupleElemIds,
        problems: &mut impl Push<Problem>,
        debug_info: &mut Option<DebugInfo>,
        var: Variable,
    ) -> Option<MonoTypeId> {
        let mut env = Env {
            cache: self,
            mono_types,
            field_ids: field_indices,
            elem_ids: elem_indices,
            problems,
            debug_info,
        };

        lower_var(&mut env, subs, var)
    }
}

struct Env<'c, 'd, 'e, 'f, 'm, 'p, P: Push<Problem>> {
    cache: &'c mut MonoCache,
    mono_types: &'m mut MonoTypes,
    field_ids: &'f mut RecordFieldIds,
    elem_ids: &'e mut TupleElemIds,
    problems: &'p mut P,
    debug_info: &'d mut Option<DebugInfo>,
}

fn lower_var<P: Push<Problem>>(
    env: &mut Env<'_, '_, '_, '_, '_, '_, P>,
    subs: &Subs,
    var: Variable,
) -> Option<MonoTypeId> {
    let root_var = subs.get_root_key_without_compacting(var);

    // TODO: we could replace this cache by having Subs store a Content::Monomorphic(MonoTypeId)
    // and then overwrite it rather than having a separate cache. That memory is already in cache
    // for sure, and the lookups should be faster because they're O(1) but don't require hashing.
    if let Some(mono_id) = env.cache.inner.get(&root_var) {
        return Some(*mono_id);
    }

    // Convert the Content to a MonoType, often by passing an iterator. None of these iterators introduce allocations.
    let opt_mono_id = match *subs.get_content_without_compacting(root_var) {
        Content::Structure(flat_type) => match flat_type {
            FlatType::Apply(symbol, args) => {
                let new_args = args
                    .into_iter()
                    .flat_map(|var_index| lower_var(env, subs, subs[var_index]));

                todo!("maybe instead of making new_args, branch and then call lower_var to create Primitive, List, Box, etc.");
            }
            // FlatType::Func(args, _capture, ret) => {
            //     let mono_args = args
            //         .into_iter()
            //         .flat_map(|var_index| lower_var(env, subs[var_index]));

            //     let todo = (); // TODO populate debuginfo (if it's Some, meaning we want it)
            //     let func = lower_var(env, ret);
            //     Some(env.mono_types.add_function(func, mono_args))
            // }
            _ => {
                todo!();
            } /*
                  FlatType::Record(fields, ext) => {
                      let mut labeled_mono_ids = lower_record(env, fields, ext);

                      // Handle the special cases of 0 fields and 1 field.
                      match labeled_mono_ids.first() {
                          Some((label, first_field_id)) => {
                              if labeled_mono_ids.len() == 1 {
                                  // If we ended up with a single field, return it unwrapped.
                                  let todo = (); // TODO populate debuginfo using the label (if it's Some, meaning we want it)
                                  let todo = (); // To preserve debuginfo, we need to actually clone this mono_id and not just return the same one.
                                  return Some(*first_field_id);
                              }
                          }
                          None => {
                              // If we ended up with an empty record,
                              // after removing other empty things, return None.
                              return None;
                          }
                      }

                      // Now we know we have at least 2 fields, so sort them by field name.
                      // This can be unstable sort because all field names are known to be unique,
                      // so sorting unstable won't be observable (and is faster than stable).
                      labeled_mono_ids.sort_unstable_by(|(label1, _), (label2, _)| label1.cmp(label2));

                      let todo = (); // TODO populate debuginfo (if it's Some, meaning we want it)

                      // Safety: we already verified that this has at least 2 elements, and
                      // we would have early returned before this point if we had fewer than 2.
                      let mono_id = unsafe {
                          mono_types.add_struct_unchecked(labeled_mono_ids.iter().map(|(_label, mono_id)| *mono_id))
                      };

                      let labeled_indices = VecMap::from_iter(labeled_mono_ids.into_iter().enumerate().map(|(index, (label, _mono_id))| (label, MonoFieldId::new(index as u16))));

                      env.field_ids.insert(mono_id, labeled_indices);

                      Some(mono_id)
                  }
                  FlatType::Tuple(elems, ext) => {
                      let indexed_mono_ids = lower_tuple(env, elems, ext);

                      // This can be unstable sort because all indices are known to be unique,
                      // so sorting unstable won't be observable (and is faster than stable).
                      indexed_mono_ids.sort_unstable_by(|(index1, _), (index2, _)| index1.cmp(index2));

                      let todo = (); // TODO populate debuginfo (if it's Some, meaning we want it)
                      mono_types.add_struct(indexed_mono_ids.iter().map(|(_, mono_id)| *mono_id))
                  }
                  FlatType::TagUnion(tags, ext) => {
                      let tagged_payload_ids = lower_tag_union(env, tags, ext);

                      // This can be unstable sort because all tag names are known to be unique,
                      // so sorting unstable won't be observable (and is faster than stable).
                      tagged_payload_ids.sort_unstable_by(|(tag1, _), (tag2, _)| tag1.cmp(tag2));

                      let todo = (); // TODO populate debuginfo (if it's Some, meaning we want it)
                      mono_types.add_tag_union(tagged_payload_ids.iter().map(|(_, mono_id)| *mono_id))
                  }
                  FlatType::FunctionOrTagUnion(tag_names, _symbols, ext) => {
                      // If this is still a FunctionOrTagUnion, turn it into a TagUnion.

                      // First, resolve the ext var.
                      let mut tags = resolve_tag_ext(subs, problems, UnionTags::default(), *ext);

                      // Now lower all the tags we gathered from the ext var.
                      // (Do this in a separate pass to avoid borrow errors on Subs.)
                      lower_vars(tags.iter_mut().flat_map(|(_, vars)| vars.iter_mut()), cache, subs, problems);

                      // Then, add the tag names with no payloads. (There are no variables to lower here.)
                      for index in tag_names.into_iter() {
                          tags.push(((subs[index]).clone(), Vec::new()));
                      }

                      Content::Structure(FlatType::TagUnion(
                          UnionTags::insert_into_subs(subs, tags),
                          TagExt::Any(Variable::EMPTY_TAG_UNION),
                      ))
                  }
                  FlatType::RecursiveTagUnion(rec, tags, ext) => {
                      let mut tags = resolve_tag_ext(subs, problems, *tags, *ext);

                      // Now lower all the tags we gathered. Do this in a separate pass to avoid borrow errors on Subs.
                      lower_vars(tags.iter_mut().flat_map(|(_, vars)| vars.iter_mut()), cache, subs, problems);

                      Content::Structure(FlatType::RecursiveTagUnion(
                          lower_var(cache, subs, problems, *rec),
                          UnionTags::insert_into_subs(subs, tags),
                          TagExt::Any(Variable::EMPTY_TAG_UNION),
                      ))
                  }
                  FlatType::EmptyRecord|
                  FlatType::EmptyTuple |
                  FlatType::EmptyTagUnion => None,
              },
              Content::RangedNumber(_) // RangedNumber goes in Num's type parameter slot, so monomorphize it to []
              | Content::FlexVar(_)
              | Content::RigidVar(_)
              | Content::FlexAbleVar(_, _)
              | Content::RigidAbleVar(_, _)
              | Content::RecursionVar { .. } => Content::Structure(FlatType::EmptyTagUnion),
              Content::LambdaSet(lambda_set) => Content::LambdaSet(lambda_set),
              Content::ErasedLambda => Content::ErasedLambda,
              Content::Alias(symbol, args, real, kind) => {
                  let todo = (); // TODO we should unwrap this, but doing that probably means changing this from root_var to other stuff.
                  let new_real = lower_var(cache, subs, problems, *real);
                  Content::Alias(*symbol, *args, new_real, *kind)
              }
              Content::Error => Content::Error,
               */
        },
        _ => {
            todo!();
        }
    };

    if let Some(mono_id) = opt_mono_id {
        // This var is now known to be monomorphic, so we don't repeat this work again later.
        // (We don't insert entries for Unit values.)
        env.cache.inner.insert(root_var, mono_id);
    }

    opt_mono_id
}

fn resolve_tag_ext(
    subs: &mut Subs,
    mono_types: &mut MonoTypes,
    problems: &mut impl Push<Problem>,
    mut tags: UnionTags,
    mut ext: TagExt,
) -> Vec<(TagName, Vec<Variable>)> {
    let mut all_tags = Vec::new();

    // Collapse (recursively) all the tags in ext_var into a flat list of tags.
    loop {
        for (tag, vars) in tags.iter_from_subs(subs) {
            all_tags.push((tag.clone(), vars.to_vec()));
        }

        match subs.get_content_without_compacting(ext.var()) {
            Content::Structure(FlatType::TagUnion(new_tags, new_ext)) => {
                // Update tags and ext and loop back again to process them.
                tags = *new_tags;
                ext = *new_ext;
            }
            Content::Structure(FlatType::FunctionOrTagUnion(tag_names, _symbols, new_ext)) => {
                for index in tag_names.into_iter() {
                    all_tags.push((subs[index].clone(), Vec::new()));
                }
                ext = *new_ext;
            }
            Content::Structure(FlatType::EmptyTagUnion) => break,
            Content::FlexVar(_) | Content::FlexAbleVar(_, _) => break,
            Content::Alias(_, _, real, _) => {
                // Follow the alias and process it on the next iteration of the loop.
                ext = TagExt::Any(*real);

                // We just processed these tags, so don't process them again!
                tags = UnionLabels::default();
            }
            _ => {
                // This should never happen! If it does, record a Problem and break.
                problems.push(Problem::TagUnionExtWasNotTagUnion);

                break;
            }
        }
    }

    all_tags
}

fn lower_record<P: Push<Problem>>(
    env: &mut Env<'_, '_, '_, '_, '_, '_, P>,
    subs: &Subs,
    mut fields: RecordFields,
    mut ext: Variable,
) -> Vec<(Lowercase, Option<MonoTypeId>)> {
    let mut labeled_mono_ids = Vec::with_capacity(fields.len());

    // Collapse (recursively) all the fields in ext into a flat list of fields.
    loop {
        // Add all the current fields to the answer.
        labeled_mono_ids.extend(
            fields
                .sorted_iterator(subs, ext)
                .map(|(label, field)| (label, lower_var(env, subs, *field.as_inner()))),
        );

        // If the ext record is nonempty, set its fields to be the next ones we handle, and loop back.
        match subs.get_content_without_compacting(ext) {
            Content::Structure(FlatType::Record(new_fields, new_ext)) => {
                // Update fields and ext and loop back again to process them.
                fields = *new_fields;
                ext = *new_ext;
            }
            Content::Structure(FlatType::EmptyRecord)
            | Content::FlexVar(_)
            | Content::FlexAbleVar(_, _) => return labeled_mono_ids,
            Content::Alias(_, _, real, _) => {
                // Follow the alias and process it on the next iteration of the loop.
                ext = *real;

                // We just processed these fields, so don't process them again!
                fields = RecordFields::empty();
            }
            _ => {
                // This should never happen! If it does, record a Problem and early return.
                env.problems.push(Problem::RecordExtWasNotRecord);

                return labeled_mono_ids;
            }
        }
    }
}

fn resolve_tuple_ext(
    subs: &mut Subs,
    mono_types: &mut MonoTypes,
    problems: &mut impl Push<Problem>,
    mut elems: TupleElems,
    mut ext: Variable,
) -> Vec<(usize, Variable)> {
    let mut all_elems = Vec::new();

    // Collapse (recursively) all the elements in ext into a flat list of elements.
    loop {
        for (idx, var_index) in elems.iter_all() {
            all_elems.push((idx.index as usize, subs[var_index]));
        }

        match subs.get_content_without_compacting(ext) {
            Content::Structure(FlatType::Tuple(new_elems, new_ext)) => {
                // Update elems and ext and loop back again to process them.
                elems = *new_elems;
                ext = *new_ext;
            }
            Content::Structure(FlatType::EmptyTuple) => break,
            Content::FlexVar(_) | Content::FlexAbleVar(_, _) => break,
            Content::Alias(_, _, real, _) => {
                // Follow the alias and process it on the next iteration of the loop.
                ext = *real;

                // We just processed these elements, so don't process them again!
                elems = TupleElems::empty();
            }
            _ => {
                // This should never happen! If it does, record a Problem and break.
                problems.push(Problem::TupleExtWasNotTuple);

                break;
            }
        }
    }

    all_elems
}

// /// Lower the given vars in-place.
// fn lower_vars<'a>(
//     vars: impl Iterator<Item = &'a mut Variable>,
//     cache: &mut MonoCache,
//     subs: &mut Subs,
//     mono_types: &mut MonoTypes,
//     problems: &mut impl Push<Problem>,
// ) {
//     for var in vars {
//         if let Some(var) = lower_var(env, *var) // hmm not sure if this is still a good idea as a helper function
//         *var = ;
//     }
// }
