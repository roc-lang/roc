/// Given a Subs that's been populated from type inference, and a Variable,
/// ensure that Variable is monomorphic by going through and creating
/// specializations of that type wherever necessary.
///
/// This only operates at the type level. It does not create new function implementations (for example).
use crate::{
    debug_info::DebugInfo,
    mono_type::{self, MonoType, MonoTypes},
};
use core::iter;
use roc_collections::{Push, VecMap};
use roc_module::ident::{Lowercase, TagName};
use roc_types::{
    subs::{
        Content, FlatType, RecordFields, SortedFieldIterator, Subs, TagExt, TupleElems,
        UnionLabels, UnionTags, Variable,
    },
    types::RecordField,
};
use soa::Id;

#[derive(Debug, PartialEq, Eq)]
pub enum Problem {
    // Compiler bugs; these should never happen!
    TagUnionExtWasNotTagUnion,
    RecordExtWasNotRecord,
    TupleExtWasNotTuple,
}

/// Variables that have already been monomorphized.
pub struct MonoCache {
    inner: VecMap<Variable, Id<MonoType>>,
}

impl MonoCache {
    pub fn from_subs(subs: &Subs) -> Self {
        Self {
            inner: VecMap::with_capacity(subs.len()),
        }
    }

    pub fn monomorphize_var(
        &mut self,
        subs: &mut Subs,
        mono_types: &mut MonoTypes,
        problems: &mut impl Push<Problem>,
        debug_info: &mut Option<DebugInfo>,
        var: Variable,
    ) -> Id<MonoType> {
        lower_var(
            Env {
                cache: self,
                subs,
                mono_types,
                problems,
                debug_info,
            },
            var,
        )
    }
}

struct Env<'c, 'd, 'm, 'p, 's, P: Push<Problem>> {
    cache: &'c mut MonoCache,
    subs: &'s mut Subs,
    mono_types: &'m mut MonoTypes,
    problems: &'p mut P,
    debug_info: &'d mut Option<DebugInfo>,
}

fn lower_var<P: Push<Problem>>(env: Env<'_, '_, '_, '_, '_, P>, var: Variable) -> Id<MonoType> {
    let cache = env.cache;
    let subs = env.subs;
    let root_var = subs.get_root_key_without_compacting(var);

    // TODO: we could replace this cache by having Subs store a Content::Monomorphic(Id<MonoType>)
    // and then overwrite it rather than having a separate cache. That memory is already in cache
    // for sure, and the lookups should be faster because they're O(1) but don't require hashing.
    if let Some(mono_id) = cache.inner.get(&root_var) {
        return *mono_id;
    }

    let mono_types = env.mono_types;
    let problems = env.problems;

    // Convert the Content to a MonoType, often by passing an iterator. None of these iterators introduce allocations.
    let mono_id = match *subs.get_content_without_compacting(root_var) {
            Content::Structure(flat_type) => match flat_type {
                FlatType::Apply(symbol, args) => {
                    let new_args = args
                        .into_iter()
                        .map(|var_index| lower_var(env, subs[var_index]));

                    mono_types.add_apply(symbol, new_args)
                }
                FlatType::Func(args, closure, ret) => {
                    let new_args = args
                        .into_iter()
                        .map(|var_index| lower_var(env, subs[var_index]));

                    let new_closure = lower_var(env,  closure);
                    let new_ret = lower_var(env,  ret);

                    mono_types.add_function(new_closure, new_ret, new_args)
                }
                FlatType::Record(fields, ext) => {
                    let todo = (); // TODO populate debuginfo (if it's Some, meaning we want it)
                    mono_types.add_record(
                        LowerRecordIterator {
                            cache,
                            subs,
                            mono_types,
                            problems,
                            fields: fields.sorted_iterator(subs, ext),
                            ext,
                        })
                }
                FlatType::Tuple(elems, ext) => {
                    let todo = (); // TODO populate debuginfo (if it's Some, meaning we want it)
                    mono_types.add_tuple(
                        LowerTupleIterator {
                            cache,
                            subs,
                            mono_types,
                            problems,
                            elems: elems.sorted_iterator(subs, ext),
                            ext,
                        })
                }
                FlatType::TagUnion(tags, ext) => {
                    let mut tags = resolve_tag_ext(subs, problems, *tags, *ext);

                    // Now lower all the tags we gathered. Do this in a separate pass to avoid borrow errors on Subs.
                    lower_vars(tags.iter_mut().flat_map(|(_, vars)| vars.iter_mut()), cache, subs, problems);

                    Content::Structure(FlatType::TagUnion(
                        UnionTags::insert_into_subs(subs, tags),
                        TagExt::Any(Variable::EMPTY_TAG_UNION),
                    ))
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
                FlatType::EmptyRecord => Content::Structure(FlatType::EmptyRecord),
                FlatType::EmptyTuple => Content::Structure(FlatType::EmptyTuple),
                FlatType::EmptyTagUnion => Content::Structure(FlatType::EmptyTagUnion),
            },
            Content::RangedNumber(_) // RangedNumber goes in Num's type parameter slot, so monomorphize it to []
            | Content::FlexVar(_)
            | Content::RigidVar(_)
            | Content::FlexAbleVar(_, _)
            | Content::RigidAbleVar(_, _)
            | Content::RecursionVar { .. } => Content::Structure(FlatType::EmptyTagUnion),
            Content::LambdaSet(lambda_set) => Content::LambdaSet(*lambda_set),
            Content::ErasedLambda => Content::ErasedLambda,
            Content::Alias(symbol, args, real, kind) => {
                let todo = (); // TODO we should unwrap this, but doing that probably means changing this from root_var to other stuff.
                let new_real = lower_var(cache, subs, problems, *real);
                Content::Alias(*symbol, *args, new_real, *kind)
            }
            Content::Error => Content::Error,
        };

    // This var is now known to be monomorphic, so we don't repeat this work again later.
    cache.inner.insert(root_var, mono_id);

    mono_id
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

struct LowerRecordIterator<'c, 's, 'm, 'p, 'r, P: Push<Problem>> {
    cache: &'c mut MonoCache,
    subs: &'s mut Subs,
    mono_types: &'m mut MonoTypes,
    problems: &'p mut P,
    fields: SortedFieldIterator<'r>, // TODO impl Iterator
    ext: Variable,
}

impl<'c, 's, 'm, 'p, 'r, P> Iterator for LowerRecordIterator<'c, 's, 'm, 'p, 'r, P>
where
    P: Push<Problem>,
{
    type Item = (Lowercase, Id<MonoType>);

    fn next(&mut self) -> Option<Self::Item> {
        // Collapse (recursively) all the fields in ext into a flat list of fields.
        loop {
            if let Some((label, field)) = self.fields.next() {
                let var = match field {
                    RecordField::Demanded(var) => var,
                    RecordField::Required(var) => var,
                    RecordField::Optional(var) => var,
                    RecordField::RigidRequired(var) => var,
                    RecordField::RigidOptional(var) => var,
                };

                return Some((
                    label,
                    lower_var(self.cache, self.subs, self.mono_types, self.problems, var),
                ));
            }

            match self.subs.get_content_without_compacting(self.ext) {
                Content::Structure(FlatType::Record(new_fields, new_ext)) => {
                    // Update fields and ext and loop back again to process them.
                    self.fields = new_fields.sorted_iterator(self.subs, self.ext);
                    self.ext = *new_ext;
                }
                Content::Structure(FlatType::EmptyRecord) => return None,
                Content::FlexVar(_) | Content::FlexAbleVar(_, _) => return None,
                Content::Alias(_, _, real, _) => {
                    // Follow the alias and process it on the next iteration of the loop.
                    self.ext = *real;

                    // We just processed these fields, so don't process them again!
                    self.fields = Box::new(iter::empty());
                }
                _ => {
                    // This should never happen! If it does, record a Problem and break.
                    self.problems.push(Problem::RecordExtWasNotRecord);

                    return None;
                }
            }
        }
    }
}

struct LowerTupleIterator<'c, 's, 'm, 'p, 'r, P: Push<Problem>> {
    cache: &'c mut MonoCache,
    subs: &'s mut Subs,
    mono_types: &'m mut MonoTypes,
    problems: &'p mut P,
    fields: SortedElemsIterator<'r>,
    ext: Variable,
}

impl<'c, 's, 'm, 'p, 'r, P> Iterator for LowerRecordIterator<'c, 's, 'm, 'p, 'r, P>
where
    P: Push<Problem>,
{
    type Item = (Lowercase, Id<MonoType>);

    fn next(&mut self) -> Option<Self::Item> {
        // Collapse (recursively) all the fields in ext into a flat list of fields.
        loop {
            if let Some((label, field)) = self.fields.next() {
                let var = match field {
                    RecordField::Demanded(var) => var,
                    RecordField::Required(var) => var,
                    RecordField::Optional(var) => var,
                    RecordField::RigidRequired(var) => var,
                    RecordField::RigidOptional(var) => var,
                };

                return Some((
                    label,
                    lower_var(self.cache, self.subs, self.mono_types, self.problems, var),
                ));
            }

            match self.subs.get_content_without_compacting(self.ext) {
                Content::Structure(FlatType::Record(new_fields, new_ext)) => {
                    // Update fields and ext and loop back again to process them.
                    self.fields = new_fields.sorted_iterator(self.subs, self.ext);
                    self.ext = *new_ext;
                }
                Content::Structure(FlatType::EmptyRecord) => return None,
                Content::FlexVar(_) | Content::FlexAbleVar(_, _) => return None,
                Content::Alias(_, _, real, _) => {
                    // Follow the alias and process it on the next iteration of the loop.
                    self.ext = *real;

                    // We just processed these fields, so don't process them again!
                    self.fields = Box::new(iter::empty());
                }
                _ => {
                    // This should never happen! If it does, record a Problem and break.
                    self.problems.push(Problem::RecordExtWasNotRecord);

                    return None;
                }
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

/// Lower the given vars in-place.
fn lower_vars<'a>(
    vars: impl Iterator<Item = &'a mut Variable>,
    cache: &mut MonoCache,
    subs: &mut Subs,
    mono_types: &mut MonoTypes,
    problems: &mut impl Push<Problem>,
) {
    for var in vars {
        *var = lower_var(env, *var);
    }
}
