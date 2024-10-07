/// Given a Subs that's been populated from type inference, and a Variable,
/// ensure that Variable is monomorphic by going through and creating
/// specializations of that type wherever necessary.
///
/// This only operates at the type level. It does not create new function implementations (for example).
use bitvec::vec::BitVec;
use roc_module::ident::{Lowercase, TagName};
use roc_types::{
    subs::{
        Content, FlatType, RecordFields, Subs, TagExt, TupleElems, UnionLabels, UnionTags,
        Variable, VariableSubsSlice,
    },
    types::RecordField,
};

#[derive(Debug, PartialEq, Eq)]
pub enum Problem {
    // Compiler bugs; these should never happen!
    TagUnionExtWasNotTagUnion,
    RecordExtWasNotRecord,
    TupleExtWasNotTuple,
}

/// Variables that have already been monomorphized.
pub struct MonoCache {
    inner: BitVec,
}

impl MonoCache {
    pub fn from_subs(subs: &Subs) -> Self {
        Self {
            inner: BitVec::repeat(false, subs.len()),
        }
    }

    /// Returns true iff we know this Variable is monomorphic because
    /// we've visited it before in the monomorphization process
    /// (and either it was already monomorphic, or we made it so).
    pub fn is_known_monomorphic(&self, var: Variable) -> bool {
        match self.inner.get(var.index() as usize) {
            Some(initialized) => {
                // false if it has never been set, because we initialized all the bits to 0
                *initialized
            }
            None => false,
        }
    }

    /// Records that the given variable is now known to be monomorphic.
    fn set_monomorphic(&mut self, var: Variable) {
        self.inner.set(var.index() as usize, true);
    }

    pub fn monomorphize_var(
        &mut self,
        subs: &mut Subs,
        problems: &mut Vec<Problem>,
        var: Variable,
    ) {
        lower_var(self, subs, problems, var);
    }
}

fn lower_var(
    cache: &mut MonoCache,
    subs: &mut Subs,
    problems: &mut Vec<Problem>,
    var: Variable,
) -> Variable {
    let root_var = subs.get_root_key_without_compacting(var);

    if !cache.is_known_monomorphic(root_var) {
        let content = subs.get_content_without_compacting(root_var).clone();
        let content = lower_content(cache, subs, problems, &content);

        // Update Subs so when we look up this var in the future, it's the monomorphized Content.
        subs.set_content(root_var, content);

        // This var is now known to be monomorphic.
        cache.set_monomorphic(root_var);
    }

    root_var
}

fn lower_content(
    cache: &mut MonoCache,
    subs: &mut Subs,
    problems: &mut Vec<Problem>,
    content: &Content,
) -> Content {
    match content {
        Content::Structure(flat_type) => match flat_type {
            FlatType::Apply(symbol, args) => {
                let new_args = args
                    .into_iter()
                    .map(|var_index| lower_var(cache, subs, problems, subs[var_index]))
                    .collect::<Vec<Variable>>();

                Content::Structure(FlatType::Apply(
                    *symbol,
                    VariableSubsSlice::insert_into_subs(subs, new_args),
                ))
            }
            FlatType::Func(args, closure, ret) => {
                let new_args = args
                    .into_iter()
                    .map(|var_index| lower_var(cache, subs, problems, subs[var_index]))
                    .collect::<Vec<_>>();
                let new_closure = lower_var(cache, subs, problems, *closure);
                let new_ret = lower_var(cache, subs, problems, *ret);
                Content::Structure(FlatType::Func(
                    VariableSubsSlice::insert_into_subs(subs, new_args),
                    new_closure,
                    new_ret,
                ))
            }
            FlatType::Record(fields, ext) => {
                let mut fields = resolve_record_ext(subs, problems, *fields, *ext);

                // Now lower all the fields we gathered. Do this in a separate pass to avoid borrow errors on Subs.
                for (_, field) in fields.iter_mut() {
                    let var = match field {
                        RecordField::Required(v) | RecordField::Optional(v) | RecordField::Demanded(v) => v,
                        RecordField::RigidRequired(v) | RecordField::RigidOptional(v) => v,
                    };

                    *var = lower_var(cache, subs, problems, *var);
                }

                Content::Structure(FlatType::Record(
                    RecordFields::insert_into_subs(subs, fields.into_iter()),
                    Variable::EMPTY_RECORD,
                ))
            }
            FlatType::Tuple(elems, ext) => {
                let mut elems = resolve_tuple_ext(subs, problems, *elems, *ext);

                // Now lower all the elems we gathered. Do this in a separate pass to avoid borrow errors on Subs.
                lower_vars(elems.iter_mut().map(|(_, var)| var), cache, subs, problems);

                Content::Structure(FlatType::Tuple(
                    TupleElems::insert_into_subs(subs, elems.into_iter()),
                    Variable::EMPTY_TUPLE,
                ))
            }
            FlatType::TagUnion(tags, ext) => {
                let mut tags = resolve_tag_ext(subs, problems, *tags, *ext);

                // Now lower all the tags we gathered. Do this in a separate pass to avoid borrow errors on Subs.
                lower_vars(tags.iter_mut().flat_map(|(_, vars)| vars.iter_mut()), cache, subs, problems);

                Content::Structure(FlatType::TagUnion(
                    UnionTags::insert_into_subs(subs, tags.into_iter()),
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
                    tags.push(((&subs[index]).clone(), Vec::new()));
                }

                Content::Structure(FlatType::TagUnion(
                    UnionTags::insert_into_subs(subs, tags.into_iter()),
                    TagExt::Any(Variable::EMPTY_TAG_UNION),
                ))
            }
            FlatType::RecursiveTagUnion(rec, tags, ext) => {
                let mut tags = resolve_tag_ext(subs, problems, *tags, *ext);

                // Now lower all the tags we gathered. Do this in a separate pass to avoid borrow errors on Subs.
                lower_vars(tags.iter_mut().flat_map(|(_, vars)| vars.iter_mut()), cache, subs, problems);

                Content::Structure(FlatType::RecursiveTagUnion(
                    lower_var(cache, subs, problems, *rec),
                    UnionTags::insert_into_subs(subs, tags.into_iter()),
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
        Content::LambdaSet(lambda_set) => Content::LambdaSet(lambda_set.clone()),
        Content::ErasedLambda => Content::ErasedLambda,
        Content::Alias(symbol, args, real, kind) => {
            let new_real = lower_var(cache, subs, problems, *real);
            Content::Alias(*symbol, args.clone(), new_real, *kind)
        }
        Content::Error => Content::Error,
    }
}

fn resolve_tag_ext(
    subs: &mut Subs,
    problems: &mut Vec<Problem>,
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

fn resolve_record_ext(
    subs: &mut Subs,
    problems: &mut Vec<Problem>,
    mut fields: RecordFields,
    mut ext: Variable,
) -> Vec<(Lowercase, RecordField<Variable>)> {
    let mut all_fields = Vec::new();

    // Collapse (recursively) all the fields in ext into a flat list of fields.
    loop {
        for (label, field) in fields.sorted_iterator(subs, ext) {
            all_fields.push((label.clone(), field.clone()));
        }

        match subs.get_content_without_compacting(ext) {
            Content::Structure(FlatType::Record(new_fields, new_ext)) => {
                // Update fields and ext and loop back again to process them.
                fields = *new_fields;
                ext = *new_ext;
            }
            Content::Structure(FlatType::EmptyRecord) => break,
            Content::FlexVar(_) | Content::FlexAbleVar(_, _) => break,
            Content::Alias(_, _, real, _) => {
                // Follow the alias and process it on the next iteration of the loop.
                ext = *real;

                // We just processed these fields, so don't process them again!
                fields = RecordFields::empty();
            }
            _ => {
                // This should never happen! If it does, record a Problem and break.
                problems.push(Problem::RecordExtWasNotRecord);

                break;
            }
        }
    }

    all_fields
}

fn resolve_tuple_ext(
    subs: &mut Subs,
    problems: &mut Vec<Problem>,
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
    problems: &mut Vec<Problem>,
) {
    for var in vars {
        *var = lower_var(cache, subs, problems, *var);
    }
}
