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
    /// Compiler bug; this should never happen!
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
    match dbg!(content) {
        Content::Structure(flat_type) => match flat_type {
            FlatType::Apply(symbol, args) => {
                let new_args = args
                    .into_iter()
                    .map(|var_index| lower_var(cache, subs, problems, subs[var_index]))
                    // TODO there might be a way to remove this heap allocation
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
                let mut fields = flatten_fields(subs, problems, *fields, *ext);

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
                let mut elems = flatten_tuple(subs, problems, *elems, *ext);

                // Now lower all the elems we gathered. Do this in a separate pass to avoid borrow errors on Subs.
                for (_, var) in elems.iter_mut() {
                    *var = lower_var(cache, subs, problems, *var);
                }

                Content::Structure(FlatType::Tuple(
                    TupleElems::insert_into_subs(subs, elems.into_iter()),
                    Variable::EMPTY_TUPLE,
                ))
            }
            FlatType::TagUnion(tags, ext) => {
                let mut tags = flatten_tags(subs, problems, *tags, *ext);

                // Now lower all the tags we gathered. Do this in a separate pass to avoid borrow errors on Subs.
                for (_, vars) in tags.iter_mut() {
                    for var in vars.iter_mut() {
                        *var = lower_var(cache, subs, problems, *var);
                    }
                }

                Content::Structure(FlatType::TagUnion(
                    UnionTags::insert_into_subs(subs, tags.into_iter()),
                    TagExt::Any(Variable::EMPTY_TAG_UNION),
                ))
            }
            FlatType::FunctionOrTagUnion(tag_names, symbols, ext) => {
                let new_ext = TagExt::Any(lower_var(cache, subs, problems, ext.var()));
                Content::Structure(FlatType::FunctionOrTagUnion(*tag_names, *symbols, new_ext))
            }
            FlatType::RecursiveTagUnion(rec, tags, ext) => {
                let mut new_tags = flatten_tags(subs, problems, *tags, *ext);

                // Now lower all the tags we gathered. Do this in a separate pass to avoid borrow errors on Subs.
                for (_, vars) in new_tags.iter_mut() {
                    for var in vars.iter_mut() {
                        *var = lower_var(cache, subs, problems, *var);
                    }
                }

                Content::Structure(FlatType::RecursiveTagUnion(
                    lower_var(cache, subs, problems, *rec),
                    UnionTags::insert_into_subs(subs, new_tags.into_iter()),
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

fn flatten_tags(
    subs: &mut Subs,
    problems: &mut Vec<Problem>,
    mut tags: UnionTags,
    mut ext: TagExt,
) -> Vec<(TagName, Vec<Variable>)> {
    let mut all_tags = Vec::new();

    // First, collapse (recursively) all the tags in ext_var into a flat list of tags.
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

    // ext should have ended up being empty
    debug_assert_eq!(ext.var(), Variable::EMPTY_TAG_UNION);

    all_tags
}

fn flatten_fields(
    subs: &mut Subs,
    problems: &mut Vec<Problem>,
    mut fields: RecordFields,
    mut ext: Variable,
) -> Vec<(Lowercase, RecordField<Variable>)> {
    let mut all_fields = Vec::new();

    // First, collapse (recursively) all the fields in ext into a flat list of fields.
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

    // ext should have ended up being empty
    debug_assert_eq!(ext, Variable::EMPTY_RECORD);

    all_fields
}

fn flatten_tuple(
    subs: &mut Subs,
    problems: &mut Vec<Problem>,
    mut elems: TupleElems,
    mut ext: Variable,
) -> Vec<(usize, Variable)> {
    let mut all_elems = Vec::new();

    // First, collapse (recursively) all the elements in ext into a flat list of elements.
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

    // ext should have ended up being empty
    debug_assert_eq!(ext, Variable::EMPTY_TUPLE);

    all_elems
}
