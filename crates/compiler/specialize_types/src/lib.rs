use roc_collections::MutMap;
use roc_module::ident::{Lowercase, TagName};
use roc_types::{
    subs::{
        Content, FlatType, RecordFields, Subs, TagExt, TupleElems, UnionTags, Variable,
        VariableSubsSlice,
    },
    types::RecordField,
};

pub type MonoCache = MutMap<Variable, Content>;

fn ty_unfilled() -> Content {
    Content::Structure(FlatType::TagUnion(
        UnionTags::default(),
        TagExt::Any(Variable::EMPTY_TAG_UNION),
    ))
}

pub fn monomorphize_var(cache: &mut MonoCache, subs: &Subs, var: Variable) -> Variable {
    let root_var = subs.get_root_key_without_compacting(var);
    if cache.contains_key(&root_var) {
        return root_var;
    }
    cache.insert(root_var, ty_unfilled());
    let content = lower_content(cache, subs, subs.get_content_without_compacting(root_var));
    cache.insert(root_var, content);
    root_var
}

fn lower_content(cache: &mut MonoCache, subs: &Subs, content: &Content) -> Content {
    match content {
        Content::Structure(flat_type) => match flat_type {
            FlatType::Apply(symbol, args) => {
                let new_args = args
                    .into_iter()
                    .map(|var_index| monomorphize_var(cache, subs, subs[var_index]))
                    .collect::<Vec<_>>();
                Content::Structure(FlatType::Apply(
                    *symbol,
                    VariableSubsSlice::new(new_args.as_ptr() as u32, new_args.len() as u16),
                ))
            }
            FlatType::Func(args, closure, ret) => {
                let new_args = args
                    .into_iter()
                    .map(|var_index| monomorphize_var(cache, subs, subs[var_index]))
                    .collect::<Vec<_>>();
                let new_closure = monomorphize_var(cache, subs, *closure);
                let new_ret = monomorphize_var(cache, subs, *ret);
                Content::Structure(FlatType::Func(
                    VariableSubsSlice::new(new_args.as_ptr() as u32, new_args.len() as u16),
                    new_closure,
                    new_ret,
                ))
            }
            FlatType::Record(fields, ext) => {
                let (fields, ext) = chase_fields(subs, *fields, *ext);
                let new_fields = fields
                    .into_iter()
                    .map(|(field, var)| {
                        (
                            Lowercase::from(field),
                            RecordField::Required(monomorphize_var(cache, subs, var)),
                        )
                    })
                    .collect::<Vec<_>>();
                let new_ext = monomorphize_var(cache, subs, ext);
                Content::Structure(FlatType::Record(
                    RecordFields::insert_into_subs(&mut Subs::new(), new_fields.into_iter()),
                    new_ext,
                ))
            }
            FlatType::Tuple(elems, ext) => {
                let new_elems = elems
                    .iter_all()
                    .map(|(idx, var_index)| {
                        (
                            idx.index as usize,
                            monomorphize_var(cache, subs, subs[var_index]),
                        )
                    })
                    .collect::<Vec<_>>();
                let new_ext = monomorphize_var(cache, subs, *ext);
                Content::Structure(FlatType::Tuple(
                    TupleElems::insert_into_subs(&mut Subs::new(), new_elems.into_iter()),
                    new_ext,
                ))
            }
            FlatType::TagUnion(tags, ext) => {
                let (tags, ext) = chase_tags(subs, *tags, *ext);
                let new_tags = tags
                    .into_iter()
                    .map(|(tag, vars)| {
                        (
                            tag,
                            vars.into_iter()
                                .map(|var| monomorphize_var(cache, subs, var))
                                .collect::<Vec<Variable>>(),
                        )
                    })
                    .collect::<Vec<_>>();
                let new_ext = TagExt::Any(monomorphize_var(cache, subs, ext));
                Content::Structure(FlatType::TagUnion(
                    UnionTags::insert_into_subs(&mut Subs::new(), new_tags.into_iter()),
                    new_ext,
                ))
            }
            FlatType::FunctionOrTagUnion(tag_names, symbols, ext) => {
                let new_ext = TagExt::Any(monomorphize_var(cache, subs, ext.var()));
                Content::Structure(FlatType::FunctionOrTagUnion(*tag_names, *symbols, new_ext))
            }
            FlatType::RecursiveTagUnion(rec, tags, ext) => {
                let new_rec = monomorphize_var(cache, subs, *rec);
                let (tags, ext_var) = chase_tags(subs, *tags, *ext);
                let new_tags = tags
                    .into_iter()
                    .map(|(tag, vars)| {
                        (
                            tag,
                            vars.into_iter()
                                .map(|var| monomorphize_var(cache, subs, var))
                                .collect::<Vec<Variable>>(),
                        )
                    })
                    .collect::<Vec<_>>();
                let new_ext = TagExt::Any(monomorphize_var(cache, subs, ext_var));
                Content::Structure(FlatType::RecursiveTagUnion(
                    new_rec,
                    UnionTags::insert_into_subs(&mut Subs::new(), new_tags.into_iter()),
                    new_ext,
                ))
            }
            FlatType::EmptyRecord => Content::Structure(FlatType::EmptyRecord),
            FlatType::EmptyTuple => Content::Structure(FlatType::EmptyTuple),
            FlatType::EmptyTagUnion => Content::Structure(FlatType::EmptyTagUnion),
        },
        Content::FlexVar(opt_name) => Content::FlexVar(*opt_name),
        Content::RigidVar(name) => Content::RigidVar(*name),
        Content::FlexAbleVar(opt_name, abilities) => Content::FlexAbleVar(*opt_name, *abilities),
        Content::RigidAbleVar(name, abilities) => Content::RigidAbleVar(*name, *abilities),
        Content::RecursionVar {
            structure,
            opt_name,
        } => Content::RecursionVar {
            structure: monomorphize_var(cache, subs, *structure),
            opt_name: *opt_name,
        },
        Content::LambdaSet(lambda_set) => Content::LambdaSet(lambda_set.clone()),
        Content::ErasedLambda => Content::ErasedLambda,
        Content::Alias(symbol, args, real, kind) => {
            let new_real = monomorphize_var(cache, subs, *real);
            Content::Alias(*symbol, args.clone(), new_real, *kind)
        }
        Content::RangedNumber(range) => Content::RangedNumber(*range),
        Content::Error => Content::Error,
    }
}

fn chase_tags(
    subs: &Subs,
    mut tags: UnionTags,
    mut ext: TagExt,
) -> (Vec<(TagName, Vec<Variable>)>, Variable) {
    let mut all_tags = Vec::new();
    loop {
        for (tag, vars) in tags.iter_from_subs(subs) {
            all_tags.push((tag.clone(), vars.to_vec()));
        }
        match subs.get_content_without_compacting(ext.var()) {
            Content::Structure(FlatType::TagUnion(new_tags, new_ext)) => {
                tags = *new_tags;
                ext = *new_ext;
            }
            Content::Structure(FlatType::EmptyTagUnion) => break,
            Content::FlexVar(_) | Content::FlexAbleVar(_, _) => break,
            Content::Alias(_, _, real, _) => ext = TagExt::Any(*real),
            _ => panic!("Invalid tag union extension"),
        }
    }
    (all_tags, ext.var())
}

fn chase_fields(
    subs: &Subs,
    mut fields: RecordFields,
    mut ext: Variable,
) -> (Vec<(String, Variable)>, Variable) {
    let mut all_fields = Vec::new();
    loop {
        for (field, record_field) in fields.sorted_iterator(subs, ext) {
            let var = match record_field {
                RecordField::Required(v) | RecordField::Optional(v) | RecordField::Demanded(v) => v,
                RecordField::RigidRequired(v) | RecordField::RigidOptional(v) => v,
            };
            all_fields.push((field.to_string(), var));
        }
        match subs.get_content_without_compacting(ext) {
            Content::Structure(FlatType::Record(new_fields, new_ext)) => {
                fields = *new_fields;
                ext = *new_ext;
            }
            Content::Structure(FlatType::EmptyRecord) => break,
            Content::FlexVar(_) | Content::FlexAbleVar(_, _) => break,
            Content::Alias(_, _, real, _) => ext = *real,
            _ => panic!("Invalid record extension"),
        }
    }
    (all_fields, ext)
}
