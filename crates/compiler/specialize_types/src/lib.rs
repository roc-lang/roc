use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use roc_types::{
    subs::{Content, FlatType, RecordFields, Subs, TagExt, UnionTags, Variable},
    types::RecordField,
};

#[derive(Clone, Debug)]
pub enum TyContent {
    TFn(Rc<Ty>, Rc<Ty>),
    TTag(Vec<(String, Vec<Rc<Ty>>)>),
    TRecord(Vec<(String, Rc<Ty>)>),
    TPrim(Primitive),
}

#[derive(Clone, Debug)]
pub enum Primitive {
    Str,
    Int,
    Erased,
}

#[derive(Clone, Debug)]
pub struct Ty(RefCell<TyContent>);

type MonoCache = RefCell<HashMap<Variable, Rc<Ty>>>;

pub fn fresh_mono_cache() -> MonoCache {
    RefCell::new(HashMap::new())
}

fn unlink_tvar(subs: &Subs, mut var: Variable) -> Variable {
    loop {
        match subs.get_content_without_compacting(var) {
            Content::Alias(_, _, real, _) => var = *real,
            _ => break var,
        }
    }
}

fn ty_unfilled() -> TyContent {
    TyContent::TTag(vec![("__unfilled".to_string(), vec![])])
}

pub fn lower_type(cache: &MonoCache, subs: &Subs, var: Variable) -> Rc<Ty> {
    fn fail(s: &str, var: Variable) -> ! {
        panic!("lower_type: {}: {:?}", s, var)
    }

    fn go_content(cache: &MonoCache, subs: &Subs, content: &Content) -> TyContent {
        match content {
            Content::Structure(flat_type) => match flat_type {
                FlatType::Apply(_, _) => unimplemented!("FlatType::Apply"),
                FlatType::Func(args, closure, ret) => {
                    let in_ty = go(cache, subs, subs.variables[args.start as usize]);
                    let out_ty = go(cache, subs, *ret);
                    TyContent::TFn(in_ty, out_ty)
                }
                FlatType::Record(fields, ext) => {
                    let (fields, ext) = chase_fields(subs, *fields, *ext);
                    let fields = fields
                        .into_iter()
                        .map(|(field, var)| (field, go(cache, subs, var)))
                        .collect::<Vec<_>>();
                    assert!(
                        matches!(*go(cache, subs, ext).0.borrow(), TyContent::TTag(ref tags) if tags.is_empty())
                            || matches!(*go(cache, subs, ext).0.borrow(), TyContent::TRecord(ref fields) if fields.is_empty())
                    );
                    TyContent::TRecord(fields)
                }
                FlatType::Tuple(_, _) => unimplemented!("FlatType::Tuple"),
                FlatType::TagUnion(tags, ext) => {
                    let (tags, ext) = chase_tags(subs, *tags, *ext);
                    let tags = tags
                        .into_iter()
                        .map(|(tag, vars)| {
                            (
                                tag,
                                vars.into_iter().map(|var| go(cache, subs, var)).collect(),
                            )
                        })
                        .collect::<Vec<_>>();
                    assert!(
                        matches!(*go(cache, subs, ext).0.borrow(), TyContent::TTag(ref t) if t.is_empty())
                    );
                    TyContent::TTag(tags)
                }
                FlatType::FunctionOrTagUnion(_, _, _) => {
                    unimplemented!("FlatType::FunctionOrTagUnion")
                }
                FlatType::RecursiveTagUnion(_, _, _) => {
                    unimplemented!("FlatType::RecursiveTagUnion")
                }
                FlatType::EmptyRecord => TyContent::TRecord(vec![]),
                FlatType::EmptyTuple => unimplemented!("FlatType::EmptyTuple"),
                FlatType::EmptyTagUnion => TyContent::TTag(vec![]),
            },
            Content::FlexVar(_) => TyContent::TTag(vec![]),
            Content::RigidVar(_) => fail("unexpected rigid var", Variable::NULL),
            Content::FlexAbleVar(_, _) => TyContent::TTag(vec![]),
            Content::RigidAbleVar(_, _) => fail("unexpected rigid able var", Variable::NULL),
            Content::RecursionVar { .. } => fail("unexpected recursion var", Variable::NULL),
            Content::LambdaSet(_) => fail("unexpected lambda set", Variable::NULL),
            Content::ErasedLambda => TyContent::TPrim(Primitive::Erased),
            Content::Alias(_, _, _, _) => fail("unexpected alias", Variable::NULL),
            Content::RangedNumber(_) => unimplemented!("Content::RangedNumber"),
            Content::Error => fail("error type", Variable::NULL),
        }
    }

    fn go(cache: &MonoCache, subs: &Subs, var: Variable) -> Rc<Ty> {
        let var = unlink_tvar(subs, var);
        if let Some(ty) = cache.borrow().get(&var) {
            return ty.clone();
        }
        let ty = Rc::new(Ty(RefCell::new(ty_unfilled())));
        cache.borrow_mut().insert(var, ty.clone());
        let content = go_content(cache, subs, subs.get_content_without_compacting(var));
        *ty.0.borrow_mut() = content;
        ty
    }

    go(cache, subs, var)
}

fn chase_tags(
    subs: &Subs,
    mut tags: UnionTags,
    mut ext: TagExt,
) -> (Vec<(String, Vec<Variable>)>, Variable) {
    let mut all_tags = Vec::new();
    loop {
        for (tag, vars) in tags.iter_from_subs(subs) {
            all_tags.push((tag.0.to_string(), vars.to_vec()));
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
