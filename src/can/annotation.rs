use crate::can::env::Env;
use crate::can::ident::{Lowercase, ModuleName, TagName, Uppercase};
use crate::can::scope::Scope;
use crate::can::symbol::Symbol;
use crate::collections::{ImMap, SendMap};
use crate::parse::ast;
use crate::region::{Located, Region};
use crate::subs::{VarStore, Variable};
use crate::types::RecordFieldLabel;
use crate::types::{Problem, Type};

pub fn canonicalize_annotation<'a>(
    env: &Env,
    scope: &mut Scope,
    annotation: &'a crate::parse::ast::TypeAnnotation,
    var_store: &VarStore,
) -> (SendMap<Variable, Lowercase>, crate::types::Type) {
    // NOTE on rigids
    //
    // Rigids must be unique within a type annoation.
    // E.g. in `identity : a -> a`, there should only be one
    // variable (a rigid one, with name "a").
    // Hence `rigids : ImMap<Lowercase, Variable>`
    //
    // But then between annotations, the same name can occur multiple times,
    // but a variable can only have one name. Therefore
    // `ftv : SendMap<Variable, Lowercase>`.
    let mut rigids = ImMap::default();

    // TODO find annotation-local aliases first
    let mut aliases = scope.aliases.clone();

    let result = can_annotation_help(env, annotation, var_store, &mut rigids, &mut aliases);

    let tipe = to_type(&result, var_store, &aliases, &rigids);
    let mut ftv = SendMap::default();

    for (k, v) in rigids {
        if let Ok(var) = v {
            ftv.insert(var, k.as_str().into());
        }
    }

    (ftv, tipe)
}

fn to_type(
    type_annotation: &TypeAnnotation,
    var_store: &VarStore,
    aliases: &ImMap<Symbol, (Region, Vec<Located<Lowercase>>, TypeAnnotation)>,
    rigids: &ImMap<Symbol, Result<Variable, TypeAnnotation>>,
) -> Type {
    use TypeAnnotation::*;
    match type_annotation {
        Function(args, ret) => {
            let mut new_args = Vec::with_capacity(args.len());
            for arg in args {
                new_args.push(to_type(&arg.value, var_store, aliases, rigids));
            }

            Type::Function(
                new_args,
                Box::new(to_type(&ret.value, var_store, aliases, rigids)),
            )
        }
        Apply(symbol, args) => {
            dbg!(&aliases, symbol);
            if let Some((_region, type_variables, actual)) = aliases.get(&symbol) {
                if args.len() != type_variables.len() {
                    panic!("TODO incorrect number of arguments to type alias");
                }

                let mut zipped = Vec::with_capacity(args.len());
                let mut zipped_map = ImMap::default();

                for (var, arg) in type_variables.iter().zip(args.iter()) {
                    zipped.push((
                        var.value.clone(),
                        to_type(&arg.value, var_store, aliases, rigids),
                    ));
                    zipped_map.insert(
                        Symbol::new("Test", var.value.as_str()),
                        Err(arg.value.clone()),
                    );
                }

                let body = to_type(actual, var_store, aliases, &mut zipped_map);

                Type::Alias(
                    "Test".into(),
                    symbol.as_str().into(),
                    zipped,
                    Box::new(body),
                )
            } else {
                let mut new_args: Vec<Type> = Vec::with_capacity(args.len());

                for arg in args {
                    new_args.push(to_type(&arg.value, var_store, aliases, rigids));
                }

                let symbstr = symbol.as_str();

                if symbstr == "NumNum" || symbstr == "Num.Num" {
                    Type::Apply {
                        module_name: "Num".into(),
                        name: "Num".into(),
                        args: new_args,
                    }
                } else if symbstr == "IntInteger" || symbstr == "Int.Integer" {
                    Type::Apply {
                        module_name: "Int".into(),
                        name: "Integer".into(),
                        args: new_args,
                    }
                } else if symbstr == "StrStr" || symbstr == "Str.Str" {
                    Type::Apply {
                        module_name: "Str".into(),
                        name: "Str".into(),
                        args: new_args,
                    }
                } else {
                    Type::Apply {
                        module_name: "Test".into(),
                        name: symbol.as_str().into(),
                        args: new_args,
                    }
                }
            }
        }
        Rigid(symbol) => match rigids.get(symbol) {
            Some(Err(ta)) => to_type(&ta.clone(), var_store, aliases, rigids),
            Some(Ok(var)) => Type::Variable(*var),
            None => panic!("undefined rigid"),
        },
        As(_, _) => panic!("todo As"),
        Record { fields, ext } => {
            let ext_type = match ext {
                Some(loc_ann) => to_type(&loc_ann.value, var_store, aliases, rigids),
                None => Type::EmptyRec,
            };
            let mut fields_map = SendMap::default();

            for field in fields {
                match &field.value {
                    AssignedField::LabeledValue(loc_name, loc_ann) => {
                        fields_map.insert(
                            loc_name.value.clone(),
                            to_type(&loc_ann.value, var_store, aliases, rigids),
                        );
                    }
                    AssignedField::LabelOnly(loc_name) => {
                        fields_map.insert(
                            loc_name.value.clone(),
                            to_type(
                                &Rigid(Symbol::new("", loc_name.value.as_str())),
                                var_store,
                                aliases,
                                rigids,
                            ),
                        );
                    }
                }
            }
            Type::Record(fields_map, Box::new(ext_type))
        }
        TagUnion { tags, ext } => {
            let ext_type = match ext {
                Some(loc_ann) => to_type(&loc_ann.value, var_store, aliases, rigids),
                None => Type::EmptyTagUnion,
            };
            let mut tags_map = Vec::new();

            for tag in tags {
                match &tag.value {
                    Tag::Global { name, args } => {
                        let mut new_args = Vec::with_capacity(args.len());

                        for arg in args {
                            new_args.push(to_type(&arg.value, var_store, aliases, rigids));
                        }

                        let tag_name = TagName::Global(name.value.clone());

                        tags_map.push((tag_name, new_args));
                    }
                    Tag::Private { name, args } => {
                        let mut new_args = Vec::with_capacity(args.len());

                        for arg in args {
                            new_args.push(to_type(&arg.value, var_store, aliases, rigids));
                        }

                        let tag_name = TagName::Private(name.value.clone());

                        tags_map.push((tag_name, new_args));
                    }
                    Tag::Malformed(_) => panic!("TODO handle malformed tag"),
                }
            }
            Type::TagUnion(tags_map, Box::new(ext_type))
        }
        Wildcard => Type::Variable(var_store.fresh()),
        EmptyTagUnion => Type::EmptyTagUnion,
        EmptyRec => Type::EmptyRec,
        Malformed(_) => panic!("handle malformed annotation"),
    }
}

pub fn canonicalize_intermediate_annotation<'a>(
    env: &Env,
    scope: &mut Scope,
    annotation: &'a crate::parse::ast::TypeAnnotation,
    var_store: &VarStore,
) -> (SendMap<Variable, Lowercase>, TypeAnnotation) {
    // NOTE on rigids
    //
    // Rigids must be unique within a type annoation.
    // E.g. in `identity : a -> a`, there should only be one
    // variable (a rigid one, with name "a").
    // Hence `rigids : ImMap<Lowercase, Variable>`
    //
    // But then between annotations, the same name can occur multiple times,
    // but a variable can only have one name. Therefore
    // `ftv : SendMap<Variable, Lowercase>`.
    let mut rigids = ImMap::default();

    // TODO find annotation-local aliases first
    let mut aliases = scope.aliases.clone();

    let result = can_annotation_help(env, annotation, var_store, &mut rigids, &mut aliases);

    let mut ftv = SendMap::default();

    for (k, v) in rigids {
        if let Ok(var) = v {
            ftv.insert(var, k.as_str().into());
        }
    }

    (ftv, result)
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeAnnotation {
    /// A function. The types of its arguments, then the type of its return value.
    Function(Vec<Located<TypeAnnotation>>, Box<Located<TypeAnnotation>>),
    /// Applying a type to some arguments (e.g. Map.Map String Int)
    Apply(Symbol, Vec<Located<TypeAnnotation>>),
    /// A bound type variable, e.g. `a` in `(a -> a)`
    Rigid(Symbol),
    /// Inline type alias, e.g. `as List a` in `[ Cons a (List a), Nil ] as List a`
    As(Box<Located<TypeAnnotation>>, Box<Located<TypeAnnotation>>),
    Record {
        fields: Vec<Located<AssignedField<TypeAnnotation>>>,
        /// The row type variable in an open record, e.g. the `r` in `{ name: Str }r`.
        /// This is None if it's a closed record annotation like `{ name: Str }`.
        ext: Option<Located<Box<TypeAnnotation>>>,
    },
    /// A tag union, e.g. `[
    TagUnion {
        tags: Vec<Located<Tag>>,
        /// The row type variable in an open tag union, e.g. the `a` in `[ Foo, Bar ]a`.
        /// This is None if it's a closed tag union like `[ Foo, Bar]`.
        ext: Option<Located<Box<TypeAnnotation>>>,
    },
    EmptyTagUnion,
    EmptyRec,
    /// The `*` type variable, e.g. in (List *)
    Wildcard,
    /// A malformed type annotation, which will code gen to a runtime error
    Malformed(Box<str>),
}
#[derive(Debug, Clone, PartialEq)]
pub enum AssignedField<Val> {
    // Both a label and a value, e.g. `{ name: "blah" }`
    LabeledValue(Located<Lowercase>, Located<Val>),
    // A label with no value, e.g. `{ name }` (this is sugar for { name: name })
    LabelOnly(Located<Lowercase>),
}
#[derive(Debug, Clone, PartialEq)]
pub enum Tag {
    Global {
        name: Located<Uppercase>,
        args: Vec<Located<TypeAnnotation>>,
    },
    Private {
        name: Located<Symbol>,
        args: Vec<Located<TypeAnnotation>>,
    },
    /// A malformed tag, which will code gen to a runtime error
    Malformed(Box<str>),
}

fn can_annotation_help(
    env: &Env,
    annotation: &crate::parse::ast::TypeAnnotation,
    var_store: &VarStore,
    rigids: &mut ImMap<Symbol, Result<Variable, TypeAnnotation>>,
    aliases: &mut ImMap<Symbol, (Region, Vec<Located<Lowercase>>, TypeAnnotation)>,
) -> TypeAnnotation {
    use crate::parse::ast::TypeAnnotation::*;

    match annotation {
        Function(argument_types, return_type) => {
            let mut args = Vec::new();

            for arg in *argument_types {
                args.push(Located::at(
                    Region::zero(),
                    can_annotation_help(env, &arg.value, var_store, rigids, aliases),
                ));
            }

            let ret = can_annotation_help(env, &return_type.value, var_store, rigids, aliases);
            TypeAnnotation::Function(args, Box::new(Located::at(Region::zero(), ret)))
        }
        Apply(module_name, name, type_arguments) => {
            // TODO use this to subsitute aliases
            //            if let Some((type_variables, actual)) = aliases.get(name) {
            //                let mut zipped_args: Vec<(Lowercase, Type)> =
            //                    Vec::with_capacity(type_variables.len());
            //
            //                for (tvar, arg) in type_variables.iter().zip(args.iter()) {
            //                    zipped_args.push((tvar.clone(), arg.clone()));
            //                }
            //
            //                let inner_rigids: ImMap<Lowercase, Type> =
            //                    zipped_args.clone().into_iter().collect();
            //
            //                let mut instantiated =
            //                    can_annotation_help(env, actual, var_store, &inner_rigids, &mut Vec::new());
            //
            //                dbg!(&ftv, &inner_rigids, &instantiated);
            //
            //                *tipe = Alias(
            //                    // module_name.clone(),
            //                    "Test".into(),
            //                    name.clone(),
            //                    zipped_args,
            //                    Box::new(instantiated),
            //                );
            //            } else {
            //                for arg in args {
            //                    substitute_alias(arg, aliases, ftv, rigids);
            //                }
            //            }
            let mut args = Vec::new();

            for arg in *type_arguments {
                args.push(Located::at(
                    Region::zero(),
                    can_annotation_help(env, &arg.value, var_store, rigids, aliases),
                ));
            }

            // let temp = format!("{}.", module_name.join("."));

            let symbol = Symbol::from_parts(module_name, name);

            TypeAnnotation::Apply(symbol, args)
        }
        BoundVariable(v) => {
            if v == &"*" {
                TypeAnnotation::Wildcard
            } else {
                let name = Symbol::from(*v);

                if !rigids.contains_key(&name) {
                    rigids.insert(name.clone(), Ok(var_store.fresh()));
                }

                TypeAnnotation::Rigid(name)
            }

            /*
            if let Some(var) = rigids.get(&name) {
                TypeAnnotation::Variable(*var)
            } else {
                let var = var_store.fresh();
                rigids.insert(name, var);
                TypeAnnotation::Variable(var)
            }
            */
        }
        As(loc_inner, _spaces, loc_as) => {
            // add this alias to the list of aliases
            // aliases: &mut ImMap<Symbol, (Region, Vec<Located<Lowercase>>, TypeAnnotation)>,

            match loc_as.value {
                Apply(module_name, name, loc_vars) if module_name.is_empty() => {
                    let inner_type =
                        can_annotation_help(env, &loc_inner.value, var_store, rigids, aliases);
                    let old_name = name.clone();
                    let name = Uppercase::from(name);
                    let mut loc_type_vars: Vec<Located<Lowercase>> =
                        Vec::with_capacity(loc_vars.len());

                    for loc_var in loc_vars {
                        match loc_var.value {
                            BoundVariable(ident) => {
                                let var_name = Lowercase::from(ident);
                                let tvar = Located::at(loc_var.region, var_name);
                                loc_type_vars.push(tvar);
                            }
                            _ => {
                                // If anything other than a lowercase identifier
                                // appears here, the whole annotation is invalid.
                                // return TypeAnnotation::Erroneous(Problem::CanonicalizationProblem);
                                panic!("problem");
                            }
                        }
                    }

                    let symbol = Symbol::new(env.home.clone().as_str(), name.as_str());

                    aliases.insert(symbol, (Region::zero(), loc_type_vars, inner_type));

                    let fake_apply = Apply(&["Test"], old_name, loc_vars.clone());
                    can_annotation_help(env, &fake_apply, var_store, rigids, aliases)
                }
                _ => {
                    // This is a syntactically invalid type alias.
                    //TypeAnnotation::Erroneous(Problem::CanonicalizationProblem)
                    panic!("problem");
                }
            }
        }
        Record { fields, ext } => {
            let mut field_types = Vec::with_capacity(fields.len());

            for field in fields.iter() {
                field_types.push(Located::at(
                    field.region,
                    can_assigned_field(env, &field.value, var_store, rigids, aliases),
                ));
            }

            let ext_type = match ext {
                Some(loc_ann) => Some(Located::at(
                    Region::zero(),
                    Box::new(can_annotation_help(
                        env,
                        &loc_ann.value,
                        var_store,
                        rigids,
                        aliases,
                    )),
                )),
                None => None,
            };

            TypeAnnotation::Record {
                fields: field_types,
                ext: ext_type,
            }
        }
        TagUnion { tags, ext } => {
            let mut tag_types = Vec::with_capacity(tags.len());

            for tag in tags.iter() {
                can_tag(env, &tag.value, var_store, rigids, aliases, &mut tag_types);
            }

            let ext_type = match ext {
                Some(loc_ann) => Some(Located::at(
                    Region::zero(),
                    Box::new(can_annotation_help(
                        env,
                        &loc_ann.value,
                        var_store,
                        rigids,
                        aliases,
                    )),
                )),
                None => None,
            };

            TypeAnnotation::TagUnion {
                tags: tag_types,
                ext: ext_type,
            }
        }
        SpaceBefore(nested, _) | SpaceAfter(nested, _) => {
            can_annotation_help(env, nested, var_store, rigids, aliases)
        }
        Wildcard => TypeAnnotation::Wildcard,
        Malformed(_) => {
            //            let var = var_store.fresh();
            //            TypeAnnotation::Variable(var)
            panic!("figure this out")
        }
    }
}

fn can_assigned_field<'a>(
    env: &Env,
    field: &ast::AssignedField<'a, ast::TypeAnnotation<'a>>,
    var_store: &VarStore,
    rigids: &mut ImMap<Symbol, Result<Variable, TypeAnnotation>>,
    aliases: &mut ImMap<Symbol, (Region, Vec<Located<Lowercase>>, TypeAnnotation)>,
) -> AssignedField<TypeAnnotation> {
    use crate::parse::ast::AssignedField::*;

    match field {
        LabeledValue(field_name, _, annotation) => {
            let field_type =
                can_annotation_help(env, &annotation.value, var_store, rigids, aliases);
            let label = Lowercase::from(field_name.value);
            AssignedField::LabeledValue(
                Located::at(field_name.region, label),
                Located::at(annotation.region, field_type),
            )
        }
        LabelOnly(loc_field_name) => {
            // Interpret { a, b } as { a : a, b : b }
            let field_name = RecordFieldLabel::from(loc_field_name.value);
            let field_symbol = Symbol::from(loc_field_name.value);
            let field_type = TypeAnnotation::Rigid(field_symbol);
            /*
            let field_type = {
                if let Some(var) = rigids.get(&field_name) {
                    TypeAnnotation::Rigid(*var)
                } else {
                    let field_var = var_store.fresh();
                    rigids.insert(field_name.clone(), field_var);
                    Annotation::Rigid(field_var)
                }
            };
            */
            AssignedField::LabelOnly(Located::at(loc_field_name.region, field_name))
        }
        SpaceBefore(nested, _) | SpaceAfter(nested, _) => {
            can_assigned_field(env, nested, var_store, rigids, aliases)
        }
        Malformed(_) => panic!("TODO handle malformed record field"),
    }
}

fn can_tag<'a>(
    env: &Env,
    tag: &ast::Tag<'a>,
    var_store: &VarStore,
    rigids: &mut ImMap<Symbol, Result<Variable, TypeAnnotation>>,
    aliases: &mut ImMap<Symbol, (Region, Vec<Located<Lowercase>>, TypeAnnotation)>,
    tag_types: &mut Vec<Located<Tag>>,
) {
    match tag {
        ast::Tag::Global { name, args } => {
            let name: Uppercase = name.value.into();

            let arg_types = args
                .iter()
                .map(|arg| {
                    Located::at(
                        Region::zero(),
                        can_annotation_help(env, &arg.value, var_store, rigids, aliases),
                    )
                })
                .collect();

            tag_types.push(Located::at(
                Region::zero(),
                Tag::Global {
                    name: Located::at(Region::zero(), name),
                    args: arg_types,
                },
            ));
        }
        ast::Tag::Private { name, args } => {
            let symbol = Symbol::from_private_tag(env.home.as_str(), name.value);

            let arg_types = args
                .iter()
                .map(|arg| {
                    Located::at(
                        Region::zero(),
                        can_annotation_help(env, &arg.value, var_store, rigids, aliases),
                    )
                })
                .collect();

            tag_types.push(Located::at(
                Region::zero(),
                Tag::Private {
                    name: Located::at(Region::zero(), symbol),
                    args: arg_types,
                },
            ));
        }
        ast::Tag::SpaceBefore(nested, _) | ast::Tag::SpaceAfter(nested, _) => {
            can_tag(env, nested, var_store, rigids, aliases, tag_types)
        }
        ast::Tag::Malformed(_) => {}
    }
}
