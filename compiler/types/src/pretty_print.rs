use crate::subs::{Content, FlatType, GetSubsSlice, Subs, UnionTags, Variable};
use crate::types::{name_type_var, RecordField};
use roc_collections::all::{MutMap, MutSet};
use roc_module::ident::{Lowercase, TagName};
use roc_module::symbol::{Interns, ModuleId, Symbol};

pub static WILDCARD: &str = "*";
static EMPTY_RECORD: &str = "{}";
static EMPTY_TAG_UNION: &str = "[]";

/// Rerquirements for parentheses.
///
/// If we're inside a function (that is, this is either an argument or a return
/// value), we may need to use parens. Examples:
///
/// a -> (* -> a)
/// (* -> a) -> a
///
/// Separately, if we're inside a type parameter, we may need to use parens:
///
/// List I64
/// List (List I64)
///
/// Otherwise, parens are unnecessary.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Parens {
    InFn,
    InTypeParam,
    Unnecessary,
}

macro_rules! write_parens {
    ($insert_parens:expr, $buf:expr, $body:expr) => {{
        if $insert_parens {
            $buf.push('(');
        }

        $body

        if $insert_parens {
            $buf.push(')');
        }
    }
    };
}

struct Env<'a> {
    home: ModuleId,
    interns: &'a Interns,
}

/// How many times a root variable appeared in Subs.
///
/// We only care about whether it was a single time or multiple times,
/// because single appearances get a wildcard (*) and multiple times
/// get a generated letter ("a" etc).
enum Appearances {
    Single,
    Multiple,
}

/// Generate names for all type variables, replacing FlexVar(None) with
/// FlexVar(Some(name)) where appropriate. Example: for the identity
/// function, generate a name of "a" for both its argument and return
/// type variables.
///
/// We also want to count how many times a root appears, because we should
/// only generate a name for it if it appears more than once.
fn find_names_needed(
    variable: Variable,
    subs: &mut Subs,
    roots: &mut Vec<Variable>,
    root_appearances: &mut MutMap<Variable, Appearances>,
    names_taken: &mut MutSet<Lowercase>,
) {
    use crate::subs::Content::*;
    use crate::subs::FlatType::*;

    while let Err((recursive, _chain)) = subs.occurs(variable) {
        let rec_var = subs.fresh_unnamed_flex_var();
        let content = subs.get_content_without_compacting(recursive);

        match content {
            Content::Structure(FlatType::TagUnion(tags, ext_var)) => {
                let ext_var = *ext_var;

                let mut new_tags = MutMap::default();

                for (name_index, slice_index) in tags.iter_all() {
                    let slice = subs[slice_index];

                    let mut new_vars = Vec::new();
                    for var_index in slice {
                        let var = subs[var_index];
                        new_vars.push(if var == recursive { rec_var } else { var });
                    }

                    new_tags.insert(subs[name_index].clone(), new_vars);
                }

                let mut x: Vec<_> = new_tags.into_iter().collect();
                x.sort();

                let union_tags = UnionTags::insert_into_subs(subs, x);

                let flat_type = FlatType::RecursiveTagUnion(rec_var, union_tags, ext_var);
                subs.set_content(recursive, Content::Structure(flat_type));
            }
            _ => panic!(
                "unfixable recursive type in roc_types::pretty_print {:?} {:?} {:?}",
                recursive, variable, content
            ),
        }
    }

    match &subs.get_content_without_compacting(variable).clone() {
        RecursionVar { opt_name: None, .. } | FlexVar(None) => {
            let root = subs.get_root_key_without_compacting(variable);

            // If this var is *not* its own root, then the
            // root var necessarily appears in multiple places.
            // We need a name for it!
            match root_appearances.get(&root) {
                Some(Appearances::Single) => {
                    root_appearances.insert(root, Appearances::Multiple);
                }
                Some(Appearances::Multiple) => {
                    // It's already multiple, so do nothing!
                }
                None => {
                    roots.push(root);
                    root_appearances.insert(root, Appearances::Single);
                }
            }
        }
        RecursionVar {
            opt_name: Some(name),
            ..
        }
        | FlexVar(Some(name)) => {
            // This root already has a name. Nothing more to do here!

            // User-defined names are already taken.
            // We must not accidentally generate names that collide with them!
            names_taken.insert(name.clone());
        }
        RigidVar(name) => {
            // User-defined names are already taken.
            // We must not accidentally generate names that collide with them!
            names_taken.insert(name.clone());
        }
        Structure(Apply(_, args)) => {
            for index in args.into_iter() {
                let var = subs[index];
                find_names_needed(var, subs, roots, root_appearances, names_taken);
            }
        }
        Structure(Func(arg_vars, _closure_var, ret_var)) => {
            for index in arg_vars.into_iter() {
                let var = subs[index];
                find_names_needed(var, subs, roots, root_appearances, names_taken);
            }

            find_names_needed(*ret_var, subs, roots, root_appearances, names_taken);
        }
        Structure(Record(sorted_fields, ext_var)) => {
            for index in sorted_fields.iter_variables() {
                let var = subs[index];
                find_names_needed(var, subs, roots, root_appearances, names_taken);
            }

            find_names_needed(*ext_var, subs, roots, root_appearances, names_taken);
        }
        Structure(TagUnion(tags, ext_var)) => {
            for slice_index in tags.variables() {
                let slice = subs[slice_index];
                for var_index in slice {
                    let var = subs[var_index];
                    find_names_needed(var, subs, roots, root_appearances, names_taken);
                }
            }

            find_names_needed(*ext_var, subs, roots, root_appearances, names_taken);
        }
        Structure(FunctionOrTagUnion(_, _, ext_var)) => {
            find_names_needed(*ext_var, subs, roots, root_appearances, names_taken);
        }
        Structure(RecursiveTagUnion(rec_var, tags, ext_var)) => {
            for slice_index in tags.variables() {
                let slice = subs[slice_index];
                for var_index in slice {
                    let var = subs[var_index];
                    find_names_needed(var, subs, roots, root_appearances, names_taken);
                }
            }

            find_names_needed(*ext_var, subs, roots, root_appearances, names_taken);
            find_names_needed(*rec_var, subs, roots, root_appearances, names_taken);
        }
        Alias(_symbol, args, _actual) => {
            // only find names for named parameters!
            for var_index in args.into_iter().take(args.len()) {
                let var = subs[var_index];
                find_names_needed(var, subs, roots, root_appearances, names_taken);
            }
            // TODO should we also look in the actual variable?
            // find_names_needed(_actual, subs, roots, root_appearances, names_taken);
        }
        Error | Structure(Erroneous(_)) | Structure(EmptyRecord) | Structure(EmptyTagUnion) => {
            // Errors and empty records don't need names.
        }
    }
}

pub fn name_all_type_vars(variable: Variable, subs: &mut Subs) {
    let mut roots = Vec::new();
    let mut letters_used = 0;
    let mut appearances = MutMap::default();
    let mut taken = MutSet::default();

    // Populate names_needed
    find_names_needed(variable, subs, &mut roots, &mut appearances, &mut taken);

    for root in roots {
        // show the type variable number instead of `*`. useful for debugging
        // set_root_name(root, (format!("<{:?}>", root).into()), subs);
        if let Some(Appearances::Multiple) = appearances.get(&root) {
            letters_used = name_root(letters_used, root, subs, &mut taken);
        }
    }
}

fn name_root(
    letters_used: u32,
    root: Variable,
    subs: &mut Subs,
    taken: &mut MutSet<Lowercase>,
) -> u32 {
    let (generated_name, new_letters_used) = name_type_var(letters_used, taken);

    set_root_name(root, generated_name, subs);

    new_letters_used
}

fn set_root_name(root: Variable, name: Lowercase, subs: &mut Subs) {
    use crate::subs::Content::*;

    let old_content = subs.get_content_without_compacting(root);

    match old_content {
        FlexVar(None) => {
            let content = FlexVar(Some(name));
            subs.set_content(root, content);
        }
        RecursionVar {
            opt_name: None,
            structure,
        } => {
            let content = RecursionVar {
                structure: *structure,
                opt_name: Some(name),
            };
            subs.set_content(root, content);
        }
        RecursionVar {
            opt_name: Some(_existing),
            ..
        }
        | FlexVar(Some(_existing)) => {
            panic!("TODO FIXME - make sure the generated name does not clash with any bound vars! In other words, if the user decided to name a type variable 'a', make sure we don't generate 'a' to name a different one!");
        }

        _ => (),
    }
}

pub fn content_to_string(
    content: &Content,
    subs: &Subs,
    home: ModuleId,
    interns: &Interns,
) -> String {
    let mut buf = String::new();
    let env = Env { home, interns };

    write_content(&env, content, subs, &mut buf, Parens::Unnecessary);

    buf
}

fn write_content(env: &Env, content: &Content, subs: &Subs, buf: &mut String, parens: Parens) {
    use crate::subs::Content::*;

    match content {
        FlexVar(Some(name)) => buf.push_str(name.as_str()),
        FlexVar(None) => buf.push_str(WILDCARD),
        RigidVar(name) => buf.push_str(name.as_str()),
        RecursionVar { opt_name, .. } => match opt_name {
            Some(name) => buf.push_str(name.as_str()),
            None => buf.push_str(WILDCARD),
        },
        Structure(flat_type) => write_flat_type(env, flat_type, subs, buf, parens),
        Alias(symbol, args, _actual) => {
            let write_parens = parens == Parens::InTypeParam && !args.is_empty();

            match *symbol {
                Symbol::NUM_NUM => {
                    debug_assert_eq!(args.len(), 1);

                    let arg_var_index = args
                        .into_iter()
                        .next()
                        .expect("Num was not applied to a type argument!");
                    let arg_var = subs[arg_var_index];
                    let content = subs.get_content_without_compacting(arg_var);

                    match &content {
                        Alias(nested, _, _) => match *nested {
                            Symbol::NUM_INTEGER => buf.push_str("I64"),
                            Symbol::NUM_FLOATINGPOINT => buf.push_str("F64"),

                            _ => write_parens!(write_parens, buf, {
                                buf.push_str("Num ");
                                write_content(env, content, subs, buf, parens);
                            }),
                        },

                        _ => write_parens!(write_parens, buf, {
                            buf.push_str("Num ");
                            write_content(env, content, subs, buf, parens);
                        }),
                    }
                }

                _ => write_parens!(write_parens, buf, {
                    write_symbol(env, *symbol, buf);

                    for var_index in args.into_iter() {
                        let var = subs[var_index];
                        buf.push(' ');
                        write_content(
                            env,
                            subs.get_content_without_compacting(var),
                            subs,
                            buf,
                            Parens::InTypeParam,
                        );
                    }

                    // useful for debugging
                    if false {
                        buf.push_str("[[ but really ");
                        let content = subs.get_content_without_compacting(*_actual);
                        write_content(env, content, subs, buf, parens);
                        buf.push_str("]]");
                    }
                }),
            }
        }
        Error => buf.push_str("<type mismatch>"),
    }
}

enum ExtContent<'a> {
    Empty,
    Content(Variable, &'a Content),
}

impl<'a> ExtContent<'a> {
    fn from_var(subs: &'a Subs, ext: Variable) -> Self {
        let content = subs.get_content_without_compacting(ext);
        match content {
            Content::Structure(FlatType::EmptyTagUnion) => ExtContent::Empty,
            Content::Structure(FlatType::EmptyRecord) => ExtContent::Empty,

            Content::FlexVar(_) | Content::RigidVar(_) => ExtContent::Content(ext, content),

            other => unreachable!("something weird ended up in an ext var: {:?}", other),
        }
    }
}

fn write_ext_content<'a>(
    env: &Env,
    subs: &'a Subs,
    buf: &mut String,
    ext_content: ExtContent<'a>,
    parens: Parens,
) {
    if let ExtContent::Content(_, content) = ext_content {
        // This is an open record or tag union, so print the variable
        // right after the '}' or ']'
        //
        // e.g. the "*" at the end of `{ x: I64 }*`
        // or the "r" at the end of `{ x: I64 }r`
        write_content(env, content, subs, buf, parens)
    }
}

fn write_sorted_tags2<'a>(
    env: &Env,
    subs: &'a Subs,
    buf: &mut String,
    tags: &UnionTags,
    ext_var: Variable,
) -> ExtContent<'a> {
    // Sort the fields so they always end up in the same order.
    let (it, new_ext_var) = tags.unsorted_iterator_and_ext(subs, ext_var);
    let mut sorted_fields: Vec<_> = it.collect();

    let interns = &env.interns;
    let home = env.home;

    sorted_fields.sort_by(|(a, _), (b, _)| {
        a.as_ident_str(interns, home)
            .cmp(&b.as_ident_str(interns, home))
    });

    let mut any_written_yet = false;

    for (label, vars) in sorted_fields {
        if any_written_yet {
            buf.push_str(", ");
        } else {
            any_written_yet = true;
        }

        buf.push_str(label.as_ident_str(interns, home).as_str());

        for var in vars {
            buf.push(' ');
            write_content(
                env,
                subs.get_content_without_compacting(*var),
                subs,
                buf,
                Parens::InTypeParam,
            );
        }
    }

    ExtContent::from_var(subs, new_ext_var)
}

fn write_sorted_tags<'a>(
    env: &Env,
    subs: &'a Subs,
    buf: &mut String,
    tags: &MutMap<TagName, Vec<Variable>>,
    ext_var: Variable,
) -> ExtContent<'a> {
    // Sort the fields so they always end up in the same order.
    let mut sorted_fields = Vec::with_capacity(tags.len());

    for (label, vars) in tags {
        sorted_fields.push((label, vars));
    }

    // If the `ext` contains tags, merge them into the list of tags.
    // this can occur when inferring mutually recursive tags
    let mut from_ext = Default::default();
    let _ext_content = chase_ext_tag_union(subs, ext_var, &mut from_ext);

    for (tag_name, arguments) in from_ext.iter() {
        sorted_fields.push((tag_name, arguments));
    }

    let interns = &env.interns;
    let home = env.home;

    sorted_fields.sort_by(|(a, _), (b, _)| {
        a.as_ident_str(interns, home)
            .cmp(&b.as_ident_str(interns, home))
    });

    let mut any_written_yet = false;

    for (label, vars) in sorted_fields {
        if any_written_yet {
            buf.push_str(", ");
        } else {
            any_written_yet = true;
        }

        buf.push_str(label.as_ident_str(interns, home).as_str());

        for var in vars {
            buf.push(' ');
            write_content(
                env,
                subs.get_content_without_compacting(*var),
                subs,
                buf,
                Parens::InTypeParam,
            );
        }
    }

    ExtContent::from_var(subs, ext_var)
}

fn write_flat_type(env: &Env, flat_type: &FlatType, subs: &Subs, buf: &mut String, parens: Parens) {
    use crate::subs::FlatType::*;

    match flat_type {
        Apply(symbol, args) => {
            write_apply(env, *symbol, subs.get_subs_slice(*args), subs, buf, parens)
        }
        EmptyRecord => buf.push_str(EMPTY_RECORD),
        EmptyTagUnion => buf.push_str(EMPTY_TAG_UNION),
        Func(args, _closure, ret) => {
            write_fn(env, subs.get_subs_slice(*args), *ret, subs, buf, parens)
        }
        Record(fields, ext_var) => {
            use crate::types::{gather_fields, RecordStructure};

            // If the `ext` has concrete fields (e.g. { foo : I64}{ bar : Bool }), merge them
            let RecordStructure {
                fields: sorted_fields,
                ext,
            } = gather_fields(subs, *fields, *ext_var)
                .expect("Something ended up weird in this record type");
            let ext_var = ext;

            if fields.is_empty() {
                buf.push_str(EMPTY_RECORD)
            } else {
                buf.push_str("{ ");

                let mut any_written_yet = false;

                for (label, record_field) in sorted_fields {
                    use RecordField::*;

                    let var = *record_field.as_inner();

                    if any_written_yet {
                        buf.push_str(", ");
                    } else {
                        any_written_yet = true;
                    }
                    buf.push_str(label.as_str());

                    match record_field {
                        Optional(_) => buf.push_str(" ? "),
                        Required(_) => buf.push_str(" : "),
                        Demanded(_) => buf.push_str(" : "),
                    };

                    write_content(
                        env,
                        subs.get_content_without_compacting(var),
                        subs,
                        buf,
                        Parens::Unnecessary,
                    );
                }

                buf.push_str(" }");
            }

            match subs.get_content_without_compacting(ext_var) {
                Content::Structure(EmptyRecord) => {
                    // This is a closed record. We're done!
                }
                content => {
                    // This is an open record, so print the variable
                    // right after the '}'
                    //
                    // e.g. the "*" at the end of `{ x: I64 }*`
                    // or the "r" at the end of `{ x: I64 }r`
                    write_content(env, content, subs, buf, parens)
                }
            }
        }
        TagUnion(tags, ext_var) => {
            buf.push_str("[ ");

            let ext_content = write_sorted_tags2(env, subs, buf, tags, *ext_var);

            buf.push_str(" ]");

            write_ext_content(env, subs, buf, ext_content, parens)
        }

        FunctionOrTagUnion(tag_name, _, ext_var) => {
            buf.push_str("[ ");

            let mut tags: MutMap<TagName, _> = MutMap::default();
            tags.insert(subs[*tag_name].clone(), vec![]);
            let ext_content = write_sorted_tags(env, subs, buf, &tags, *ext_var);

            buf.push_str(" ]");

            write_ext_content(env, subs, buf, ext_content, parens)
        }

        RecursiveTagUnion(rec_var, tags, ext_var) => {
            buf.push_str("[ ");

            let ext_content = write_sorted_tags2(env, subs, buf, tags, *ext_var);

            buf.push_str(" ]");

            write_ext_content(env, subs, buf, ext_content, parens);

            buf.push_str(" as ");
            write_content(
                env,
                subs.get_content_without_compacting(*rec_var),
                subs,
                buf,
                parens,
            )
        }
        Erroneous(problem) => {
            buf.push_str(&format!("<Type Mismatch: {:?}>", problem));
        }
    }
}

pub fn chase_ext_tag_union<'a>(
    subs: &'a Subs,
    var: Variable,
    fields: &mut Vec<(TagName, Vec<Variable>)>,
) -> Result<(), (Variable, &'a Content)> {
    use FlatType::*;
    match subs.get_content_without_compacting(var) {
        Content::Structure(EmptyTagUnion) => Ok(()),
        Content::Structure(TagUnion(tags, ext_var)) => {
            for (name_index, slice_index) in tags.iter_all() {
                let subs_slice = subs[slice_index];
                let slice = subs.get_subs_slice(subs_slice);
                let tag_name = subs[name_index].clone();

                fields.push((tag_name, slice.to_vec()));
            }

            chase_ext_tag_union(subs, *ext_var, fields)
        }

        Content::Structure(RecursiveTagUnion(_, tags, ext_var)) => {
            for (name_index, slice_index) in tags.iter_all() {
                let subs_slice = subs[slice_index];
                let slice = subs.get_subs_slice(subs_slice);
                let tag_name = subs[name_index].clone();

                fields.push((tag_name, slice.to_vec()));
            }

            chase_ext_tag_union(subs, *ext_var, fields)
        }
        Content::Structure(FunctionOrTagUnion(tag_name, _, ext_var)) => {
            fields.push((subs[*tag_name].clone(), vec![]));

            chase_ext_tag_union(subs, *ext_var, fields)
        }

        Content::Alias(_, _, var) => chase_ext_tag_union(subs, *var, fields),

        content => Err((var, content)),
    }
}

fn write_apply(
    env: &Env,
    symbol: Symbol,
    args: &[Variable],
    subs: &Subs,
    buf: &mut String,
    parens: Parens,
) {
    let write_parens = parens == Parens::InTypeParam && !args.is_empty();

    // Hardcoded type aliases
    match symbol {
        Symbol::STR_STR => {
            buf.push_str("Str");
        }
        Symbol::NUM_NUM => {
            let arg = args
                .iter()
                .next()
                .unwrap_or_else(|| panic!("Num did not have any type parameters somehow."));
            let arg_content = subs.get_content_without_compacting(*arg);
            let mut arg_param = String::new();

            let mut default_case = |subs, content| {
                if write_parens {
                    buf.push('(');
                }

                write_content(env, content, subs, &mut arg_param, Parens::InTypeParam);
                buf.push_str("Num ");
                buf.push_str(&arg_param);

                if write_parens {
                    buf.push(')');
                }
            };

            match &arg_content {
                Content::Structure(FlatType::Apply(symbol, nested_args)) => match *symbol {
                    Symbol::NUM_INTEGER if nested_args.len() == 1 => {
                        buf.push_str("I64");
                    }
                    Symbol::NUM_FLOATINGPOINT if nested_args.len() == 1 => {
                        buf.push_str("F64");
                    }
                    _ => default_case(subs, arg_content),
                },
                _ => default_case(subs, arg_content),
            }
        }
        _ => {
            if write_parens {
                buf.push('(');
            }

            write_symbol(env, symbol, buf);

            for arg in args {
                buf.push(' ');
                write_content(
                    env,
                    subs.get_content_without_compacting(*arg),
                    subs,
                    buf,
                    Parens::InTypeParam,
                );
            }

            if write_parens {
                buf.push(')');
            }
        }
    }
}

fn write_fn(
    env: &Env,
    args: &[Variable],
    ret: Variable,
    subs: &Subs,
    buf: &mut String,
    parens: Parens,
) {
    let mut needs_comma = false;
    let use_parens = parens != Parens::Unnecessary;

    if use_parens {
        buf.push('(');
    }

    for arg in args {
        if needs_comma {
            buf.push_str(", ");
        } else {
            needs_comma = true;
        }

        write_content(
            env,
            subs.get_content_without_compacting(*arg),
            subs,
            buf,
            Parens::InFn,
        );
    }

    buf.push_str(" -> ");
    write_content(
        env,
        subs.get_content_without_compacting(ret),
        subs,
        buf,
        Parens::InFn,
    );

    if use_parens {
        buf.push(')');
    }
}

fn write_symbol(env: &Env, symbol: Symbol, buf: &mut String) {
    let interns = &env.interns;
    let ident = symbol.ident_str(interns);
    let module_id = symbol.module_id();

    // Don't qualify the symbol if it's in our home module,
    // or if it's a builtin (since all their types are always in scope)
    if module_id != env.home && !module_id.is_builtin() {
        buf.push_str(module_id.to_ident_str(interns).as_str());
        buf.push('.');
    }

    buf.push_str(ident.as_str());
}
