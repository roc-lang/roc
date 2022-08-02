//! Synthesizes type variables.

use crate::subs::{Content, Descriptor, Mark, OptVariable, Rank, Subs, Variable};

pub fn synth_var(subs: &mut Subs, content: Content) -> Variable {
    let descriptor = Descriptor {
        content,
        rank: Rank::toplevel(),
        mark: Mark::NONE,
        copy: OptVariable::NONE,
    };
    subs.fresh(descriptor)
}

/// DSL for creating [`Content`][crate::subs::Content].
#[macro_export]
macro_rules! v {
    ({ $($field:ident: $make_v:expr,)* $(?$opt_field:ident : $make_opt_v:expr,)* }) => {{
        use $crate::types::RecordField;
        use $crate::subs::{Subs, RecordFields, Content, FlatType, Variable};
        |subs: &mut Subs| {
            $(let $field = $make_v(subs);)*
            $(let $opt_field = $make_opt_v(subs);)*
            let fields = vec![
                $( (stringify!($field).into(), RecordField::Required($field)) ,)*
                $( (stringify!($opt_field).into(), RecordField::Required($opt_field)) ,)*
            ];
            let fields = RecordFields::insert_into_subs(subs, fields);
            $crate::synth::synth_var(subs, Content::Structure(FlatType::Record(fields, Variable::EMPTY_RECORD)))
        }
    }};
    ([ $($tag:ident $($payload:expr)*),* ]$( $ext:tt )?) => {{
        use $crate::subs::{Subs, UnionTags, Content, FlatType, Variable};
        use roc_module::ident::TagName;
        |subs: &mut Subs| {
            $(
            let $tag = vec![ $( $payload(subs), )* ];
            )*
            let tags = UnionTags::insert_into_subs::<_, Vec<Variable>>(subs, vec![ $( (TagName(stringify!($tag).into()), $tag) ,)* ]);
            let mut ext = Variable::EMPTY_TAG_UNION;
            $( ext = $crate::v!($ext)(subs) )?;
            $crate::synth::synth_var(subs, Content::Structure(FlatType::TagUnion(tags, ext)))
        }
    }};
    ([ $($tag:ident $($payload:expr)*),* ] as $rec_var:ident) => {{
        use $crate::subs::{Subs, SubsIndex, Variable, Content, FlatType, UnionTags};
        use roc_module::ident::TagName;
        |subs: &mut Subs| {
            let $rec_var = subs.fresh_unnamed_flex_var();
            let rec_name_index =
                SubsIndex::push_new(&mut subs.field_names, stringify!($rec).into());

            $(
            let $tag = vec![ $( $payload(subs), )* ];
            )*
            let tags = UnionTags::insert_into_subs::<_, Vec<Variable>>(subs, vec![ $( (TagName(stringify!($tag).into()), $tag) ,)* ]);
            let tag_union_var = $crate::synth::synth_var(subs, Content::Structure(FlatType::RecursiveTagUnion($rec_var, tags, Variable::EMPTY_TAG_UNION)));

            subs.set_content(
                $rec_var,
                Content::RecursionVar {
                    structure: tag_union_var,
                    opt_name: Some(rec_name_index),
                },
            );
            tag_union_var
        }
    }};
    (Symbol::$sym:ident $($arg:expr)*) => {{
        use $crate::subs::{Subs, SubsSlice, Content, FlatType};
        use roc_module::symbol::Symbol;
        |subs: &mut Subs| {
            let $sym = vec![ $( $arg(subs) ,)* ];
            let var_slice = SubsSlice::insert_into_subs(subs, $sym);
            $crate::synth::synth_var(subs, Content::Structure(FlatType::Apply(Symbol::$sym, var_slice)))
        }
    }};
    (Symbol::$alias:ident $($arg:expr)* => $real_var:expr) => {{
        use $crate::subs::{Subs, AliasVariables, Content};
        use $crate::types::AliasKind;
        use roc_module::symbol::Symbol;
        |subs: &mut Subs| {
            let args = vec![$( $arg(subs) )*];
            let alias_variables = AliasVariables::insert_into_subs::<Vec<_>, Vec<_>>(subs, args, vec![]);
            let real_var = $real_var(subs);
            $crate::synth::synth_var(subs, Content::Alias(Symbol::$alias, alias_variables, real_var, AliasKind::Structural))
        }
    }};
    (@Symbol::$alias:ident $($arg:expr)* => $real_var:expr) => {{
        use $crate::subs::{Subs, AliasVariables, Content};
        use $crate::types::AliasKind;
        use roc_module::symbol::Symbol;
        |subs: &mut Subs| {
            let args = vec![$( $arg(subs) )*];
            let alias_variables = AliasVariables::insert_into_subs::<Vec<_>, Vec<_>>(subs, args, vec![]);
            let real_var = $real_var(subs);
            $crate::synth::synth_var(subs, Content::Alias(Symbol::$alias, alias_variables, real_var, AliasKind::Opaque))
        }
    }};
    (*) => {{
        use $crate::subs::{Subs, Content};
        |subs: &mut Subs| { $crate::synth::synth_var(subs, Content::FlexVar(None)) }
    }};
    (^$rec_var:ident) => {{
        use $crate::subs::{Subs, Variable};
        |_: &mut Subs| { $rec_var }
    }};
    ($var:ident) => {{
        use $crate::subs::{Subs};
        |_: &mut Subs| { Variable::$var }
    }};
}
