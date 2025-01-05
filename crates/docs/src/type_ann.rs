use bumpalo::Bump;
use roc_load::docs::{Tag, TypeAnnotation};
use roc_module::symbol::Symbol;
use roc_parse::ast::FunctionArrow;
use roc_types::subs::Subs;

#[derive(Debug, Clone)]
pub enum TypeAnnWithLink {
    TagUnion {
        tags: Vec<Tag>,
        extension: Box<TypeAnnWithLink>,
        url: String,
    },
    Function {
        args: Vec<TypeAnnWithLink>,
        arrow: FunctionArrow,
        output: Box<TypeAnnWithLink>,
        url: String,
    },
    ObscuredTagUnion {
        url: String,
    },
    ObscuredRecord {
        url: String,
    },
    BoundVariable(String, String), // (name, url)
    Apply {
        name: String,
        parts: Vec<TypeAnnWithLink>,
        url: String,
    },
    Record {
        fields: Vec<RecordFieldWithLink>,
        extension: Box<TypeAnnWithLink>,
        url: String,
    },
    Tuple {
        elems: Vec<TypeAnnWithLink>,
        extension: Box<TypeAnnWithLink>,
        url: String,
    },
    Ability {
        members: Vec<AbilityMemberWithLink>,
        url: String,
    },
    Wildcard {
        url: String,
    },
    NoTypeAnn,
    Where {
        ann: Box<TypeAnnWithLink>,
        implements: Vec<ImplementsClauseWithLink>,
        url: String,
    },
    As {
        ann: Box<TypeAnnWithLink>,
        name: String,
        vars: Vec<String>,
        url: String,
    },
}

#[derive(Debug, Clone)]
pub enum RecordFieldWithLink {
    RecordField {
        name: String,
        type_annotation: TypeAnnWithLink,
    },
    OptionalField {
        name: String,
        type_annotation: TypeAnnWithLink,
    },
    LabelOnly {
        name: String,
    },
}

#[derive(Debug, Clone)]
pub struct AbilityMemberWithLink {
    pub name: String,
    pub type_annotation: TypeAnnWithLink,
    pub able_variables: Vec<(String, Vec<TypeAnnWithLink>)>,
    pub docs: Option<String>,
}

#[derive(Debug, Clone)]
pub struct ImplementsClauseWithLink {
    pub name: String,
    pub abilities: Vec<TypeAnnWithLink>,
}

impl TypeAnnWithLink {
    pub fn new(ann: TypeAnnotation, symbol: Symbol, subs: &Subs) -> Self {
        // TODO
    }

    fn url(symbol: Symbol, subs: &Subs) {}
}
