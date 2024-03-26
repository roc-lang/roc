#[derive(Debug, Clone)]
pub enum Type<'a> {
    TagUnion {
        tags: &'a [(&'a str, Self)],
        extension: &'a Self,
    },
    Function {
        args: &'a [Self],
        output: &'a Self,
    },
    ObscuredTagUnion,
    ObscuredRecord,
    BoundVariable(&'a str),
    Apply {
        name: &'a str,
        parts: &'a [Self],
    },
    Record {
        fields: &'a [RecordField<'a>],
        extension: &'a Self,
    },
    Tuple {
        elems: &'a [Self],
        extension: &'a Self,
    },
    Ability {
        members: &'a [AbilityMember<'a>],
    },
    Wildcard,
    NoTypeAnn,
    Where {
        ann: &'a Self,
        implements: &'a [(&'a str, Self)],
    },
    As {
        ann: &'a Self,
        name: &'a str,
        vars: &'a [&'a str],
    },
}

#[derive(Debug, Clone)]
pub enum RecordField<'a> {
    RecordField {
        name: &'a str,
        type_annotation: &'a Type<'a>,
    },
    OptionalField {
        name: &'a str,
        type_annotation: &'a Type<'a>,
    },
    LabelOnly {
        name: &'a str,
    },
}

#[derive(Debug, Clone)]
pub struct AbilityMember<'a> {
    pub name: &'a str,
    pub type_annotation: Type<'a>,
    pub able_variables: &'a [(&'a str, &'a [Type<'a>])],
    pub docs: Option<&'a str>,
}
