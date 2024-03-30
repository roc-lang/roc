use bumpalo::{collections::string::String, Bump};

pub trait TypeVisitor<Type> {
    fn render<'a>(
        &mut self,
        arena: &'a Bump,
        render_type: impl Fn(&mut Self, &'a Bump, Type, &mut String<'a>),
        typ: Type,
        buf: &mut String<'a>,
    ) {
        render_type(self, arena, typ, buf)
    }

    fn tag_union<'a>(
        &mut self,
        arena: &'a Bump,
        render_type: impl Fn(&mut Self, Type),
        tags: impl Iterator<Item = (impl AsRef<str>, Type)>,
        ext: Type,
        buf: &mut String<'a>,
    );
    fn function<'a>(
        &mut self,
        arena: &'a Bump,
        render_type: impl Fn(&mut Self, Type),
        args: impl Iterator<Item = Type>,
        ret: Type,
        buf: &mut String<'a>,
    );
    fn apply_type<'a>(
        &mut self,
        arena: &'a Bump,
        render_type: impl Fn(&mut Self, Type),
        type_name: impl AsRef<str>,
        type_params: impl Iterator<Item = Type>,
        buf: &mut String<'a>,
    );
    fn record<'a>(
        &mut self,
        arena: &'a Bump,
        render_type: impl Fn(&mut Self, Type),
        fields: impl Iterator<Item = RecordField<impl AsRef<str>, Type>>,
        ext: Type,
        buf: &mut String<'a>,
    );
    fn tuple<'a>(
        &mut self,
        arena: &'a Bump,
        render_type: impl Fn(&mut Self, Type),
        elems: impl Iterator<Item = RecordField<impl AsRef<str>, Type>>,
        ext: Type,
        buf: &mut String<'a>,
    );
    fn with_where_clause<'a>(
        &mut self,
        arena: &'a Bump,
        render_type: impl Fn(&mut Self, Type),
        wrapped_type: Type,
        implements: impl Iterator<Item = (impl AsRef<str>, Type)>,
        buf: &mut String<'a>,
    );
    fn with_as<'a>(
        &mut self,
        arena: &'a Bump,
        render_type: impl Fn(&mut Self, Type),
        wrapped_type: Type,
        vars: impl Iterator<Item = impl Iterator<Item = impl AsRef<str>>>,
        buf: &mut String<'a>,
    );
    fn ability<'a>(
        &mut self,
        arena: &'a Bump,
        render_type: impl Fn(&mut Self, Type),
        members: impl Iterator<
            Item = AbilityMember<
                impl AsRef<str>,
                Type,
                impl Iterator<Item = (impl AsRef<str>, impl Iterator<Item = Type>)>,
            >,
        >,
        buf: &mut String<'a>,
    );
    fn obscured_tag_union<'a>(&mut self, arena: &'a Bump, buf: &mut String<'a>);
    fn obscured_record<'a>(&mut self, arena: &'a Bump, buf: &mut String<'a>);
    fn wildcard<'a>(&mut self, arena: &'a Bump, buf: &mut String<'a>);
    fn bound_variable<'a>(
        &mut self,
        arena: &'a Bump,
        var_name: impl AsRef<str>,
        buf: &mut String<'a>,
    );
}

#[derive(Debug, Clone)]
pub enum RecordField<Str, VisitType> {
    RecordField {
        name: Str,
        type_annotation: VisitType,
    },
    OptionalField {
        name: Str,
        type_annotation: VisitType,
    },
    LabelOnly {
        name: Str,
    },
}

#[derive(Debug, Clone)]
pub struct AbilityMember<Str, VisitType, VisitAbleVars> {
    pub name: Str,
    pub type_annotation: VisitType,
    // able_variables: &'a [(Str, &'a [Type<'a>])],
    pub able_variables: VisitAbleVars,
    pub docs: Option<Str>,
}
