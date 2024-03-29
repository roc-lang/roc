pub trait TypeVisitor<Type> {
    fn render(&mut self, render_type: impl Fn(&mut Self, Type), typ: Type) {
        render_type(self, typ)
    }

    fn tag_union(
        &mut self,
        render_type: impl Fn(&mut Self, Type),
        tags: impl Iterator<Item = (impl AsRef<str>, Type)>,
        ext: Type,
    );
    fn function(
        &mut self,
        render_type: impl Fn(&mut Self, Type),
        args: impl Iterator<Item = Type>,
        ret: Type,
    );
    fn apply_type(
        &mut self,
        render_type: impl Fn(&mut Self, Type),
        type_name: impl AsRef<str>,
        type_params: impl Iterator<Item = Type>,
    );
    fn record(
        &mut self,
        render_type: impl Fn(&mut Self, Type),
        fields: impl Iterator<Item = RecordField<impl AsRef<str>, Type>>,
        ext: Type,
    );
    fn tuple(
        &mut self,
        render_type: impl Fn(&mut Self, Type),
        elems: impl Iterator<Item = RecordField<impl AsRef<str>, Type>>,
        ext: Type,
    );
    fn with_where_clause(
        &mut self,
        render_type: impl Fn(&mut Self, Type),
        wrapped_type: Type,
        implements: impl Iterator<Item = (impl AsRef<str>, Type)>,
    );
    fn with_as(
        &mut self,
        render_type: impl Fn(&mut Self, Type),
        wrapped_type: Type,
        vars: impl Iterator<Item = impl Iterator<Item = impl AsRef<str>>>,
    );
    fn ability(
        &mut self,
        render_type: impl Fn(&mut Self, Type),
        members: impl Iterator<
            Item = AbilityMember<
                impl AsRef<str>,
                Type,
                impl Iterator<Item = (impl AsRef<str>, impl Iterator<Item = Type>)>,
            >,
        >,
    );
    fn obscured_tag_union(&mut self);
    fn obscured_record(&mut self);
    fn wildcard(&mut self);
    fn bound_variable(&mut self, var_name: impl AsRef<str>);
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
