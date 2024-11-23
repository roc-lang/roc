use std::collections::HashMap;

use roc_module::{ident, symbol};
use roc_types::{
    num,
    subs::{self, GetSubsSlice, Subs, SubsIndex, SubsSlice, UnionLabels},
    types,
};

use roc_checkmate_schema::{
    AliasKind, AliasTypeVariables, ClosureType, Content, NumericRange, NumericRangeKind, Rank,
    RecordField, RecordFieldKind, Symbol, TagUnionExtension, UnificationMode,
    UnspecializedClosureType, Variable,
};

pub trait AsSchema<T> {
    fn as_schema(&self, subs: &Subs) -> T;
}

impl<T, U> AsSchema<Option<U>> for Option<T>
where
    T: AsSchema<U>,
    T: Copy,
{
    fn as_schema(&self, subs: &Subs) -> Option<U> {
        self.map(|i| i.as_schema(subs))
    }
}

impl<T, U> AsSchema<Vec<U>> for &[T]
where
    T: AsSchema<U>,
{
    fn as_schema(&self, subs: &Subs) -> Vec<U> {
        self.iter().map(|i| i.as_schema(subs)).collect()
    }
}

impl<T, U> AsSchema<U> for SubsIndex<T>
where
    Subs: std::ops::Index<SubsIndex<T>, Output = T>,
    T: AsSchema<U>,
{
    fn as_schema(&self, subs: &Subs) -> U {
        subs[*self].as_schema(subs)
    }
}

impl<T, U> AsSchema<Vec<U>> for SubsSlice<T>
where
    Subs: GetSubsSlice<T>,
    T: AsSchema<U>,
{
    fn as_schema(&self, subs: &Subs) -> Vec<U> {
        subs.get_subs_slice(*self)
            .iter()
            .map(|i| i.as_schema(subs))
            .collect()
    }
}

impl AsSchema<Content> for subs::Content {
    fn as_schema(&self, subs: &Subs) -> Content {
        use {subs::Content as A, Content as B};
        match self {
            A::FlexVar(name) => B::Flex(name.as_schema(subs)),
            A::RigidVar(name) => B::Rigid(name.as_schema(subs)),
            A::FlexAbleVar(name, abilities) => {
                B::FlexAble(name.as_schema(subs), abilities.as_schema(subs))
            }
            A::RigidAbleVar(name, abilities) => {
                B::RigidAble(name.as_schema(subs), abilities.as_schema(subs))
            }
            A::RecursionVar {
                structure,
                opt_name,
            } => B::Recursive(opt_name.as_schema(subs), structure.as_schema(subs)),
            A::LambdaSet(lambda_set) => lambda_set.as_schema(subs),
            A::ErasedLambda => B::ErasedLambda(),
            A::Pure => B::Pure(),
            A::Effectful => B::Effectful(),
            A::Structure(flat_type) => flat_type.as_schema(subs),
            A::Alias(name, type_vars, real_var, kind) => B::Alias(
                name.as_schema(subs),
                type_vars.as_schema(subs),
                real_var.as_schema(subs),
                kind.as_schema(subs),
            ),
            A::RangedNumber(range) => B::RangedNumber(range.as_schema(subs)),
            A::Error => B::Error(),
        }
    }
}

impl AsSchema<Content> for subs::FlatType {
    fn as_schema(&self, subs: &Subs) -> Content {
        match self {
            subs::FlatType::Apply(symbol, variables) => {
                Content::Apply(symbol.as_schema(subs), variables.as_schema(subs))
            }
            subs::FlatType::Func(arguments, closure, ret, fx) => Content::Function(
                arguments.as_schema(subs),
                closure.as_schema(subs),
                ret.as_schema(subs),
                fx.as_schema(subs),
            ),
            subs::FlatType::Record(fields, ext) => {
                Content::Record(fields.as_schema(subs), ext.as_schema(subs))
            }
            subs::FlatType::Tuple(elems, ext) => {
                Content::Tuple(elems.as_schema(subs), ext.as_schema(subs))
            }
            subs::FlatType::TagUnion(tags, ext) => {
                Content::TagUnion(tags.as_schema(subs), ext.as_schema(subs))
            }
            subs::FlatType::FunctionOrTagUnion(tags, functions, ext) => {
                Content::FunctionOrTagUnion(
                    functions.as_schema(subs),
                    tags.as_schema(subs),
                    ext.as_schema(subs),
                )
            }
            subs::FlatType::RecursiveTagUnion(rec_var, tags, ext) => Content::RecursiveTagUnion(
                rec_var.as_schema(subs),
                tags.as_schema(subs),
                ext.as_schema(subs),
            ),
            subs::FlatType::EmptyRecord => Content::EmptyRecord(),
            subs::FlatType::EmptyTagUnion => Content::EmptyTagUnion(),
            subs::FlatType::EffectfulFunc => Content::EffectfulFunc(),
        }
    }
}

impl AsSchema<Content> for subs::LambdaSet {
    fn as_schema(&self, subs: &Subs) -> Content {
        let subs::LambdaSet {
            solved,
            unspecialized,
            recursion_var,
            ambient_function,
        } = self;

        Content::LambdaSet(
            solved.as_schema(subs),
            unspecialized.as_schema(subs),
            recursion_var.as_schema(subs),
            ambient_function.as_schema(subs),
        )
    }
}

impl AsSchema<String> for ident::Lowercase {
    fn as_schema(&self, _subs: &Subs) -> String {
        self.to_string()
    }
}

impl AsSchema<Symbol> for symbol::Symbol {
    fn as_schema(&self, _subs: &Subs) -> Symbol {
        Symbol(format!("{:#?}", self))
    }
}

impl AsSchema<Variable> for subs::Variable {
    fn as_schema(&self, _subs: &Subs) -> Variable {
        Variable(self.index())
    }
}

impl AsSchema<Option<Variable>> for subs::OptVariable {
    fn as_schema(&self, _subs: &Subs) -> Option<Variable> {
        self.into_variable().map(|i| i.as_schema(_subs))
    }
}

impl AsSchema<Vec<ClosureType>> for UnionLabels<symbol::Symbol> {
    fn as_schema(&self, subs: &Subs) -> Vec<ClosureType> {
        self.iter_from_subs(subs)
            .map(|(function, environment)| ClosureType {
                function: function.as_schema(subs),
                environment: environment.as_schema(subs),
            })
            .collect()
    }
}

impl AsSchema<UnspecializedClosureType> for types::Uls {
    fn as_schema(&self, subs: &Subs) -> UnspecializedClosureType {
        let types::Uls(specialization, ability_member, lambda_set_region) = self;

        UnspecializedClosureType {
            specialization: specialization.as_schema(subs),
            ability_member: ability_member.as_schema(subs),
            lambda_set_region: *lambda_set_region,
        }
    }
}

impl AsSchema<AliasTypeVariables> for subs::AliasVariables {
    fn as_schema(&self, subs: &Subs) -> AliasTypeVariables {
        let type_variables = self.type_variables().as_schema(subs);
        let lambda_set_variables = self.lambda_set_variables().as_schema(subs);
        let infer_ext_in_output_position_variables =
            self.infer_ext_in_output_variables().as_schema(subs);

        AliasTypeVariables {
            type_variables,
            lambda_set_variables,
            infer_ext_in_output_position_variables,
        }
    }
}

impl AsSchema<AliasKind> for types::AliasKind {
    fn as_schema(&self, _subs: &Subs) -> AliasKind {
        match self {
            types::AliasKind::Structural => AliasKind::Structural,
            types::AliasKind::Opaque => AliasKind::Opaque,
        }
    }
}

impl AsSchema<HashMap<String, RecordField>> for subs::RecordFields {
    fn as_schema(&self, subs: &Subs) -> HashMap<String, RecordField> {
        let mut map = HashMap::new();
        for (name, var, field) in self.iter_all() {
            let name = name.as_schema(subs);
            let field_type = var.as_schema(subs);
            let kind = field.as_schema(subs);
            map.insert(name, RecordField { field_type, kind });
        }
        map
    }
}

impl AsSchema<RecordFieldKind> for types::RecordField<()> {
    fn as_schema(&self, _subs: &Subs) -> RecordFieldKind {
        match self {
            types::RecordField::Demanded(_) => RecordFieldKind::Demanded,
            types::RecordField::Required(_) => RecordFieldKind::Required { rigid: false },
            types::RecordField::Optional(_) => RecordFieldKind::Optional { rigid: false },
            types::RecordField::RigidRequired(_) => RecordFieldKind::Required { rigid: true },
            types::RecordField::RigidOptional(_) => RecordFieldKind::Optional { rigid: true },
        }
    }
}

impl AsSchema<HashMap<u32, Variable>> for subs::TupleElems {
    fn as_schema(&self, subs: &Subs) -> HashMap<u32, Variable> {
        let mut map = HashMap::new();
        for (index, var) in self.iter_all() {
            let name = subs[index] as _;
            let var = var.as_schema(subs);
            map.insert(name, var);
        }
        map
    }
}

impl AsSchema<HashMap<String, Vec<Variable>>> for subs::UnionTags {
    fn as_schema(&self, subs: &Subs) -> HashMap<String, Vec<Variable>> {
        let mut map = HashMap::new();
        for (tag, payloads) in self.iter_from_subs(subs) {
            map.insert(tag.as_schema(subs), payloads.as_schema(subs));
        }
        map
    }
}

impl AsSchema<TagUnionExtension> for subs::TagExt {
    fn as_schema(&self, subs: &Subs) -> TagUnionExtension {
        match self {
            subs::TagExt::Openness(var) => TagUnionExtension::Openness(var.as_schema(subs)),
            subs::TagExt::Any(var) => TagUnionExtension::Any(var.as_schema(subs)),
        }
    }
}

impl AsSchema<NumericRange> for num::NumericRange {
    fn as_schema(&self, _subs: &Subs) -> NumericRange {
        let kind =
            match self {
                num::NumericRange::IntAtLeastSigned(_)
                | num::NumericRange::IntAtLeastEitherSign(_) => NumericRangeKind::Int,
                num::NumericRange::NumAtLeastSigned(_)
                | num::NumericRange::NumAtLeastEitherSign(_) => NumericRangeKind::AnyNum,
            };

        let min_width = self.min_width();
        let (signedness, width) = min_width.signedness_and_width();
        let signed = signedness.is_signed();

        NumericRange {
            kind,
            signed,
            min_width: width,
        }
    }
}

impl AsSchema<String> for ident::TagName {
    fn as_schema(&self, _subs: &Subs) -> String {
        self.0.to_string()
    }
}

impl AsSchema<Rank> for subs::Rank {
    fn as_schema(&self, _subs: &Subs) -> Rank {
        Rank(self.into_usize() as _)
    }
}

impl AsSchema<UnificationMode> for roc_solve_schema::UnificationMode {
    fn as_schema(&self, _subs: &Subs) -> UnificationMode {
        if self.is_eq() {
            UnificationMode::Eq
        } else if self.is_present() {
            UnificationMode::Present
        } else if self.is_lambda_set_specialization() {
            UnificationMode::LambdaSetSpecialization
        } else {
            unreachable!()
        }
    }
}
