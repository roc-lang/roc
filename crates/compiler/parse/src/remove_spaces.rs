use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_module::called_via::{BinOp, UnaryOp};
use roc_region::all::{Loc, Position, Region};

use crate::{
    ast::{
        AbilityImpls, AbilityMember, AssignedField, Collection, Defs, Expr, Header, Implements,
        ImplementsAbilities, ImplementsAbility, ImplementsClause, ImportAlias, ImportAsKeyword,
        ImportExposingKeyword, ImportedModuleName, IngestedFileAnnotation, IngestedFileImport,
        Module, ModuleImport, ModuleImportParams, OldRecordBuilderField, Pattern, PatternAs,
        Spaced, Spaces, StrLiteral, StrSegment, Tag, TypeAnnotation, TypeDef, TypeHeader, ValueDef,
        WhenBranch,
    },
    header::{
        AppHeader, ExposedName, ExposesKeyword, GeneratesKeyword, HostedHeader, ImportsEntry,
        ImportsKeyword, KeywordItem, ModuleHeader, ModuleName, ModuleParams, PackageEntry,
        PackageHeader, PackageKeyword, PackageName, PackagesKeyword, PlatformHeader,
        PlatformKeyword, PlatformRequires, ProvidesKeyword, ProvidesTo, RequiresKeyword, To,
        ToKeyword, TypedIdent, WithKeyword,
    },
    ident::{BadIdent, UppercaseIdent},
    parser::{
        EAbility, EClosure, EExpect, EExposes, EExpr, EGenerates, EGeneratesWith, EHeader, EIf,
        EImport, EImportParams, EImports, EInParens, EList, EPackageEntry, EPackageName, EPackages,
        EParams, EPattern, EProvides, ERecord, ERequires, EString, EType, ETypeAbilityImpl,
        ETypeApply, ETypeInParens, ETypeInlineAlias, ETypeRecord, ETypeTagUnion, ETypedIdent,
        EWhen, PInParens, PList, PRecord, SyntaxError,
    },
};

/// RemoveSpaces normalizes the ast to something that we _expect_ to be invariant under formatting.
///
/// Currently this consists of:
/// * Removing newlines
/// * Removing comments
/// * Removing parens in Exprs
///
/// Long term, we actually want this transform to preserve comments (so we can assert they're maintained by formatting)
/// - but there are currently several bugs where they're _not_ preserved.
/// TODO: ensure formatting retains comments
pub trait RemoveSpaces<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self;
}

macro_rules! keywords {
    ($($name:ident),* $(,)?) => {
        $(
            impl<'a> RemoveSpaces<'a> for $name {
                fn remove_spaces(&self, _arena: &'a Bump) -> Self {
                    *self
                }
            }
        )*
    }
}

keywords! {
    ExposesKeyword,
    ImportsKeyword,
    WithKeyword,
    GeneratesKeyword,
    PackageKeyword,
    PackagesKeyword,
    RequiresKeyword,
    ProvidesKeyword,
    ToKeyword,
    PlatformKeyword,
}

impl<'a> RemoveSpaces<'a> for Defs<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        let mut defs = self.clone();

        defs.spaces.clear();
        defs.space_before.clear();
        defs.space_after.clear();

        for type_def in defs.type_defs.iter_mut() {
            *type_def = type_def.remove_spaces(arena);
        }

        for value_def in defs.value_defs.iter_mut() {
            *value_def = value_def.remove_spaces(arena);
        }

        for region_def in defs.regions.iter_mut() {
            *region_def = region_def.remove_spaces(arena);
        }

        defs
    }
}

impl<'a, V: RemoveSpaces<'a>> RemoveSpaces<'a> for Spaces<'a, V> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        Spaces {
            before: &[],
            item: self.item.remove_spaces(arena),
            after: &[],
        }
    }
}

impl<'a, K: RemoveSpaces<'a>, V: RemoveSpaces<'a>> RemoveSpaces<'a> for KeywordItem<'a, K, V> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        KeywordItem {
            keyword: self.keyword.remove_spaces(arena),
            item: self.item.remove_spaces(arena),
        }
    }
}

impl<'a> RemoveSpaces<'a> for ProvidesTo<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        ProvidesTo {
            provides_keyword: self.provides_keyword.remove_spaces(arena),
            entries: self.entries.remove_spaces(arena),
            types: self.types.remove_spaces(arena),
            to_keyword: self.to_keyword.remove_spaces(arena),
            to: self.to.remove_spaces(arena),
        }
    }
}

impl<'a> RemoveSpaces<'a> for Module<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        let header = match &self.header {
            Header::Module(header) => Header::Module(ModuleHeader {
                after_keyword: &[],
                params: header.params.remove_spaces(arena),
                exposes: header.exposes.remove_spaces(arena),
                interface_imports: header.interface_imports.remove_spaces(arena),
            }),
            Header::App(header) => Header::App(AppHeader {
                before_provides: &[],
                provides: header.provides.remove_spaces(arena),
                before_packages: &[],
                packages: header.packages.remove_spaces(arena),
                old_imports: header.old_imports.remove_spaces(arena),
                old_provides_to_new_package: header
                    .old_provides_to_new_package
                    .remove_spaces(arena),
            }),
            Header::Package(header) => Header::Package(PackageHeader {
                before_exposes: &[],
                exposes: header.exposes.remove_spaces(arena),
                before_packages: &[],
                packages: header.packages.remove_spaces(arena),
            }),
            Header::Platform(header) => Header::Platform(PlatformHeader {
                before_name: &[],
                name: header.name.remove_spaces(arena),
                requires: header.requires.remove_spaces(arena),
                exposes: header.exposes.remove_spaces(arena),
                packages: header.packages.remove_spaces(arena),
                imports: header.imports.remove_spaces(arena),
                provides: header.provides.remove_spaces(arena),
            }),
            Header::Hosted(header) => Header::Hosted(HostedHeader {
                before_name: &[],
                name: header.name.remove_spaces(arena),
                exposes: header.exposes.remove_spaces(arena),
                imports: header.imports.remove_spaces(arena),
                generates: header.generates.remove_spaces(arena),
                generates_with: header.generates_with.remove_spaces(arena),
            }),
        };
        Module {
            comments: &[],
            header,
        }
    }
}

impl<'a> RemoveSpaces<'a> for ModuleParams<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        ModuleParams {
            params: self.params.remove_spaces(arena),
            before_arrow: &[],
            after_arrow: &[],
        }
    }
}

impl<'a> RemoveSpaces<'a> for Region {
    fn remove_spaces(&self, _arena: &'a Bump) -> Self {
        Region::zero()
    }
}

impl<'a> RemoveSpaces<'a> for &'a str {
    fn remove_spaces(&self, _arena: &'a Bump) -> Self {
        self
    }
}

impl<'a, T: RemoveSpaces<'a> + Copy> RemoveSpaces<'a> for Spaced<'a, T> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            Spaced::Item(a) => Spaced::Item(a.remove_spaces(arena)),
            Spaced::SpaceBefore(a, _) => a.remove_spaces(arena),
            Spaced::SpaceAfter(a, _) => a.remove_spaces(arena),
        }
    }
}

impl<'a> RemoveSpaces<'a> for ExposedName<'a> {
    fn remove_spaces(&self, _arena: &'a Bump) -> Self {
        *self
    }
}

impl<'a> RemoveSpaces<'a> for ModuleName<'a> {
    fn remove_spaces(&self, _arena: &'a Bump) -> Self {
        *self
    }
}

impl<'a> RemoveSpaces<'a> for PackageName<'a> {
    fn remove_spaces(&self, _arena: &'a Bump) -> Self {
        *self
    }
}

impl<'a> RemoveSpaces<'a> for To<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            To::ExistingPackage(a) => To::ExistingPackage(a),
            To::NewPackage(a) => To::NewPackage(a.remove_spaces(arena)),
        }
    }
}

impl<'a> RemoveSpaces<'a> for TypedIdent<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        TypedIdent {
            ident: self.ident.remove_spaces(arena),
            spaces_before_colon: &[],
            ann: self.ann.remove_spaces(arena),
        }
    }
}

impl<'a> RemoveSpaces<'a> for PlatformRequires<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        PlatformRequires {
            rigids: self.rigids.remove_spaces(arena),
            signature: self.signature.remove_spaces(arena),
        }
    }
}

impl<'a> RemoveSpaces<'a> for UppercaseIdent<'a> {
    fn remove_spaces(&self, _arena: &'a Bump) -> Self {
        *self
    }
}

impl<'a> RemoveSpaces<'a> for PackageEntry<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        PackageEntry {
            shorthand: self.shorthand,
            spaces_after_shorthand: &[],
            platform_marker: match self.platform_marker {
                Some(_) => Some(&[]),
                None => None,
            },
            package_name: self.package_name.remove_spaces(arena),
        }
    }
}

impl<'a> RemoveSpaces<'a> for ImportsEntry<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            ImportsEntry::Module(a, b) => ImportsEntry::Module(a, b.remove_spaces(arena)),
            ImportsEntry::Package(a, b, c) => ImportsEntry::Package(a, b, c.remove_spaces(arena)),
            ImportsEntry::IngestedFile(a, b) => {
                ImportsEntry::IngestedFile(a, b.remove_spaces(arena))
            }
        }
    }
}

impl<'a, T: RemoveSpaces<'a>> RemoveSpaces<'a> for Option<T> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        self.as_ref().map(|a| a.remove_spaces(arena))
    }
}

impl<'a, T: RemoveSpaces<'a> + std::fmt::Debug> RemoveSpaces<'a> for Loc<T> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        let res = self.value.remove_spaces(arena);
        Loc::at(Region::zero(), res)
    }
}

impl<'a, A: RemoveSpaces<'a>, B: RemoveSpaces<'a>> RemoveSpaces<'a> for (A, B) {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        (self.0.remove_spaces(arena), self.1.remove_spaces(arena))
    }
}

impl<'a, T: RemoveSpaces<'a>> RemoveSpaces<'a> for Collection<'a, T> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        let mut items = Vec::with_capacity_in(self.items.len(), arena);
        for item in self.items {
            items.push(item.remove_spaces(arena));
        }
        Collection::with_items(items.into_bump_slice())
    }
}

impl<'a, T: RemoveSpaces<'a> + std::fmt::Debug> RemoveSpaces<'a> for &'a [T] {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        let mut items = Vec::with_capacity_in(self.len(), arena);
        for item in *self {
            let res = item.remove_spaces(arena);
            items.push(res);
        }
        items.into_bump_slice()
    }
}

impl<'a> RemoveSpaces<'a> for UnaryOp {
    fn remove_spaces(&self, _arena: &'a Bump) -> Self {
        *self
    }
}

impl<'a> RemoveSpaces<'a> for BinOp {
    fn remove_spaces(&self, _arena: &'a Bump) -> Self {
        *self
    }
}

impl<'a, T: RemoveSpaces<'a>> RemoveSpaces<'a> for &'a T {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        arena.alloc((*self).remove_spaces(arena))
    }
}

impl<'a> RemoveSpaces<'a> for TypeDef<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        use TypeDef::*;

        match *self {
            Alias {
                header: TypeHeader { name, vars },
                ann,
            } => Alias {
                header: TypeHeader {
                    name: name.remove_spaces(arena),
                    vars: vars.remove_spaces(arena),
                },
                ann: ann.remove_spaces(arena),
            },
            Opaque {
                header: TypeHeader { name, vars },
                typ,
                derived,
            } => Opaque {
                header: TypeHeader {
                    name: name.remove_spaces(arena),
                    vars: vars.remove_spaces(arena),
                },
                typ: typ.remove_spaces(arena),
                derived: derived.remove_spaces(arena),
            },
            Ability {
                header: TypeHeader { name, vars },
                loc_implements: loc_has,
                members,
            } => Ability {
                header: TypeHeader {
                    name: name.remove_spaces(arena),
                    vars: vars.remove_spaces(arena),
                },
                loc_implements: loc_has.remove_spaces(arena),
                members: members.remove_spaces(arena),
            },
        }
    }
}

impl<'a> RemoveSpaces<'a> for ValueDef<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        use ValueDef::*;

        match *self {
            Annotation(a, b) => Annotation(a.remove_spaces(arena), b.remove_spaces(arena)),
            Body(a, b) => Body(
                arena.alloc(a.remove_spaces(arena)),
                arena.alloc(b.remove_spaces(arena)),
            ),
            AnnotatedBody {
                ann_pattern,
                ann_type,
                lines_between: _,
                body_pattern,
                body_expr,
            } => AnnotatedBody {
                ann_pattern: arena.alloc(ann_pattern.remove_spaces(arena)),
                ann_type: arena.alloc(ann_type.remove_spaces(arena)),
                lines_between: &[],
                body_pattern: arena.alloc(body_pattern.remove_spaces(arena)),
                body_expr: arena.alloc(body_expr.remove_spaces(arena)),
            },
            Dbg {
                condition,
                preceding_comment: _,
            } => Dbg {
                condition: arena.alloc(condition.remove_spaces(arena)),
                preceding_comment: Region::zero(),
            },
            Expect {
                condition,
                preceding_comment: _,
            } => Expect {
                condition: arena.alloc(condition.remove_spaces(arena)),
                preceding_comment: Region::zero(),
            },
            ExpectFx {
                condition,
                preceding_comment: _,
            } => ExpectFx {
                condition: arena.alloc(condition.remove_spaces(arena)),
                preceding_comment: Region::zero(),
            },
            ModuleImport(module_import) => ModuleImport(module_import.remove_spaces(arena)),
            IngestedFileImport(ingested_file_import) => {
                IngestedFileImport(ingested_file_import.remove_spaces(arena))
            }
            Stmt(loc_expr) => Stmt(arena.alloc(loc_expr.remove_spaces(arena))),
        }
    }
}

impl<'a> RemoveSpaces<'a> for ModuleImport<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        ModuleImport {
            before_name: &[],
            name: self.name.remove_spaces(arena),
            params: self.params.remove_spaces(arena),
            alias: self.alias.remove_spaces(arena),
            exposed: self.exposed.remove_spaces(arena),
        }
    }
}

impl<'a> RemoveSpaces<'a> for ModuleImportParams<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        ModuleImportParams {
            before: &[],
            params: self.params.remove_spaces(arena),
        }
    }
}

impl<'a> RemoveSpaces<'a> for IngestedFileImport<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        IngestedFileImport {
            before_path: &[],
            path: self.path.remove_spaces(arena),
            name: self.name.remove_spaces(arena),
            annotation: self.annotation.remove_spaces(arena),
        }
    }
}

impl<'a> RemoveSpaces<'a> for ImportedModuleName<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        ImportedModuleName {
            package: self.package.remove_spaces(arena),
            name: self.name.remove_spaces(arena),
        }
    }
}

impl<'a> RemoveSpaces<'a> for ImportAlias<'a> {
    fn remove_spaces(&self, _arena: &'a Bump) -> Self {
        *self
    }
}

impl<'a> RemoveSpaces<'a> for ImportAsKeyword {
    fn remove_spaces(&self, _arena: &'a Bump) -> Self {
        *self
    }
}

impl<'a> RemoveSpaces<'a> for ImportExposingKeyword {
    fn remove_spaces(&self, _arena: &'a Bump) -> Self {
        *self
    }
}

impl<'a> RemoveSpaces<'a> for IngestedFileAnnotation<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        IngestedFileAnnotation {
            before_colon: &[],
            annotation: self.annotation.remove_spaces(arena),
        }
    }
}

impl<'a> RemoveSpaces<'a> for Implements<'a> {
    fn remove_spaces(&self, _arena: &'a Bump) -> Self {
        Implements::Implements
    }
}

impl<'a> RemoveSpaces<'a> for AbilityMember<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        AbilityMember {
            name: self.name.remove_spaces(arena),
            typ: self.typ.remove_spaces(arena),
        }
    }
}

impl<'a> RemoveSpaces<'a> for WhenBranch<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        WhenBranch {
            patterns: self.patterns.remove_spaces(arena),
            value: self.value.remove_spaces(arena),
            guard: self.guard.remove_spaces(arena),
        }
    }
}

impl<'a, T: RemoveSpaces<'a> + Copy + std::fmt::Debug> RemoveSpaces<'a> for AssignedField<'a, T> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            AssignedField::RequiredValue(a, _, c) => AssignedField::RequiredValue(
                a.remove_spaces(arena),
                arena.alloc([]),
                arena.alloc(c.remove_spaces(arena)),
            ),
            AssignedField::OptionalValue(a, _, c) => AssignedField::OptionalValue(
                a.remove_spaces(arena),
                arena.alloc([]),
                arena.alloc(c.remove_spaces(arena)),
            ),
            AssignedField::LabelOnly(a) => AssignedField::LabelOnly(a.remove_spaces(arena)),
            AssignedField::Malformed(a) => AssignedField::Malformed(a),
            AssignedField::SpaceBefore(a, _) => a.remove_spaces(arena),
            AssignedField::SpaceAfter(a, _) => a.remove_spaces(arena),
        }
    }
}

impl<'a> RemoveSpaces<'a> for OldRecordBuilderField<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            OldRecordBuilderField::Value(a, _, c) => OldRecordBuilderField::Value(
                a.remove_spaces(arena),
                &[],
                arena.alloc(c.remove_spaces(arena)),
            ),
            OldRecordBuilderField::ApplyValue(a, _, _, c) => OldRecordBuilderField::ApplyValue(
                a.remove_spaces(arena),
                &[],
                &[],
                arena.alloc(c.remove_spaces(arena)),
            ),
            OldRecordBuilderField::LabelOnly(a) => {
                OldRecordBuilderField::LabelOnly(a.remove_spaces(arena))
            }
            OldRecordBuilderField::Malformed(a) => OldRecordBuilderField::Malformed(a),
            OldRecordBuilderField::SpaceBefore(a, _) => a.remove_spaces(arena),
            OldRecordBuilderField::SpaceAfter(a, _) => a.remove_spaces(arena),
        }
    }
}

impl<'a> RemoveSpaces<'a> for StrLiteral<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            StrLiteral::PlainLine(t) => StrLiteral::PlainLine(t),
            StrLiteral::Line(t) => StrLiteral::Line(t.remove_spaces(arena)),
            StrLiteral::Block(t) => StrLiteral::Block(t.remove_spaces(arena)),
        }
    }
}

impl<'a> RemoveSpaces<'a> for StrSegment<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            StrSegment::Plaintext(t) => StrSegment::Plaintext(t),
            StrSegment::Unicode(t) => StrSegment::Unicode(t.remove_spaces(arena)),
            StrSegment::EscapedChar(c) => StrSegment::EscapedChar(c),
            StrSegment::Interpolated(t) => StrSegment::Interpolated(t.remove_spaces(arena)),
            StrSegment::DeprecatedInterpolated(t) => {
                StrSegment::Interpolated(t.remove_spaces(arena))
            }
        }
    }
}

impl<'a> RemoveSpaces<'a> for Expr<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            Expr::Float(a) => Expr::Float(a),
            Expr::Num(a) => Expr::Num(a),
            Expr::NonBase10Int {
                string,
                base,
                is_negative,
            } => Expr::NonBase10Int {
                string,
                base,
                is_negative,
            },
            Expr::Str(a) => Expr::Str(a.remove_spaces(arena)),
            Expr::RecordAccess(a, b) => Expr::RecordAccess(arena.alloc(a.remove_spaces(arena)), b),
            Expr::AccessorFunction(a) => Expr::AccessorFunction(a),
            Expr::TupleAccess(a, b) => Expr::TupleAccess(arena.alloc(a.remove_spaces(arena)), b),
            Expr::TaskAwaitBang(a) => Expr::TaskAwaitBang(arena.alloc(a.remove_spaces(arena))),
            Expr::List(a) => Expr::List(a.remove_spaces(arena)),
            Expr::RecordUpdate { update, fields } => Expr::RecordUpdate {
                update: arena.alloc(update.remove_spaces(arena)),
                fields: fields.remove_spaces(arena),
            },
            Expr::Record(a) => Expr::Record(a.remove_spaces(arena)),
            Expr::OldRecordBuilder(a) => Expr::OldRecordBuilder(a.remove_spaces(arena)),
            Expr::RecordBuilder { mapper, fields } => Expr::RecordBuilder {
                mapper: arena.alloc(mapper.remove_spaces(arena)),
                fields: fields.remove_spaces(arena),
            },
            Expr::Tuple(a) => Expr::Tuple(a.remove_spaces(arena)),
            Expr::Var { module_name, ident } => Expr::Var { module_name, ident },
            Expr::Underscore(a) => Expr::Underscore(a),
            Expr::Tag(a) => Expr::Tag(a),
            Expr::OpaqueRef(a) => Expr::OpaqueRef(a),
            Expr::Closure(a, b) => Expr::Closure(
                arena.alloc(a.remove_spaces(arena)),
                arena.alloc(b.remove_spaces(arena)),
            ),
            Expr::Crash => Expr::Crash,
            Expr::Defs(a, b) => {
                let mut defs = a.clone();
                defs.space_before = vec![Default::default(); defs.len()];
                defs.space_after = vec![Default::default(); defs.len()];
                defs.regions = vec![Region::zero(); defs.len()];
                defs.spaces.clear();

                for type_def in defs.type_defs.iter_mut() {
                    *type_def = type_def.remove_spaces(arena);
                }

                for value_def in defs.value_defs.iter_mut() {
                    *value_def = value_def.remove_spaces(arena);
                }

                Expr::Defs(arena.alloc(defs), arena.alloc(b.remove_spaces(arena)))
            }
            Expr::Backpassing(a, b, c) => Expr::Backpassing(
                arena.alloc(a.remove_spaces(arena)),
                arena.alloc(b.remove_spaces(arena)),
                arena.alloc(c.remove_spaces(arena)),
            ),
            Expr::Expect(a, b) => Expr::Expect(
                arena.alloc(a.remove_spaces(arena)),
                arena.alloc(b.remove_spaces(arena)),
            ),
            Expr::Dbg(a, b) => Expr::Dbg(
                arena.alloc(a.remove_spaces(arena)),
                arena.alloc(b.remove_spaces(arena)),
            ),
            Expr::LowLevelDbg(x, a, b) => Expr::LowLevelDbg(
                x,
                arena.alloc(a.remove_spaces(arena)),
                arena.alloc(b.remove_spaces(arena)),
            ),
            Expr::Apply(a, b, c) => Expr::Apply(
                arena.alloc(a.remove_spaces(arena)),
                b.remove_spaces(arena),
                c,
            ),
            Expr::BinOps(a, b) => {
                Expr::BinOps(a.remove_spaces(arena), arena.alloc(b.remove_spaces(arena)))
            }
            Expr::UnaryOp(a, b) => {
                Expr::UnaryOp(arena.alloc(a.remove_spaces(arena)), b.remove_spaces(arena))
            }
            Expr::If(a, b) => Expr::If(a.remove_spaces(arena), arena.alloc(b.remove_spaces(arena))),
            Expr::When(a, b) => {
                Expr::When(arena.alloc(a.remove_spaces(arena)), b.remove_spaces(arena))
            }
            Expr::ParensAround(a) => {
                // The formatter can remove redundant parentheses, so also remove these when normalizing for comparison.
                a.remove_spaces(arena)
            }
            Expr::MalformedIdent(a, b) => Expr::MalformedIdent(a, remove_spaces_bad_ident(b)),
            Expr::MalformedClosure => Expr::MalformedClosure,
            Expr::MalformedSuffixed(a) => Expr::MalformedSuffixed(a),
            Expr::PrecedenceConflict(a) => Expr::PrecedenceConflict(a),
            Expr::SpaceBefore(a, _) => a.remove_spaces(arena),
            Expr::SpaceAfter(a, _) => a.remove_spaces(arena),
            Expr::SingleQuote(a) => Expr::Num(a),
            Expr::MultipleOldRecordBuilders(a) => {
                Expr::MultipleOldRecordBuilders(arena.alloc(a.remove_spaces(arena)))
            }
            Expr::UnappliedOldRecordBuilder(a) => {
                Expr::UnappliedOldRecordBuilder(arena.alloc(a.remove_spaces(arena)))
            }
            Expr::EmptyRecordBuilder(a) => {
                Expr::EmptyRecordBuilder(arena.alloc(a.remove_spaces(arena)))
            }
            Expr::SingleFieldRecordBuilder(a) => {
                Expr::SingleFieldRecordBuilder(arena.alloc(a.remove_spaces(arena)))
            }
            Expr::OptionalFieldInRecordBuilder(a, b) => Expr::OptionalFieldInRecordBuilder(
                arena.alloc(a.remove_spaces(arena)),
                arena.alloc(b.remove_spaces(arena)),
            ),
        }
    }
}

fn remove_spaces_bad_ident(ident: BadIdent) -> BadIdent {
    match ident {
        BadIdent::Start(_) => BadIdent::Start(Position::zero()),
        BadIdent::Space(e, _) => BadIdent::Space(e, Position::zero()),
        BadIdent::UnderscoreAlone(_) => BadIdent::UnderscoreAlone(Position::zero()),
        BadIdent::UnderscoreInMiddle(_) => BadIdent::UnderscoreInMiddle(Position::zero()),
        BadIdent::UnderscoreAtStart {
            position: _,
            declaration_region,
        } => BadIdent::UnderscoreAtStart {
            position: Position::zero(),
            declaration_region,
        },
        BadIdent::QualifiedTag(_) => BadIdent::QualifiedTag(Position::zero()),
        BadIdent::WeirdAccessor(_) => BadIdent::WeirdAccessor(Position::zero()),
        BadIdent::WeirdDotAccess(_) => BadIdent::WeirdDotAccess(Position::zero()),
        BadIdent::WeirdDotQualified(_) => BadIdent::WeirdDotQualified(Position::zero()),
        BadIdent::StrayDot(_) => BadIdent::StrayDot(Position::zero()),
        BadIdent::BadOpaqueRef(_) => BadIdent::BadOpaqueRef(Position::zero()),
        BadIdent::QualifiedTupleAccessor(_) => BadIdent::QualifiedTupleAccessor(Position::zero()),
    }
}

impl<'a> RemoveSpaces<'a> for Pattern<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            Pattern::Identifier { ident } => Pattern::Identifier { ident },
            Pattern::Tag(a) => Pattern::Tag(a),
            Pattern::OpaqueRef(a) => Pattern::OpaqueRef(a),
            Pattern::Apply(a, b) => Pattern::Apply(
                arena.alloc(a.remove_spaces(arena)),
                arena.alloc(b.remove_spaces(arena)),
            ),
            Pattern::RecordDestructure(a) => Pattern::RecordDestructure(a.remove_spaces(arena)),
            Pattern::RequiredField(a, b) => {
                Pattern::RequiredField(a, arena.alloc(b.remove_spaces(arena)))
            }
            Pattern::OptionalField(a, b) => {
                Pattern::OptionalField(a, arena.alloc(b.remove_spaces(arena)))
            }
            Pattern::As(pattern, pattern_as) => Pattern::As(
                arena.alloc(pattern.remove_spaces(arena)),
                pattern_as.remove_spaces(arena),
            ),
            Pattern::NumLiteral(a) => Pattern::NumLiteral(a),
            Pattern::NonBase10Literal {
                string,
                base,
                is_negative,
            } => Pattern::NonBase10Literal {
                string,
                base,
                is_negative,
            },
            Pattern::FloatLiteral(a) => Pattern::FloatLiteral(a),
            Pattern::StrLiteral(a) => Pattern::StrLiteral(a),
            Pattern::Underscore(a) => Pattern::Underscore(a),
            Pattern::Malformed(a) => Pattern::Malformed(a),
            Pattern::MalformedIdent(a, b) => Pattern::MalformedIdent(a, remove_spaces_bad_ident(b)),
            Pattern::QualifiedIdentifier { module_name, ident } => {
                Pattern::QualifiedIdentifier { module_name, ident }
            }
            Pattern::SpaceBefore(a, _) => a.remove_spaces(arena),
            Pattern::SpaceAfter(a, _) => a.remove_spaces(arena),
            Pattern::SingleQuote(a) => Pattern::SingleQuote(a),
            Pattern::List(pats) => Pattern::List(pats.remove_spaces(arena)),
            Pattern::Tuple(pats) => Pattern::Tuple(pats.remove_spaces(arena)),
            Pattern::ListRest(opt_pattern_as) => Pattern::ListRest(
                opt_pattern_as
                    .map(|(_, pattern_as)| ([].as_ref(), pattern_as.remove_spaces(arena))),
            ),
        }
    }
}

impl<'a> RemoveSpaces<'a> for TypeAnnotation<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            TypeAnnotation::Function(a, b) => TypeAnnotation::Function(
                arena.alloc(a.remove_spaces(arena)),
                arena.alloc(b.remove_spaces(arena)),
            ),
            TypeAnnotation::Apply(a, b, c) => TypeAnnotation::Apply(a, b, c.remove_spaces(arena)),
            TypeAnnotation::BoundVariable(a) => TypeAnnotation::BoundVariable(a),
            TypeAnnotation::As(a, _, TypeHeader { name, vars }) => TypeAnnotation::As(
                arena.alloc(a.remove_spaces(arena)),
                &[],
                TypeHeader {
                    name: name.remove_spaces(arena),
                    vars: vars.remove_spaces(arena),
                },
            ),
            TypeAnnotation::Tuple { elems: fields, ext } => TypeAnnotation::Tuple {
                elems: fields.remove_spaces(arena),
                ext: ext.remove_spaces(arena),
            },
            TypeAnnotation::Record { fields, ext } => TypeAnnotation::Record {
                fields: fields.remove_spaces(arena),
                ext: ext.remove_spaces(arena),
            },
            TypeAnnotation::TagUnion { ext, tags } => TypeAnnotation::TagUnion {
                ext: ext.remove_spaces(arena),
                tags: tags.remove_spaces(arena),
            },
            TypeAnnotation::Inferred => TypeAnnotation::Inferred,
            TypeAnnotation::Wildcard => TypeAnnotation::Wildcard,
            TypeAnnotation::Where(annot, has_clauses) => TypeAnnotation::Where(
                arena.alloc(annot.remove_spaces(arena)),
                arena.alloc(has_clauses.remove_spaces(arena)),
            ),
            TypeAnnotation::SpaceBefore(a, _) => a.remove_spaces(arena),
            TypeAnnotation::SpaceAfter(a, _) => a.remove_spaces(arena),
            TypeAnnotation::Malformed(a) => TypeAnnotation::Malformed(a),
        }
    }
}

impl<'a> RemoveSpaces<'a> for ImplementsClause<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        ImplementsClause {
            var: self.var.remove_spaces(arena),
            abilities: self.abilities.remove_spaces(arena),
        }
    }
}

impl<'a> RemoveSpaces<'a> for Tag<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            Tag::Apply { name, args } => Tag::Apply {
                name: name.remove_spaces(arena),
                args: args.remove_spaces(arena),
            },
            Tag::Malformed(a) => Tag::Malformed(a),
            Tag::SpaceBefore(a, _) => a.remove_spaces(arena),
            Tag::SpaceAfter(a, _) => a.remove_spaces(arena),
        }
    }
}

impl<'a> RemoveSpaces<'a> for AbilityImpls<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            AbilityImpls::AbilityImpls(impls) => {
                AbilityImpls::AbilityImpls(impls.remove_spaces(arena))
            }
            AbilityImpls::SpaceBefore(has, _) | AbilityImpls::SpaceAfter(has, _) => {
                has.remove_spaces(arena)
            }
        }
    }
}

impl<'a> RemoveSpaces<'a> for ImplementsAbility<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            ImplementsAbility::ImplementsAbility { ability, impls } => {
                ImplementsAbility::ImplementsAbility {
                    ability: ability.remove_spaces(arena),
                    impls: impls.remove_spaces(arena),
                }
            }
            ImplementsAbility::SpaceBefore(has, _) | ImplementsAbility::SpaceAfter(has, _) => {
                has.remove_spaces(arena)
            }
        }
    }
}

impl<'a> RemoveSpaces<'a> for ImplementsAbilities<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            ImplementsAbilities::Implements(derived) => {
                ImplementsAbilities::Implements(derived.remove_spaces(arena))
            }
            ImplementsAbilities::SpaceBefore(derived, _)
            | ImplementsAbilities::SpaceAfter(derived, _) => derived.remove_spaces(arena),
        }
    }
}

impl<'a> RemoveSpaces<'a> for PatternAs<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        PatternAs {
            spaces_before: &[],
            identifier: self.identifier.remove_spaces(arena),
        }
    }
}

impl<'a> RemoveSpaces<'a> for EExpr<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match self {
            EExpr::TrailingOperator(_pos) => EExpr::TrailingOperator(Position::zero()),
            EExpr::Start(_pos) => EExpr::Start(Position::zero()),
            EExpr::End(_pos) => EExpr::End(Position::zero()),
            EExpr::BadExprEnd(_pos) => EExpr::BadExprEnd(Position::zero()),
            EExpr::Space(inner_err, _pos) => EExpr::Space(*inner_err, Position::zero()),
            EExpr::Dot(_pos) => EExpr::Dot(Position::zero()),
            EExpr::Access(_pos) => EExpr::Access(Position::zero()),
            EExpr::UnaryNot(_pos) => EExpr::UnaryNot(Position::zero()),
            EExpr::UnaryNegate(_pos) => EExpr::UnaryNegate(Position::zero()),
            EExpr::BadOperator(inner_err, _pos) => EExpr::BadOperator(
                arena.alloc(inner_err.remove_spaces(arena)),
                Position::zero(),
            ),
            EExpr::DefMissingFinalExpr(_pos) => EExpr::DefMissingFinalExpr(Position::zero()),
            EExpr::DefMissingFinalExpr2(inner_err, _pos) => EExpr::DefMissingFinalExpr2(
                arena.alloc(inner_err.remove_spaces(arena)),
                Position::zero(),
            ),
            EExpr::Type(inner_err, _pos) => {
                EExpr::Type(inner_err.remove_spaces(arena), Position::zero())
            }
            EExpr::Pattern(inner_err, _pos) => EExpr::Pattern(
                arena.alloc(inner_err.remove_spaces(arena)),
                Position::zero(),
            ),
            EExpr::Ability(inner_err, _pos) => {
                EExpr::Ability(inner_err.remove_spaces(arena), Position::zero())
            }
            EExpr::IndentDefBody(_pos) => EExpr::IndentDefBody(Position::zero()),
            EExpr::IndentEquals(_pos) => EExpr::IndentEquals(Position::zero()),
            EExpr::IndentAnnotation(_pos) => EExpr::IndentAnnotation(Position::zero()),
            EExpr::Equals(_pos) => EExpr::Equals(Position::zero()),
            EExpr::Colon(_pos) => EExpr::Colon(Position::zero()),
            EExpr::DoubleColon(_pos) => EExpr::DoubleColon(Position::zero()),
            EExpr::Ident(_pos) => EExpr::Ident(Position::zero()),
            EExpr::ElmStyleFunction(_region, _pos) => {
                EExpr::ElmStyleFunction(Region::zero(), Position::zero())
            }
            EExpr::MalformedPattern(_pos) => EExpr::MalformedPattern(Position::zero()),
            EExpr::QualifiedTag(_pos) => EExpr::QualifiedTag(Position::zero()),
            EExpr::BackpassComma(_pos) => EExpr::BackpassComma(Position::zero()),
            EExpr::BackpassArrow(_pos) => EExpr::BackpassArrow(Position::zero()),
            EExpr::BackpassContinue(_pos) => EExpr::BackpassContinue(Position::zero()),
            EExpr::DbgContinue(_pos) => EExpr::DbgContinue(Position::zero()),
            EExpr::When(inner_err, _pos) => {
                EExpr::When(inner_err.remove_spaces(arena), Position::zero())
            }
            EExpr::If(inner_err, _pos) => {
                EExpr::If(inner_err.remove_spaces(arena), Position::zero())
            }
            EExpr::Expect(inner_err, _pos) => {
                EExpr::Expect(inner_err.remove_spaces(arena), Position::zero())
            }
            EExpr::Dbg(inner_err, _pos) => {
                EExpr::Dbg(inner_err.remove_spaces(arena), Position::zero())
            }
            EExpr::Import(inner_err, _pos) => {
                EExpr::Import(inner_err.remove_spaces(arena), Position::zero())
            }
            EExpr::Closure(inner_err, _pos) => {
                EExpr::Closure(inner_err.remove_spaces(arena), Position::zero())
            }
            EExpr::Underscore(_pos) => EExpr::Underscore(Position::zero()),
            EExpr::Crash(_pos) => EExpr::Crash(Position::zero()),
            EExpr::InParens(inner_err, _pos) => {
                EExpr::InParens(inner_err.remove_spaces(arena), Position::zero())
            }
            EExpr::Record(inner_err, _pos) => {
                EExpr::Record(inner_err.remove_spaces(arena), Position::zero())
            }
            EExpr::OptionalValueInRecordBuilder(_pos) => {
                EExpr::OptionalValueInRecordBuilder(Region::zero())
            }
            EExpr::Str(inner_err, _pos) => {
                EExpr::Str(inner_err.remove_spaces(arena), Position::zero())
            }
            EExpr::Number(inner_err, _pos) => EExpr::Number(inner_err.clone(), Position::zero()),
            EExpr::List(inner_err, _pos) => {
                EExpr::List(inner_err.remove_spaces(arena), Position::zero())
            }
            EExpr::IndentStart(_pos) => EExpr::IndentStart(Position::zero()),
            EExpr::IndentEnd(_pos) => EExpr::IndentEnd(Position::zero()),
            EExpr::UnexpectedComma(_pos) => EExpr::UnexpectedComma(Position::zero()),
            EExpr::UnexpectedTopLevelExpr(_pos) => EExpr::UnexpectedTopLevelExpr(Position::zero()),
            EExpr::StmtAfterExpr(_pos) => EExpr::StmtAfterExpr(Position::zero()),
            EExpr::RecordUpdateAccumulator(_) => EExpr::RecordUpdateAccumulator(Region::zero()),
            EExpr::RecordBuilderAccumulator(_) => EExpr::RecordBuilderAccumulator(Region::zero()),
        }
    }
}

impl<'a> RemoveSpaces<'a> for EList<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match self {
            EList::Open(_pos) => EList::Open(Position::zero()),
            EList::End(_pos) => EList::End(Position::zero()),
            EList::Space(inner_err, _pos) => EList::Space(*inner_err, Position::zero()),
            EList::Expr(inner_err, _pos) => EList::Expr(
                arena.alloc(inner_err.remove_spaces(arena)),
                Position::zero(),
            ),
        }
    }
}

impl<'a> RemoveSpaces<'a> for EString<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match self {
            EString::Open(_) => EString::Open(Position::zero()),
            EString::CodePtOpen(_) => EString::CodePtOpen(Position::zero()),
            EString::CodePtEnd(_) => EString::CodePtEnd(Position::zero()),
            EString::InvalidSingleQuote(inner, _) => {
                EString::InvalidSingleQuote(*inner, Position::zero())
            }
            EString::Space(inner, _) => EString::Space(*inner, Position::zero()),
            EString::EndlessSingleLine(_) => EString::EndlessSingleLine(Position::zero()),
            EString::EndlessMultiLine(_) => EString::EndlessMultiLine(Position::zero()),
            EString::EndlessSingleQuote(_) => EString::EndlessSingleQuote(Position::zero()),
            EString::UnknownEscape(_) => EString::UnknownEscape(Position::zero()),
            EString::Format(inner, _) => {
                EString::Format(arena.alloc(inner.remove_spaces(arena)), Position::zero())
            }
            EString::FormatEnd(_) => EString::FormatEnd(Position::zero()),
            EString::MultilineInsufficientIndent(_) => {
                EString::MultilineInsufficientIndent(Position::zero())
            }
            EString::ExpectedDoubleQuoteGotSingleQuote(_) => {
                EString::ExpectedDoubleQuoteGotSingleQuote(Position::zero())
            }
        }
    }
}

impl<'a> RemoveSpaces<'a> for EClosure<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match self {
            EClosure::Space(inner_err, _) => EClosure::Space(*inner_err, Position::zero()),
            EClosure::Start(_) => EClosure::Start(Position::zero()),
            EClosure::Arrow(_) => EClosure::Arrow(Position::zero()),
            EClosure::Comma(_) => EClosure::Comma(Position::zero()),
            EClosure::Arg(_) => EClosure::Arg(Position::zero()),
            EClosure::Pattern(inner_err, _) => {
                EClosure::Pattern(inner_err.remove_spaces(arena), Position::zero())
            }
            EClosure::Body(inner_err, _) => EClosure::Body(
                arena.alloc(inner_err.remove_spaces(arena)),
                Position::zero(),
            ),
            EClosure::IndentArrow(_) => EClosure::IndentArrow(Position::zero()),
            EClosure::IndentBody(_) => EClosure::IndentBody(Position::zero()),
            EClosure::IndentArg(_) => EClosure::IndentArg(Position::zero()),
        }
    }
}

impl<'a> RemoveSpaces<'a> for EInParens<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match self {
            EInParens::End(_) => EInParens::End(Position::zero()),
            EInParens::Open(_) => EInParens::Open(Position::zero()),
            EInParens::Empty(_) => EInParens::Empty(Position::zero()),
            EInParens::Expr(inner_err, _) => EInParens::Expr(
                arena.alloc(inner_err.remove_spaces(arena)),
                Position::zero(),
            ),
            EInParens::Space(inner_err, _) => EInParens::Space(*inner_err, Position::zero()),
        }
    }
}

impl<'a> RemoveSpaces<'a> for ERecord<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match self {
            ERecord::End(_) => ERecord::End(Position::zero()),
            ERecord::Open(_) => ERecord::Open(Position::zero()),
            ERecord::Field(_pos) => ERecord::Field(Position::zero()),
            ERecord::Colon(_) => ERecord::Colon(Position::zero()),
            ERecord::QuestionMark(_) => ERecord::QuestionMark(Position::zero()),
            ERecord::Arrow(_) => ERecord::Arrow(Position::zero()),
            ERecord::Ampersand(_) => ERecord::Ampersand(Position::zero()),
            ERecord::Expr(inner_err, _) => ERecord::Expr(
                arena.alloc(inner_err.remove_spaces(arena)),
                Position::zero(),
            ),
            ERecord::Space(inner_err, _) => ERecord::Space(*inner_err, Position::zero()),
            ERecord::Prefix(_) => ERecord::Prefix(Position::zero()),
        }
    }
}

impl<'a> RemoveSpaces<'a> for EPattern<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match self {
            EPattern::Record(inner_err, _) => {
                EPattern::Record(inner_err.remove_spaces(arena), Position::zero())
            }
            EPattern::List(inner_err, _) => {
                EPattern::List(inner_err.remove_spaces(arena), Position::zero())
            }
            EPattern::AsKeyword(_) => EPattern::AsKeyword(Position::zero()),
            EPattern::AsIdentifier(_) => EPattern::AsIdentifier(Position::zero()),
            EPattern::Underscore(_) => EPattern::Underscore(Position::zero()),
            EPattern::NotAPattern(_) => EPattern::NotAPattern(Position::zero()),
            EPattern::Start(_) => EPattern::Start(Position::zero()),
            EPattern::End(_) => EPattern::End(Position::zero()),
            EPattern::Space(inner_err, _) => EPattern::Space(*inner_err, Position::zero()),
            EPattern::PInParens(inner_err, _) => {
                EPattern::PInParens(inner_err.remove_spaces(arena), Position::zero())
            }
            EPattern::NumLiteral(inner_err, _) => {
                EPattern::NumLiteral(inner_err.clone(), Position::zero())
            }
            EPattern::IndentStart(_) => EPattern::IndentStart(Position::zero()),
            EPattern::IndentEnd(_) => EPattern::IndentEnd(Position::zero()),
            EPattern::AsIndentStart(_) => EPattern::AsIndentStart(Position::zero()),
            EPattern::AccessorFunction(_) => EPattern::AccessorFunction(Position::zero()),
        }
    }
}

impl<'a> RemoveSpaces<'a> for EImport<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match self {
            EImport::Import(_) => EImport::Import(Position::zero()),
            EImport::IndentStart(_) => EImport::IndentStart(Position::zero()),
            EImport::PackageShorthand(_) => EImport::PackageShorthand(Position::zero()),
            EImport::PackageShorthandDot(_) => EImport::PackageShorthandDot(Position::zero()),
            EImport::ModuleName(_) => EImport::ModuleName(Position::zero()),
            EImport::Params(inner_err, _) => {
                EImport::Params(inner_err.remove_spaces(arena), Position::zero())
            }
            EImport::IndentAs(_) => EImport::IndentAs(Position::zero()),
            EImport::As(_) => EImport::As(Position::zero()),
            EImport::IndentAlias(_) => EImport::IndentAlias(Position::zero()),
            EImport::Alias(_) => EImport::Alias(Position::zero()),
            EImport::LowercaseAlias(_) => EImport::LowercaseAlias(Region::zero()),
            EImport::IndentExposing(_) => EImport::IndentExposing(Position::zero()),
            EImport::Exposing(_) => EImport::Exposing(Position::zero()),
            EImport::ExposingListStart(_) => EImport::ExposingListStart(Position::zero()),
            EImport::ExposedName(_) => EImport::ExposedName(Position::zero()),
            EImport::ExposingListEnd(_) => EImport::ExposingListEnd(Position::zero()),
            EImport::IndentIngestedPath(_) => EImport::IndentIngestedPath(Position::zero()),
            EImport::IngestedPath(_) => EImport::IngestedPath(Position::zero()),
            EImport::IndentIngestedName(_) => EImport::IndentIngestedName(Position::zero()),
            EImport::IngestedName(_) => EImport::IngestedName(Position::zero()),
            EImport::IndentColon(_) => EImport::IndentColon(Position::zero()),
            EImport::Colon(_) => EImport::Colon(Position::zero()),
            EImport::IndentAnnotation(_) => EImport::IndentAnnotation(Position::zero()),
            EImport::Annotation(inner_err, _) => {
                EImport::Annotation(inner_err.remove_spaces(arena), Position::zero())
            }
            EImport::Space(inner_err, _) => EImport::Space(*inner_err, Position::zero()),
            EImport::EndNewline(_) => EImport::EndNewline(Position::zero()),
        }
    }
}

impl<'a> RemoveSpaces<'a> for EType<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match self {
            EType::Space(inner_err, _) => EType::Space(*inner_err, Position::zero()),
            EType::UnderscoreSpacing(_) => EType::UnderscoreSpacing(Position::zero()),
            EType::TRecord(inner_err, _) => {
                EType::TRecord(inner_err.remove_spaces(arena), Position::zero())
            }
            EType::TTagUnion(inner_err, _) => {
                EType::TTagUnion(inner_err.remove_spaces(arena), Position::zero())
            }
            EType::TInParens(inner_err, _) => {
                EType::TInParens(inner_err.remove_spaces(arena), Position::zero())
            }
            EType::TApply(inner_err, _) => {
                EType::TApply(inner_err.remove_spaces(arena), Position::zero())
            }
            EType::TInlineAlias(inner_err, _) => {
                EType::TInlineAlias(inner_err.remove_spaces(arena), Position::zero())
            }
            EType::TBadTypeVariable(_) => EType::TBadTypeVariable(Position::zero()),
            EType::TWildcard(_) => EType::TWildcard(Position::zero()),
            EType::TInferred(_) => EType::TInferred(Position::zero()),
            EType::TStart(_) => EType::TStart(Position::zero()),
            EType::TEnd(_) => EType::TEnd(Position::zero()),
            EType::TFunctionArgument(_) => EType::TFunctionArgument(Position::zero()),
            EType::TWhereBar(_) => EType::TWhereBar(Position::zero()),
            EType::TImplementsClause(_) => EType::TImplementsClause(Position::zero()),
            EType::TAbilityImpl(inner_err, _) => {
                EType::TAbilityImpl(inner_err.remove_spaces(arena), Position::zero())
            }
            EType::TIndentStart(_) => EType::TIndentStart(Position::zero()),
            EType::TIndentEnd(_) => EType::TIndentEnd(Position::zero()),
            EType::TAsIndentStart(_) => EType::TAsIndentStart(Position::zero()),
        }
    }
}

impl<'a> RemoveSpaces<'a> for EImportParams<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match self {
            EImportParams::Indent(_) => EImportParams::Indent(Position::zero()),
            EImportParams::Record(inner_err, _) => {
                EImportParams::Record(inner_err.remove_spaces(arena), Position::zero())
            }
            EImportParams::RecordUpdateFound(_) => EImportParams::RecordUpdateFound(Region::zero()),
            EImportParams::RecordApplyFound(_) => EImportParams::RecordApplyFound(Region::zero()),
            EImportParams::Space(inner_err, _) => {
                EImportParams::Space(*inner_err, Position::zero())
            }
            EImportParams::RecordBuilderFound(_) => {
                EImportParams::RecordBuilderFound(Region::zero())
            }
        }
    }
}

impl<'a> RemoveSpaces<'a> for PInParens<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match self {
            PInParens::Empty(_) => PInParens::Empty(Position::zero()),
            PInParens::End(_) => PInParens::End(Position::zero()),
            PInParens::Open(_) => PInParens::Open(Position::zero()),
            PInParens::Pattern(inner_err, _) => PInParens::Pattern(
                arena.alloc(inner_err.remove_spaces(arena)),
                Position::zero(),
            ),
            PInParens::Space(inner_err, _) => PInParens::Space(*inner_err, Position::zero()),
        }
    }
}

impl<'a> RemoveSpaces<'a> for ETypeAbilityImpl<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match self {
            ETypeAbilityImpl::End(_) => ETypeAbilityImpl::End(Position::zero()),
            ETypeAbilityImpl::Open(_) => ETypeAbilityImpl::Open(Position::zero()),
            ETypeAbilityImpl::Field(_) => ETypeAbilityImpl::Field(Position::zero()),
            ETypeAbilityImpl::Colon(_) => ETypeAbilityImpl::Colon(Position::zero()),
            ETypeAbilityImpl::Arrow(_) => ETypeAbilityImpl::Arrow(Position::zero()),
            ETypeAbilityImpl::Optional(_) => ETypeAbilityImpl::Optional(Position::zero()),
            ETypeAbilityImpl::Type(inner_err, _) => ETypeAbilityImpl::Type(
                arena.alloc(inner_err.remove_spaces(arena)),
                Position::zero(),
            ),
            ETypeAbilityImpl::Space(inner_err, _) => {
                ETypeAbilityImpl::Space(*inner_err, Position::zero())
            }
            ETypeAbilityImpl::QuestionMark(_) => ETypeAbilityImpl::QuestionMark(Position::zero()),
            ETypeAbilityImpl::Ampersand(_) => ETypeAbilityImpl::Ampersand(Position::zero()),
            ETypeAbilityImpl::Expr(inner_err, _) => ETypeAbilityImpl::Expr(
                arena.alloc(inner_err.remove_spaces(arena)),
                Position::zero(),
            ),
            ETypeAbilityImpl::IndentBar(_) => ETypeAbilityImpl::IndentBar(Position::zero()),
            ETypeAbilityImpl::IndentAmpersand(_) => {
                ETypeAbilityImpl::IndentAmpersand(Position::zero())
            }
            ETypeAbilityImpl::Prefix(_) => ETypeAbilityImpl::Prefix(Position::zero()),
        }
    }
}

impl<'a> RemoveSpaces<'a> for ETypeInlineAlias {
    fn remove_spaces(&self, _arena: &'a Bump) -> Self {
        match self {
            ETypeInlineAlias::NotAnAlias(_pos) => ETypeInlineAlias::NotAnAlias(Position::zero()),
            ETypeInlineAlias::Qualified(_pos) => ETypeInlineAlias::Qualified(Position::zero()),
            ETypeInlineAlias::ArgumentNotLowercase(_pos) => {
                ETypeInlineAlias::ArgumentNotLowercase(Position::zero())
            }
        }
    }
}

impl<'a> RemoveSpaces<'a> for ETypeApply {
    fn remove_spaces(&self, _arena: &'a Bump) -> Self {
        match self {
            ETypeApply::StartNotUppercase(_) => ETypeApply::StartNotUppercase(Position::zero()),
            ETypeApply::End(_) => ETypeApply::End(Position::zero()),
            ETypeApply::Space(inner_err, _) => ETypeApply::Space(*inner_err, Position::zero()),
            ETypeApply::DoubleDot(_) => ETypeApply::DoubleDot(Position::zero()),
            ETypeApply::TrailingDot(_) => ETypeApply::TrailingDot(Position::zero()),
            ETypeApply::StartIsNumber(_) => ETypeApply::StartIsNumber(Position::zero()),
        }
    }
}

impl<'a> RemoveSpaces<'a> for ETypeInParens<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match self {
            ETypeInParens::Empty(_) => ETypeInParens::Empty(Position::zero()),
            ETypeInParens::End(_) => ETypeInParens::End(Position::zero()),
            ETypeInParens::Open(_) => ETypeInParens::Open(Position::zero()),
            ETypeInParens::Type(inner_err, _) => ETypeInParens::Type(
                arena.alloc(inner_err.remove_spaces(arena)),
                Position::zero(),
            ),
            ETypeInParens::Space(inner_err, _) => {
                ETypeInParens::Space(*inner_err, Position::zero())
            }
            ETypeInParens::IndentOpen(_) => ETypeInParens::IndentOpen(Position::zero()),
            ETypeInParens::IndentEnd(_) => ETypeInParens::IndentEnd(Position::zero()),
        }
    }
}

impl<'a> RemoveSpaces<'a> for ETypeTagUnion<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match self {
            ETypeTagUnion::End(_) => ETypeTagUnion::End(Position::zero()),
            ETypeTagUnion::Open(_) => ETypeTagUnion::Open(Position::zero()),
            ETypeTagUnion::Type(inner_err, _) => ETypeTagUnion::Type(
                arena.alloc(inner_err.remove_spaces(arena)),
                Position::zero(),
            ),
            ETypeTagUnion::Space(inner_err, _) => {
                ETypeTagUnion::Space(*inner_err, Position::zero())
            }
        }
    }
}

impl<'a> RemoveSpaces<'a> for ETypeRecord<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match self {
            ETypeRecord::End(_) => ETypeRecord::End(Position::zero()),
            ETypeRecord::Open(_) => ETypeRecord::Open(Position::zero()),
            ETypeRecord::Field(_) => ETypeRecord::Field(Position::zero()),
            ETypeRecord::Colon(_) => ETypeRecord::Colon(Position::zero()),
            ETypeRecord::Optional(_) => ETypeRecord::Optional(Position::zero()),
            ETypeRecord::Type(inner_err, _) => ETypeRecord::Type(
                arena.alloc(inner_err.remove_spaces(arena)),
                Position::zero(),
            ),
            ETypeRecord::Space(inner_err, _) => ETypeRecord::Space(*inner_err, Position::zero()),
            ETypeRecord::IndentOpen(_) => ETypeRecord::IndentOpen(Position::zero()),
            ETypeRecord::IndentColon(_) => ETypeRecord::IndentColon(Position::zero()),
            ETypeRecord::IndentOptional(_) => ETypeRecord::IndentOptional(Position::zero()),
            ETypeRecord::IndentEnd(_) => ETypeRecord::IndentEnd(Position::zero()),
        }
    }
}

impl<'a> RemoveSpaces<'a> for PRecord<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match self {
            PRecord::End(_) => PRecord::End(Position::zero()),
            PRecord::Open(_) => PRecord::Open(Position::zero()),
            PRecord::Field(_) => PRecord::Field(Position::zero()),
            PRecord::Colon(_) => PRecord::Colon(Position::zero()),
            PRecord::Optional(_) => PRecord::Optional(Position::zero()),
            PRecord::Pattern(inner_err, _) => PRecord::Pattern(
                arena.alloc(inner_err.remove_spaces(arena)),
                Position::zero(),
            ),
            PRecord::Expr(inner_err, _) => PRecord::Expr(
                arena.alloc(inner_err.remove_spaces(arena)),
                Position::zero(),
            ),
            PRecord::Space(inner_err, _) => PRecord::Space(*inner_err, Position::zero()),
        }
    }
}

impl<'a> RemoveSpaces<'a> for PList<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match self {
            PList::End(_) => PList::End(Position::zero()),
            PList::Open(_) => PList::Open(Position::zero()),
            PList::Rest(_) => PList::Rest(Position::zero()),
            PList::Pattern(inner_err, _) => PList::Pattern(
                arena.alloc(inner_err.remove_spaces(arena)),
                Position::zero(),
            ),
            PList::Space(inner_err, _) => PList::Space(*inner_err, Position::zero()),
        }
    }
}

impl<'a> RemoveSpaces<'a> for EExpect<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match self {
            EExpect::Space(inner_err, _) => EExpect::Space(*inner_err, Position::zero()),
            EExpect::Dbg(_) => EExpect::Dbg(Position::zero()),
            EExpect::Expect(_) => EExpect::Expect(Position::zero()),
            EExpect::Condition(inner_err, _) => EExpect::Condition(
                arena.alloc(inner_err.remove_spaces(arena)),
                Position::zero(),
            ),
            EExpect::Continuation(inner_err, _) => EExpect::Continuation(
                arena.alloc(inner_err.remove_spaces(arena)),
                Position::zero(),
            ),
            EExpect::IndentCondition(_) => EExpect::IndentCondition(Position::zero()),
        }
    }
}
impl<'a> RemoveSpaces<'a> for EIf<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match self {
            EIf::Space(inner_err, _) => EIf::Space(*inner_err, Position::zero()),
            EIf::If(_) => EIf::If(Position::zero()),
            EIf::Then(_) => EIf::Then(Position::zero()),
            EIf::Else(_) => EIf::Else(Position::zero()),
            EIf::Condition(inner_err, _) => EIf::Condition(
                arena.alloc(inner_err.remove_spaces(arena)),
                Position::zero(),
            ),
            EIf::ThenBranch(inner_err, _) => EIf::ThenBranch(
                arena.alloc(inner_err.remove_spaces(arena)),
                Position::zero(),
            ),
            EIf::ElseBranch(inner_err, _) => EIf::ElseBranch(
                arena.alloc(inner_err.remove_spaces(arena)),
                Position::zero(),
            ),
            EIf::IndentCondition(_) => EIf::IndentCondition(Position::zero()),
            EIf::IndentIf(_) => EIf::IndentIf(Position::zero()),
            EIf::IndentThenToken(_) => EIf::IndentThenToken(Position::zero()),
            EIf::IndentElseToken(_) => EIf::IndentElseToken(Position::zero()),
            EIf::IndentThenBranch(_) => EIf::IndentThenBranch(Position::zero()),
            EIf::IndentElseBranch(_) => EIf::IndentElseBranch(Position::zero()),
        }
    }
}

impl<'a> RemoveSpaces<'a> for EWhen<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match self {
            EWhen::Space(inner_err, _) => EWhen::Space(*inner_err, Position::zero()),
            EWhen::When(_) => EWhen::When(Position::zero()),
            EWhen::Is(_) => EWhen::Is(Position::zero()),
            EWhen::Pattern(inner_err, _) => {
                EWhen::Pattern(inner_err.remove_spaces(arena), Position::zero())
            }
            EWhen::Arrow(_) => EWhen::Arrow(Position::zero()),
            EWhen::Bar(_) => EWhen::Bar(Position::zero()),
            EWhen::IfToken(_) => EWhen::IfToken(Position::zero()),
            EWhen::IfGuard(inner_err, _) => EWhen::IfGuard(
                arena.alloc(inner_err.remove_spaces(arena)),
                Position::zero(),
            ),
            EWhen::Condition(inner_err, _) => EWhen::Condition(
                arena.alloc(inner_err.remove_spaces(arena)),
                Position::zero(),
            ),
            EWhen::Branch(inner_err, _) => EWhen::Branch(
                arena.alloc(inner_err.remove_spaces(arena)),
                Position::zero(),
            ),
            EWhen::IndentCondition(_) => EWhen::IndentCondition(Position::zero()),
            EWhen::IndentPattern(_) => EWhen::IndentPattern(Position::zero()),
            EWhen::IndentArrow(_) => EWhen::IndentArrow(Position::zero()),
            EWhen::IndentBranch(_) => EWhen::IndentBranch(Position::zero()),
            EWhen::IndentIfGuard(_) => EWhen::IndentIfGuard(Position::zero()),
            EWhen::PatternAlignment(_alignment, _) => EWhen::PatternAlignment(0, Position::zero()),
        }
    }
}

impl<'a> RemoveSpaces<'a> for EAbility<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match self {
            EAbility::Space(inner_err, _) => EAbility::Space(*inner_err, Position::zero()),
            EAbility::Type(inner_err, _) => {
                EAbility::Type(inner_err.remove_spaces(arena), Position::zero())
            }
            EAbility::DemandAlignment(_alignment, _) => {
                EAbility::DemandAlignment(0, Position::zero())
            }
            EAbility::DemandName(_) => EAbility::DemandName(Position::zero()),
            EAbility::DemandColon(_) => EAbility::DemandColon(Position::zero()),
        }
    }
}

impl<'a> RemoveSpaces<'a> for EGeneratesWith {
    fn remove_spaces(&self, _arena: &'a Bump) -> Self {
        match self {
            EGeneratesWith::Open(_) => EGeneratesWith::Open(Position::zero()),
            EGeneratesWith::With(_) => EGeneratesWith::With(Position::zero()),
            EGeneratesWith::IndentWith(_) => EGeneratesWith::IndentWith(Position::zero()),
            EGeneratesWith::IndentListStart(_) => EGeneratesWith::IndentListStart(Position::zero()),
            EGeneratesWith::IndentListEnd(_) => EGeneratesWith::IndentListEnd(Position::zero()),
            EGeneratesWith::ListStart(_) => EGeneratesWith::ListStart(Position::zero()),
            EGeneratesWith::ListEnd(_) => EGeneratesWith::ListEnd(Position::zero()),
            EGeneratesWith::Identifier(_) => EGeneratesWith::Identifier(Position::zero()),
            EGeneratesWith::Space(inner_err, _) => {
                EGeneratesWith::Space(*inner_err, Position::zero())
            }
        }
    }
}

impl<'a> RemoveSpaces<'a> for EGenerates {
    fn remove_spaces(&self, _arena: &'a Bump) -> Self {
        match self {
            EGenerates::Open(_) => EGenerates::Open(Position::zero()),
            EGenerates::Generates(_) => EGenerates::Generates(Position::zero()),
            EGenerates::IndentGenerates(_) => EGenerates::IndentGenerates(Position::zero()),
            EGenerates::Identifier(_) => EGenerates::Identifier(Position::zero()),
            EGenerates::Space(inner_err, _) => EGenerates::Space(*inner_err, Position::zero()),
            EGenerates::IndentTypeStart(_) => EGenerates::IndentTypeStart(Position::zero()),
            EGenerates::IndentTypeEnd(_) => EGenerates::IndentTypeEnd(Position::zero()),
        }
    }
}

impl<'a> RemoveSpaces<'a> for EPackages<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match self {
            EPackages::Open(_) => EPackages::Open(Position::zero()),
            EPackages::Space(inner_err, _) => EPackages::Space(*inner_err, Position::zero()),
            EPackages::Packages(_) => EPackages::Packages(Position::zero()),
            EPackages::IndentPackages(_) => EPackages::IndentPackages(Position::zero()),
            EPackages::ListStart(_) => EPackages::ListStart(Position::zero()),
            EPackages::ListEnd(_) => EPackages::ListEnd(Position::zero()),
            EPackages::IndentListStart(_) => EPackages::IndentListStart(Position::zero()),
            EPackages::IndentListEnd(_) => EPackages::IndentListEnd(Position::zero()),
            EPackages::PackageEntry(inner_err, _) => {
                EPackages::PackageEntry(inner_err.remove_spaces(arena), Position::zero())
            }
        }
    }
}

impl<'a> RemoveSpaces<'a> for EHeader<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match self {
            EHeader::Provides(inner_err, _) => {
                EHeader::Provides(inner_err.remove_spaces(arena), Position::zero())
            }
            EHeader::Params(inner_err, _) => {
                EHeader::Params(inner_err.remove_spaces(arena), Position::zero())
            }
            EHeader::Exposes(inner_err, _) => {
                EHeader::Exposes(inner_err.remove_spaces(arena), Position::zero())
            }
            EHeader::Imports(inner_err, _) => {
                EHeader::Imports(inner_err.remove_spaces(arena), Position::zero())
            }
            EHeader::Requires(inner_err, _) => {
                EHeader::Requires(inner_err.remove_spaces(arena), Position::zero())
            }
            EHeader::Packages(inner_err, _) => {
                EHeader::Packages(inner_err.remove_spaces(arena), Position::zero())
            }
            EHeader::Generates(inner_err, _) => {
                EHeader::Generates(inner_err.remove_spaces(arena), Position::zero())
            }
            EHeader::GeneratesWith(inner_err, _) => {
                EHeader::GeneratesWith(inner_err.remove_spaces(arena), Position::zero())
            }
            EHeader::Space(inner_err, _) => EHeader::Space(*inner_err, Position::zero()),
            EHeader::Start(_) => EHeader::Start(Position::zero()),
            EHeader::ModuleName(_) => EHeader::ModuleName(Position::zero()),
            EHeader::AppName(inner_err, _) => {
                EHeader::AppName(inner_err.remove_spaces(arena), Position::zero())
            }
            EHeader::PackageName(inner_err, _) => {
                EHeader::PackageName(inner_err.remove_spaces(arena), Position::zero())
            }
            EHeader::PlatformName(inner_err, _) => {
                EHeader::PlatformName(inner_err.remove_spaces(arena), Position::zero())
            }
            EHeader::IndentStart(_) => EHeader::IndentStart(Position::zero()),
            EHeader::InconsistentModuleName(_) => EHeader::InconsistentModuleName(Region::zero()),
        }
    }
}

impl<'a> RemoveSpaces<'a> for EPackageName<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match self {
            EPackageName::BadPath(inner_err, _) => {
                EPackageName::BadPath(inner_err.remove_spaces(arena), Position::zero())
            }
            EPackageName::Escapes(_) => EPackageName::Escapes(Position::zero()),
            EPackageName::Multiline(_) => EPackageName::Multiline(Position::zero()),
        }
    }
}

impl<'a> RemoveSpaces<'a> for SyntaxError<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match self {
            SyntaxError::Unexpected(_) => SyntaxError::Unexpected(Region::zero()),
            SyntaxError::OutdentedTooFar => SyntaxError::OutdentedTooFar,
            SyntaxError::Eof(_) => SyntaxError::Eof(Region::zero()),
            SyntaxError::InvalidPattern => SyntaxError::InvalidPattern,
            SyntaxError::BadUtf8 => SyntaxError::BadUtf8,
            SyntaxError::ReservedKeyword(_) => SyntaxError::ReservedKeyword(Region::zero()),
            SyntaxError::ArgumentsBeforeEquals(_) => {
                SyntaxError::ArgumentsBeforeEquals(Region::zero())
            }
            SyntaxError::NotYetImplemented(text) => SyntaxError::NotYetImplemented(text.clone()),
            SyntaxError::Todo => SyntaxError::Todo,
            SyntaxError::Type(err) => SyntaxError::Type(err.remove_spaces(arena)),
            SyntaxError::Pattern(err) => SyntaxError::Pattern(err.remove_spaces(arena)),
            SyntaxError::Expr(err, _) => {
                SyntaxError::Expr(err.remove_spaces(arena), Position::zero())
            }
            SyntaxError::Header(err) => SyntaxError::Header(err.remove_spaces(arena)),
            SyntaxError::Space(inner_err) => SyntaxError::Space(*inner_err),
            SyntaxError::NotEndOfFile(_) => SyntaxError::NotEndOfFile(Position::zero()),
        }
    }
}

impl<'a> RemoveSpaces<'a> for EPackageEntry<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match self {
            EPackageEntry::BadPackage(inner_err, _) => {
                EPackageEntry::BadPackage(inner_err.remove_spaces(arena), Position::zero())
            }
            EPackageEntry::Shorthand(_) => EPackageEntry::Shorthand(Position::zero()),
            EPackageEntry::Colon(_) => EPackageEntry::Colon(Position::zero()),
            EPackageEntry::IndentPackage(_) => EPackageEntry::IndentPackage(Position::zero()),
            EPackageEntry::IndentPlatform(_) => EPackageEntry::IndentPlatform(Position::zero()),
            EPackageEntry::Platform(_) => EPackageEntry::Platform(Position::zero()),
            EPackageEntry::Space(inner_err, _) => {
                EPackageEntry::Space(*inner_err, Position::zero())
            }
        }
    }
}

impl<'a> RemoveSpaces<'a> for EProvides<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match self {
            EProvides::Provides(_) => EProvides::Provides(Position::zero()),
            EProvides::Open(_) => EProvides::Open(Position::zero()),
            EProvides::To(_) => EProvides::To(Position::zero()),
            EProvides::IndentProvides(_) => EProvides::IndentProvides(Position::zero()),
            EProvides::IndentTo(_) => EProvides::IndentTo(Position::zero()),
            EProvides::IndentListStart(_) => EProvides::IndentListStart(Position::zero()),
            EProvides::IndentPackage(_) => EProvides::IndentPackage(Position::zero()),
            EProvides::ListStart(_) => EProvides::ListStart(Position::zero()),
            EProvides::ListEnd(_) => EProvides::ListEnd(Position::zero()),
            EProvides::Identifier(_) => EProvides::Identifier(Position::zero()),
            EProvides::Package(inner_err, _) => {
                EProvides::Package(inner_err.remove_spaces(arena), Position::zero())
            }
            EProvides::Space(inner_err, _) => EProvides::Space(*inner_err, Position::zero()),
        }
    }
}

impl<'a> RemoveSpaces<'a> for EParams<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match self {
            EParams::Pattern(inner_err, _) => {
                EParams::Pattern(inner_err.remove_spaces(arena), Position::zero())
            }
            EParams::BeforeArrow(_) => EParams::BeforeArrow(Position::zero()),
            EParams::Arrow(_) => EParams::Arrow(Position::zero()),
            EParams::AfterArrow(_) => EParams::AfterArrow(Position::zero()),
            EParams::Space(inner_err, _) => EParams::Space(*inner_err, Position::zero()),
        }
    }
}

impl<'a> RemoveSpaces<'a> for EExposes {
    fn remove_spaces(&self, _arena: &'a Bump) -> Self {
        match self {
            EExposes::Exposes(_) => EExposes::Exposes(Position::zero()),
            EExposes::Open(_) => EExposes::Open(Position::zero()),
            EExposes::IndentExposes(_) => EExposes::IndentExposes(Position::zero()),
            EExposes::IndentListStart(_) => EExposes::IndentListStart(Position::zero()),
            EExposes::ListStart(_) => EExposes::ListStart(Position::zero()),
            EExposes::ListEnd(_) => EExposes::ListEnd(Position::zero()),
            EExposes::Identifier(_) => EExposes::Identifier(Position::zero()),
            EExposes::Space(inner_err, _) => EExposes::Space(*inner_err, Position::zero()),
        }
    }
}

impl<'a> RemoveSpaces<'a> for EImports {
    fn remove_spaces(&self, _arena: &'a Bump) -> Self {
        match self {
            EImports::Open(_) => EImports::Open(Position::zero()),
            EImports::Imports(_) => EImports::Imports(Position::zero()),
            EImports::IndentImports(_) => EImports::IndentImports(Position::zero()),
            EImports::IndentListStart(_) => EImports::IndentListStart(Position::zero()),
            EImports::IndentListEnd(_) => EImports::IndentListEnd(Position::zero()),
            EImports::ListStart(_) => EImports::ListStart(Position::zero()),
            EImports::ListEnd(_) => EImports::ListEnd(Position::zero()),
            EImports::Identifier(_) => EImports::Identifier(Position::zero()),
            EImports::ExposingDot(_) => EImports::ExposingDot(Position::zero()),
            EImports::ShorthandDot(_) => EImports::ShorthandDot(Position::zero()),
            EImports::Shorthand(_) => EImports::Shorthand(Position::zero()),
            EImports::ModuleName(_) => EImports::ModuleName(Position::zero()),
            EImports::Space(inner_err, _) => EImports::Space(*inner_err, Position::zero()),
            EImports::IndentSetStart(_) => EImports::IndentSetStart(Position::zero()),
            EImports::SetStart(_) => EImports::SetStart(Position::zero()),
            EImports::SetEnd(_) => EImports::SetEnd(Position::zero()),
            EImports::TypedIdent(_) => EImports::TypedIdent(Position::zero()),
            EImports::AsKeyword(_) => EImports::AsKeyword(Position::zero()),
            EImports::StrLiteral(_) => EImports::StrLiteral(Position::zero()),
        }
    }
}

impl<'a> RemoveSpaces<'a> for ERequires<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match self {
            ERequires::Requires(_) => ERequires::Requires(Position::zero()),
            ERequires::Open(_) => ERequires::Open(Position::zero()),
            ERequires::IndentRequires(_) => ERequires::IndentRequires(Position::zero()),
            ERequires::IndentListStart(_) => ERequires::IndentListStart(Position::zero()),
            ERequires::ListStart(_) => ERequires::ListStart(Position::zero()),
            ERequires::ListEnd(_) => ERequires::ListEnd(Position::zero()),
            ERequires::TypedIdent(inner_err, _) => {
                ERequires::TypedIdent(inner_err.remove_spaces(arena), Position::zero())
            }
            ERequires::Rigid(_) => ERequires::Rigid(Position::zero()),
            ERequires::Space(inner_err, _) => ERequires::Space(*inner_err, Position::zero()),
        }
    }
}

impl<'a> RemoveSpaces<'a> for ETypedIdent<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match self {
            ETypedIdent::Space(inner_err, _) => ETypedIdent::Space(*inner_err, Position::zero()),
            ETypedIdent::HasType(_) => ETypedIdent::HasType(Position::zero()),
            ETypedIdent::IndentHasType(_) => ETypedIdent::IndentHasType(Position::zero()),
            ETypedIdent::Name(_) => ETypedIdent::Name(Position::zero()),
            ETypedIdent::Type(inner_err, _) => {
                ETypedIdent::Type(inner_err.remove_spaces(arena), Position::zero())
            }
            ETypedIdent::IndentType(_) => ETypedIdent::IndentType(Position::zero()),
            ETypedIdent::Identifier(_) => ETypedIdent::Identifier(Position::zero()),
        }
    }
}
