use bumpalo::collections::{String, Vec};
use bumpalo::Bump;
use roc_module::called_via::{BinOp, UnaryOp};
use roc_region::all::{Loc, Position, Region};

use crate::{
    ast::{
        AbilityImpls, AbilityMember, AssignedField, Collection, Defs, Expr, FullAst, Header,
        Implements, ImplementsAbilities, ImplementsAbility, ImplementsClause, ImportAlias,
        ImportAsKeyword, ImportExposingKeyword, ImportedModuleName, IngestedFileAnnotation,
        IngestedFileImport, ModuleImport, ModuleImportParams, Pattern, PatternAs, Spaced, Spaces,
        SpacesBefore, StrLiteral, StrSegment, Tag, TypeAnnotation, TypeDef, TypeHeader, ValueDef,
        WhenBranch,
    },
    header::{
        AppHeader, ExposedName, ExposesKeyword, HostedHeader, ImportsEntry, ImportsKeyword,
        KeywordItem, ModuleHeader, ModuleName, ModuleParams, PackageEntry, PackageHeader,
        PackageKeyword, PackageName, PackagesKeyword, PlatformHeader, PlatformKeyword,
        PlatformRequires, ProvidesKeyword, ProvidesTo, RequiresKeyword, To, ToKeyword, TypedIdent,
    },
    ident::{BadIdent, UppercaseIdent},
    parser::{
        EAbility, EClosure, EExpect, EExposes, EExpr, EHeader, EIf, EImport, EImportParams,
        EImports, EInParens, EList, EPackageEntry, EPackageName, EPackages, EParams, EPattern,
        EProvides, ERecord, ERequires, EReturn, EString, EType, ETypeAbilityImpl, ETypeApply,
        ETypeInParens, ETypeInlineAlias, ETypeRecord, ETypeTagUnion, ETypedIdent, EWhen, PInParens,
        PList, PRecord, SyntaxError,
    },
};

/// Normalizes the ast to something that we _expect_ to be invariant under formatting.
///
/// Currently this consists of:
/// * Removing newlines
/// * Removing comments
/// * Removing parens in Exprs
/// * Normalizing string encoding
///
/// Long term, we actually want this transform to preserve comments (so we can assert they're maintained by formatting)
/// - but there are currently several bugs where they're _not_ preserved.
/// TODO: ensure formatting retains comments
pub trait Normalize<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self;
}

macro_rules! keywords {
    ($($name:ident),* $(,)?) => {
        $(
            impl<'a> Normalize<'a> for $name {
                fn normalize(&self, _arena: &'a Bump) -> Self {
                    *self
                }
            }
        )*
    }
}

keywords! {
    ExposesKeyword,
    ImportsKeyword,
    PackageKeyword,
    PackagesKeyword,
    RequiresKeyword,
    ProvidesKeyword,
    ToKeyword,
    PlatformKeyword,
}

impl<'a> Normalize<'a> for Defs<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        let mut defs = self.clone();

        defs.spaces.clear();
        defs.space_before.clear();
        defs.space_after.clear();

        for type_def in defs.type_defs.iter_mut() {
            *type_def = type_def.normalize(arena);
        }

        for value_def in defs.value_defs.iter_mut() {
            *value_def = value_def.normalize(arena);
        }

        for region_def in defs.regions.iter_mut() {
            *region_def = region_def.normalize(arena);
        }

        defs
    }
}

impl<'a, V: Normalize<'a>> Normalize<'a> for Spaces<'a, V> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        Spaces {
            before: &[],
            item: self.item.normalize(arena),
            after: &[],
        }
    }
}

impl<'a, V: Normalize<'a>> Normalize<'a> for SpacesBefore<'a, V> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        SpacesBefore {
            before: &[],
            item: self.item.normalize(arena),
        }
    }
}

impl<'a> Normalize<'a> for FullAst<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        FullAst {
            header: self.header.normalize(arena),
            defs: self.defs.normalize(arena),
        }
    }
}

impl<'a, K: Normalize<'a>, V: Normalize<'a>> Normalize<'a> for KeywordItem<'a, K, V> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        KeywordItem {
            keyword: self.keyword.normalize(arena),
            item: self.item.normalize(arena),
        }
    }
}

impl<'a> Normalize<'a> for ProvidesTo<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        ProvidesTo {
            provides_keyword: self.provides_keyword.normalize(arena),
            entries: self.entries.normalize(arena),
            types: self.types.normalize(arena),
            to_keyword: self.to_keyword.normalize(arena),
            to: self.to.normalize(arena),
        }
    }
}

impl<'a> Normalize<'a> for Header<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match self {
            Header::Module(header) => Header::Module(ModuleHeader {
                after_keyword: &[],
                params: header.params.normalize(arena),
                exposes: header.exposes.normalize(arena),
                interface_imports: header.interface_imports.normalize(arena),
            }),
            Header::App(header) => Header::App(AppHeader {
                before_provides: &[],
                provides: header.provides.normalize(arena),
                before_packages: &[],
                packages: header.packages.normalize(arena),
                old_imports: header.old_imports.normalize(arena),
                old_provides_to_new_package: header.old_provides_to_new_package.normalize(arena),
            }),
            Header::Package(header) => Header::Package(PackageHeader {
                before_exposes: &[],
                exposes: header.exposes.normalize(arena),
                before_packages: &[],
                packages: header.packages.normalize(arena),
            }),
            Header::Platform(header) => Header::Platform(PlatformHeader {
                before_name: &[],
                name: header.name.normalize(arena),
                requires: header.requires.normalize(arena),
                exposes: header.exposes.normalize(arena),
                packages: header.packages.normalize(arena),
                imports: header.imports.normalize(arena),
                provides: header.provides.normalize(arena),
            }),
            Header::Hosted(header) => Header::Hosted(HostedHeader {
                before_name: &[],
                name: header.name.normalize(arena),
                exposes: header.exposes.normalize(arena),
                imports: header.imports.normalize(arena),
            }),
        }
    }
}

impl<'a> Normalize<'a> for ModuleParams<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        ModuleParams {
            pattern: self.pattern.normalize(arena),
            before_arrow: &[],
            after_arrow: &[],
        }
    }
}

impl<'a> Normalize<'a> for Region {
    fn normalize(&self, _arena: &'a Bump) -> Self {
        Region::zero()
    }
}

impl<'a> Normalize<'a> for &'a str {
    fn normalize(&self, _arena: &'a Bump) -> Self {
        self
    }
}

impl<'a, T: Normalize<'a> + Copy> Normalize<'a> for Spaced<'a, T> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match *self {
            Spaced::Item(a) => Spaced::Item(a.normalize(arena)),
            Spaced::SpaceBefore(a, _) => a.normalize(arena),
            Spaced::SpaceAfter(a, _) => a.normalize(arena),
        }
    }
}

impl<'a> Normalize<'a> for ExposedName<'a> {
    fn normalize(&self, _arena: &'a Bump) -> Self {
        *self
    }
}

impl<'a> Normalize<'a> for ModuleName<'a> {
    fn normalize(&self, _arena: &'a Bump) -> Self {
        *self
    }
}

impl<'a> Normalize<'a> for PackageName<'a> {
    fn normalize(&self, _arena: &'a Bump) -> Self {
        *self
    }
}

impl<'a> Normalize<'a> for To<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match *self {
            To::ExistingPackage(a) => To::ExistingPackage(a),
            To::NewPackage(a) => To::NewPackage(a.normalize(arena)),
        }
    }
}

impl<'a> Normalize<'a> for TypedIdent<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        TypedIdent {
            ident: self.ident.normalize(arena),
            spaces_before_colon: &[],
            ann: self.ann.normalize(arena),
        }
    }
}

impl<'a> Normalize<'a> for PlatformRequires<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        PlatformRequires {
            rigids: self.rigids.normalize(arena),
            signatures: self.signatures.map_items(arena, |x| x.normalize(arena)),
        }
    }
}

impl<'a> Normalize<'a> for UppercaseIdent<'a> {
    fn normalize(&self, _arena: &'a Bump) -> Self {
        *self
    }
}

impl<'a> Normalize<'a> for PackageEntry<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        PackageEntry {
            shorthand: self.shorthand,
            spaces_after_shorthand: &[],
            platform_marker: match self.platform_marker {
                Some(_) => Some(&[]),
                None => None,
            },
            package_name: self.package_name.normalize(arena),
        }
    }
}

impl<'a> Normalize<'a> for ImportsEntry<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match *self {
            ImportsEntry::Module(a, b) => ImportsEntry::Module(a, b.normalize(arena)),
            ImportsEntry::Package(a, b, c) => ImportsEntry::Package(a, b, c.normalize(arena)),
            ImportsEntry::IngestedFile(a, b) => ImportsEntry::IngestedFile(a, b.normalize(arena)),
        }
    }
}

impl<'a, T: Normalize<'a>> Normalize<'a> for Option<T> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        self.as_ref().map(|a| a.normalize(arena))
    }
}

impl<'a, T: Normalize<'a> + std::fmt::Debug> Normalize<'a> for Loc<T> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        let res = self.value.normalize(arena);
        Loc::at(Region::zero(), res)
    }
}

impl<'a, A: Normalize<'a>, B: Normalize<'a>> Normalize<'a> for (A, B) {
    fn normalize(&self, arena: &'a Bump) -> Self {
        (self.0.normalize(arena), self.1.normalize(arena))
    }
}

impl<'a, T: Normalize<'a>> Normalize<'a> for Collection<'a, T> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        let mut items = Vec::with_capacity_in(self.items.len(), arena);
        for item in self.items {
            items.push(item.normalize(arena));
        }
        Collection::with_items(items.into_bump_slice())
    }
}

impl<'a, T: Normalize<'a> + std::fmt::Debug> Normalize<'a> for &'a [T] {
    fn normalize(&self, arena: &'a Bump) -> Self {
        let mut items = Vec::with_capacity_in(self.len(), arena);
        for item in *self {
            let res = item.normalize(arena);
            items.push(res);
        }
        items.into_bump_slice()
    }
}

impl<'a> Normalize<'a> for UnaryOp {
    fn normalize(&self, _arena: &'a Bump) -> Self {
        *self
    }
}

impl<'a> Normalize<'a> for BinOp {
    fn normalize(&self, _arena: &'a Bump) -> Self {
        *self
    }
}

impl<'a, T: Normalize<'a>> Normalize<'a> for &'a T {
    fn normalize(&self, arena: &'a Bump) -> Self {
        arena.alloc((*self).normalize(arena))
    }
}

impl<'a> Normalize<'a> for TypeDef<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        use TypeDef::*;

        match *self {
            Alias {
                header: TypeHeader { name, vars },
                ann,
            } => Alias {
                header: TypeHeader {
                    name: name.normalize(arena),
                    vars: vars.normalize(arena),
                },
                ann: ann.normalize(arena),
            },
            Opaque {
                header: TypeHeader { name, vars },
                typ,
                derived,
            } => Opaque {
                header: TypeHeader {
                    name: name.normalize(arena),
                    vars: vars.normalize(arena),
                },
                typ: typ.normalize(arena),
                derived: derived.normalize(arena),
            },
            Ability {
                header: TypeHeader { name, vars },
                loc_implements: loc_has,
                members,
            } => Ability {
                header: TypeHeader {
                    name: name.normalize(arena),
                    vars: vars.normalize(arena),
                },
                loc_implements: loc_has.normalize(arena),
                members: members.normalize(arena),
            },
        }
    }
}

impl<'a> Normalize<'a> for ValueDef<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        use ValueDef::*;

        match *self {
            Annotation(a, b) => Annotation(a.normalize(arena), b.normalize(arena)),
            Body(a, b) => {
                let a = a.normalize(arena);
                let b = b.normalize(arena);

                let is_unit_assignment = if let Pattern::RecordDestructure(collection) = a.value {
                    collection.is_empty()
                } else {
                    false
                };

                if is_unit_assignment {
                    Stmt(arena.alloc(b))
                } else {
                    Body(arena.alloc(a), arena.alloc(b))
                }
            }
            AnnotatedBody {
                ann_pattern,
                ann_type,
                lines_between: _,
                body_pattern,
                body_expr,
            } => AnnotatedBody {
                ann_pattern: arena.alloc(ann_pattern.normalize(arena)),
                ann_type: arena.alloc(ann_type.normalize(arena)),
                lines_between: &[],
                body_pattern: arena.alloc(body_pattern.normalize(arena)),
                body_expr: arena.alloc(body_expr.normalize(arena)),
            },
            Dbg {
                condition,
                preceding_comment: _,
            } => Dbg {
                condition: arena.alloc(condition.normalize(arena)),
                preceding_comment: Region::zero(),
            },
            Expect {
                condition,
                preceding_comment: _,
            } => Expect {
                condition: arena.alloc(condition.normalize(arena)),
                preceding_comment: Region::zero(),
            },
            ModuleImport(module_import) => ModuleImport(module_import.normalize(arena)),
            IngestedFileImport(ingested_file_import) => {
                IngestedFileImport(ingested_file_import.normalize(arena))
            }
            Stmt(loc_expr) => Stmt(arena.alloc(loc_expr.normalize(arena))),
            StmtAfterExpr => StmtAfterExpr,
        }
    }
}

impl<'a> Normalize<'a> for ModuleImport<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        ModuleImport {
            before_name: &[],
            name: self.name.normalize(arena),
            params: self.params.normalize(arena),
            alias: self.alias.normalize(arena),
            exposed: self.exposed.normalize(arena),
        }
    }
}

impl<'a> Normalize<'a> for ModuleImportParams<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        ModuleImportParams {
            before: &[],
            params: self.params.normalize(arena),
        }
    }
}

impl<'a> Normalize<'a> for IngestedFileImport<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        IngestedFileImport {
            before_path: &[],
            path: self.path.normalize(arena),
            name: self.name.normalize(arena),
            annotation: self.annotation.normalize(arena),
        }
    }
}

impl<'a> Normalize<'a> for ImportedModuleName<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        ImportedModuleName {
            package: self.package.normalize(arena),
            name: self.name.normalize(arena),
        }
    }
}

impl<'a> Normalize<'a> for ImportAlias<'a> {
    fn normalize(&self, _arena: &'a Bump) -> Self {
        *self
    }
}

impl<'a> Normalize<'a> for ImportAsKeyword {
    fn normalize(&self, _arena: &'a Bump) -> Self {
        *self
    }
}

impl<'a> Normalize<'a> for ImportExposingKeyword {
    fn normalize(&self, _arena: &'a Bump) -> Self {
        *self
    }
}

impl<'a> Normalize<'a> for IngestedFileAnnotation<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        IngestedFileAnnotation {
            before_colon: &[],
            annotation: self.annotation.normalize(arena),
        }
    }
}

impl<'a> Normalize<'a> for Implements<'a> {
    fn normalize(&self, _arena: &'a Bump) -> Self {
        Implements::Implements
    }
}

impl<'a> Normalize<'a> for AbilityMember<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        AbilityMember {
            name: self.name.normalize(arena),
            typ: self.typ.normalize(arena),
        }
    }
}

impl<'a> Normalize<'a> for WhenBranch<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        WhenBranch {
            patterns: self.patterns.normalize(arena),
            value: self.value.normalize(arena),
            guard: self.guard.normalize(arena),
        }
    }
}

impl<'a, T: Normalize<'a> + Copy + std::fmt::Debug> Normalize<'a> for AssignedField<'a, T> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match *self {
            AssignedField::RequiredValue(a, _, c) => AssignedField::RequiredValue(
                a.normalize(arena),
                arena.alloc([]),
                arena.alloc(c.normalize(arena)),
            ),
            AssignedField::OptionalValue(a, _, c) => AssignedField::OptionalValue(
                a.normalize(arena),
                arena.alloc([]),
                arena.alloc(c.normalize(arena)),
            ),
            AssignedField::IgnoredValue(a, _, c) => AssignedField::IgnoredValue(
                a.normalize(arena),
                arena.alloc([]),
                arena.alloc(c.normalize(arena)),
            ),
            AssignedField::LabelOnly(a) => AssignedField::LabelOnly(a.normalize(arena)),
            AssignedField::SpaceBefore(a, _) => a.normalize(arena),
            AssignedField::SpaceAfter(a, _) => a.normalize(arena),
        }
    }
}

impl<'a> Normalize<'a> for StrLiteral<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match *self {
            StrLiteral::PlainLine(t) => StrLiteral::PlainLine(t),
            StrLiteral::Line(t) => {
                let mut new_segments = Vec::new_in(arena);
                let mut last_text = String::new_in(arena);

                normalize_str_segments(arena, t, &mut last_text, &mut new_segments);
                if !last_text.is_empty() {
                    new_segments.push(StrSegment::Plaintext(last_text.into_bump_str()));
                }

                normalize_str_line(new_segments)
            }
            StrLiteral::Block(t) => {
                let mut new_segments = Vec::new_in(arena);
                let mut last_text = String::new_in(arena);
                for line in t {
                    normalize_str_segments(arena, line, &mut last_text, &mut new_segments);
                }
                if !last_text.is_empty() {
                    new_segments.push(StrSegment::Plaintext(last_text.into_bump_str()));
                }

                normalize_str_line(new_segments)
            }
        }
    }
}

fn normalize_str_line<'a>(new_segments: Vec<'a, StrSegment<'a>>) -> StrLiteral<'a> {
    if new_segments.len() == 1 {
        if let StrSegment::Plaintext(t) = new_segments[0] {
            return StrLiteral::PlainLine(t);
        }
    }

    StrLiteral::Line(new_segments.into_bump_slice())
}

fn normalize_str_segments<'a>(
    arena: &'a Bump,
    segments: &[StrSegment<'a>],
    last_text: &mut String<'a>,
    new_segments: &mut Vec<'a, StrSegment<'a>>,
) {
    for segment in segments.iter() {
        match segment {
            StrSegment::Plaintext(t) => {
                last_text.push_str(t);
            }
            StrSegment::Unicode(t) => {
                let hex_code: &str = t.value;
                if let Some(c) = u32::from_str_radix(hex_code, 16)
                    .ok()
                    .and_then(char::from_u32)
                {
                    last_text.push(c);
                } else {
                    if !last_text.is_empty() {
                        let text = std::mem::replace(last_text, String::new_in(arena));
                        new_segments.push(StrSegment::Plaintext(text.into_bump_str()));
                    }
                    new_segments.push(StrSegment::Unicode(Loc::at_zero(t.value)));
                }
            }
            StrSegment::EscapedChar(c) => {
                last_text.push(c.unescape());
            }
            StrSegment::Interpolated(e) => {
                if !last_text.is_empty() {
                    let text = std::mem::replace(last_text, String::new_in(arena));
                    new_segments.push(StrSegment::Plaintext(text.into_bump_str()));
                }
                new_segments.push(StrSegment::Interpolated(e.normalize(arena)));
            }
        }
    }
}

#[test]
fn test_str_normalize() {
    use crate::test_helpers::parse_expr_with;
    let arena = Bump::new();
    let a = parse_expr_with(&arena, r#""a\nb""#).unwrap();
    let b = parse_expr_with(&arena, "\"\"\"\na\nb\"\"\"").unwrap();

    let ar = a.normalize(&arena);
    let br = b.normalize(&arena);

    assert_eq!(ar, br);
}

impl<'a> Normalize<'a> for StrSegment<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match *self {
            StrSegment::Plaintext(t) => StrSegment::Plaintext(t),
            StrSegment::Unicode(t) => StrSegment::Unicode(t.normalize(arena)),
            StrSegment::EscapedChar(c) => StrSegment::EscapedChar(c),
            StrSegment::Interpolated(t) => StrSegment::Interpolated(t.normalize(arena)),
        }
    }
}

impl<'a> Normalize<'a> for Expr<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
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
            Expr::Str(a) => Expr::Str(a.normalize(arena)),
            Expr::RecordAccess(a, b) => Expr::RecordAccess(arena.alloc(a.normalize(arena)), b),
            Expr::AccessorFunction(a) => Expr::AccessorFunction(a),
            Expr::RecordUpdater(a) => Expr::RecordUpdater(a),
            Expr::TupleAccess(a, b) => Expr::TupleAccess(arena.alloc(a.normalize(arena)), b),
            Expr::TrySuffix { expr: a, target } => Expr::TrySuffix {
                expr: arena.alloc(a.normalize(arena)),
                target,
            },
            Expr::List(a) => Expr::List(a.normalize(arena)),
            Expr::RecordUpdate { update, fields } => Expr::RecordUpdate {
                update: arena.alloc(update.normalize(arena)),
                fields: fields.normalize(arena),
            },
            Expr::Record(a) => Expr::Record(a.normalize(arena)),
            Expr::RecordBuilder { mapper, fields } => Expr::RecordBuilder {
                mapper: arena.alloc(mapper.normalize(arena)),
                fields: fields.normalize(arena),
            },
            Expr::Tuple(a) => Expr::Tuple(a.normalize(arena)),
            Expr::Var { module_name, ident } => Expr::Var { module_name, ident },
            Expr::Underscore(a) => Expr::Underscore(a),
            Expr::Tag(a) => Expr::Tag(a),
            Expr::OpaqueRef(a) => Expr::OpaqueRef(a),
            Expr::Closure(a, b) => Expr::Closure(
                arena.alloc(a.normalize(arena)),
                arena.alloc(b.normalize(arena)),
            ),
            Expr::Crash => Expr::Crash,
            Expr::Defs(a, b) => fold_defs(arena, a.defs(), b.value.normalize(arena)),
            Expr::Dbg => Expr::Dbg,
            Expr::DbgStmt {
                first,
                extra_args,
                continuation,
            } => Expr::DbgStmt {
                first: arena.alloc(first.normalize(arena)),
                extra_args: extra_args.normalize(arena),
                continuation: arena.alloc(continuation.normalize(arena)),
            },
            Expr::LowLevelDbg(x, a, b) => Expr::LowLevelDbg(
                x,
                arena.alloc(a.normalize(arena)),
                arena.alloc(b.normalize(arena)),
            ),
            Expr::Try => Expr::Try,
            Expr::LowLevelTry(a, kind) => Expr::LowLevelTry(arena.alloc(a.normalize(arena)), kind),
            Expr::Return(a, b) => Expr::Return(
                arena.alloc(a.normalize(arena)),
                b.map(|loc_b| &*arena.alloc(loc_b.normalize(arena))),
            ),
            Expr::Apply(a, b, c) => {
                Expr::Apply(arena.alloc(a.normalize(arena)), b.normalize(arena), c)
            }
            Expr::BinOps(a, b) => Expr::BinOps(a.normalize(arena), arena.alloc(b.normalize(arena))),
            Expr::UnaryOp(a, b) => {
                let a = a.normalize(arena);
                match (a.value, b.value) {
                    (Expr::Num(text), UnaryOp::Negate) if !text.starts_with('-') => {
                        let mut res = String::new_in(arena);
                        res.push('-');
                        res.push_str(text);
                        Expr::Num(res.into_bump_str())
                    }
                    (Expr::Float(text), UnaryOp::Negate) if !text.starts_with('-') => {
                        let mut res = String::new_in(arena);
                        res.push('-');
                        res.push_str(text);
                        Expr::Float(res.into_bump_str())
                    }
                    _ => Expr::UnaryOp(arena.alloc(a), b.normalize(arena)),
                }
            }
            Expr::If {
                if_thens,
                final_else,
                indented_else,
            } => Expr::If {
                if_thens: if_thens.normalize(arena),
                final_else: arena.alloc(final_else.normalize(arena)),
                indented_else,
            },
            Expr::When(a, b) => Expr::When(arena.alloc(a.normalize(arena)), b.normalize(arena)),
            Expr::ParensAround(a) => {
                // The formatter can remove redundant parentheses, so also remove these when normalizing for comparison.
                a.normalize(arena)
            }
            Expr::MalformedIdent(a, b) => Expr::MalformedIdent(a, remove_spaces_bad_ident(b)),
            Expr::MalformedSuffixed(a) => Expr::MalformedSuffixed(a),
            Expr::PrecedenceConflict(a) => Expr::PrecedenceConflict(a),
            Expr::SpaceBefore(a, _) => a.normalize(arena),
            Expr::SpaceAfter(a, _) => a.normalize(arena),
            Expr::SingleQuote(a) => Expr::SingleQuote(a),
            Expr::EmptyRecordBuilder(a) => {
                Expr::EmptyRecordBuilder(arena.alloc(a.normalize(arena)))
            }
            Expr::SingleFieldRecordBuilder(a) => {
                Expr::SingleFieldRecordBuilder(arena.alloc(a.normalize(arena)))
            }
            Expr::OptionalFieldInRecordBuilder(a, b) => Expr::OptionalFieldInRecordBuilder(
                arena.alloc(a.normalize(arena)),
                arena.alloc(b.normalize(arena)),
            ),
        }
    }
}

fn fold_defs<'a>(
    arena: &'a Bump,
    mut defs: impl Iterator<Item = Result<&'a TypeDef<'a>, &'a ValueDef<'a>>>,
    final_expr: Expr<'a>,
) -> Expr<'a> {
    let mut new_defs = Defs::default();

    while let Some(def) = defs.next() {
        match def {
            Ok(td) => {
                let td = td.normalize(arena);
                new_defs.push_type_def(td, Region::zero(), &[], &[]);
            }
            Err(vd) => {
                let vd = vd.normalize(arena);

                match vd {
                    ValueDef::Stmt(&Loc {
                        value:
                            Expr::Apply(
                                &Loc {
                                    value: Expr::Dbg, ..
                                },
                                args,
                                _,
                            ),
                        ..
                    }) => {
                        let rest = fold_defs(arena, defs, final_expr);
                        let new_final = Expr::DbgStmt {
                            first: args[0],
                            extra_args: &args[1..],
                            continuation: arena.alloc(Loc::at_zero(rest)),
                        };
                        if new_defs.is_empty() {
                            return new_final;
                        }
                        return Expr::Defs(
                            arena.alloc(new_defs),
                            arena.alloc(Loc::at_zero(new_final)),
                        );
                    }
                    _ => {
                        new_defs.push_value_def(vd, Region::zero(), &[], &[]);
                    }
                }
            }
        }
    }
    if new_defs.is_empty() {
        return final_expr;
    }
    Expr::Defs(arena.alloc(new_defs), arena.alloc(Loc::at_zero(final_expr)))
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
        BadIdent::TooManyUnderscores(_) => BadIdent::TooManyUnderscores(Position::zero()),
        BadIdent::QualifiedTag(_) => BadIdent::QualifiedTag(Position::zero()),
        BadIdent::WeirdAccessor(_) => BadIdent::WeirdAccessor(Position::zero()),
        BadIdent::WeirdDotAccess(_) => BadIdent::WeirdDotAccess(Position::zero()),
        BadIdent::WeirdDotQualified(_) => BadIdent::WeirdDotQualified(Position::zero()),
        BadIdent::StrayDot(_) => BadIdent::StrayDot(Position::zero()),
        BadIdent::StrayAmpersand(_) => BadIdent::StrayAmpersand(Position::zero()),
        BadIdent::BadOpaqueRef(_) => BadIdent::BadOpaqueRef(Position::zero()),
        BadIdent::QualifiedTupleAccessor(_) => BadIdent::QualifiedTupleAccessor(Position::zero()),
    }
}

impl<'a> Normalize<'a> for Pattern<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match *self {
            Pattern::Identifier { ident } => Pattern::Identifier { ident },
            Pattern::Tag(a) => Pattern::Tag(a),
            Pattern::OpaqueRef(a) => Pattern::OpaqueRef(a),
            Pattern::Apply(a, b, c) => Pattern::Apply(
                arena.alloc(a.normalize(arena)),
                arena.alloc(b.normalize(arena)),
                c,
            ),
            Pattern::RecordDestructure(a) => Pattern::RecordDestructure(a.normalize(arena)),
            Pattern::RequiredField(a, b) => {
                Pattern::RequiredField(a, arena.alloc(b.normalize(arena)))
            }
            Pattern::OptionalField(a, b) => {
                Pattern::OptionalField(a, arena.alloc(b.normalize(arena)))
            }
            Pattern::As(pattern, pattern_as) => Pattern::As(
                arena.alloc(pattern.normalize(arena)),
                pattern_as.normalize(arena),
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
            Pattern::StrLiteral(a) => Pattern::StrLiteral(a.normalize(arena)),
            Pattern::Underscore(a) => Pattern::Underscore(a),
            Pattern::Malformed(a) => Pattern::Malformed(a),
            Pattern::MalformedIdent(a, b) => Pattern::MalformedIdent(a, remove_spaces_bad_ident(b)),
            Pattern::QualifiedIdentifier { module_name, ident } => {
                Pattern::QualifiedIdentifier { module_name, ident }
            }
            Pattern::SpaceBefore(a, _) => a.normalize(arena),
            Pattern::SpaceAfter(a, _) => a.normalize(arena),
            Pattern::SingleQuote(a) => Pattern::SingleQuote(a),
            Pattern::List(pats) => Pattern::List(pats.normalize(arena)),
            Pattern::Tuple(pats) => Pattern::Tuple(pats.normalize(arena)),
            Pattern::ListRest(opt_pattern_as) => Pattern::ListRest(
                opt_pattern_as.map(|(_, pattern_as)| ([].as_ref(), pattern_as.normalize(arena))),
            ),
        }
    }
}

impl<'a> Normalize<'a> for TypeAnnotation<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match *self {
            TypeAnnotation::Function(a, arrow, b) => TypeAnnotation::Function(
                arena.alloc(a.normalize(arena)),
                arrow,
                arena.alloc(b.normalize(arena)),
            ),
            TypeAnnotation::Apply(a, b, c) => TypeAnnotation::Apply(a, b, c.normalize(arena)),
            TypeAnnotation::BoundVariable(a) => TypeAnnotation::BoundVariable(a),
            TypeAnnotation::As(a, _, TypeHeader { name, vars }) => TypeAnnotation::As(
                arena.alloc(a.normalize(arena)),
                &[],
                TypeHeader {
                    name: name.normalize(arena),
                    vars: vars.normalize(arena),
                },
            ),
            TypeAnnotation::Tuple { elems: fields, ext } => TypeAnnotation::Tuple {
                elems: fields.normalize(arena),
                ext: ext.normalize(arena),
            },
            TypeAnnotation::Record { fields, ext } => TypeAnnotation::Record {
                fields: fields.normalize(arena),
                ext: ext.normalize(arena),
            },
            TypeAnnotation::TagUnion { ext, tags } => TypeAnnotation::TagUnion {
                ext: ext.normalize(arena),
                tags: tags.normalize(arena),
            },
            TypeAnnotation::Inferred => TypeAnnotation::Inferred,
            TypeAnnotation::Wildcard => TypeAnnotation::Wildcard,
            TypeAnnotation::Where(annot, has_clauses) => TypeAnnotation::Where(
                arena.alloc(annot.normalize(arena)),
                arena.alloc(has_clauses.normalize(arena)),
            ),
            TypeAnnotation::SpaceBefore(a, _) => a.normalize(arena),
            TypeAnnotation::SpaceAfter(a, _) => a.normalize(arena),
            TypeAnnotation::Malformed(a) => TypeAnnotation::Malformed(a),
        }
    }
}

impl<'a> Normalize<'a> for ImplementsClause<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        ImplementsClause {
            var: self.var.normalize(arena),
            abilities: self.abilities.normalize(arena),
        }
    }
}

impl<'a> Normalize<'a> for Tag<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match *self {
            Tag::Apply { name, args } => Tag::Apply {
                name: name.normalize(arena),
                args: args.normalize(arena),
            },
            Tag::SpaceBefore(a, _) => a.normalize(arena),
            Tag::SpaceAfter(a, _) => a.normalize(arena),
        }
    }
}

impl<'a> Normalize<'a> for AbilityImpls<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match *self {
            AbilityImpls::AbilityImpls(impls) => AbilityImpls::AbilityImpls(impls.normalize(arena)),
            AbilityImpls::SpaceBefore(has, _) | AbilityImpls::SpaceAfter(has, _) => {
                has.normalize(arena)
            }
        }
    }
}

impl<'a> Normalize<'a> for ImplementsAbility<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match *self {
            ImplementsAbility::ImplementsAbility { ability, impls } => {
                ImplementsAbility::ImplementsAbility {
                    ability: ability.normalize(arena),
                    impls: impls.normalize(arena),
                }
            }
            ImplementsAbility::SpaceBefore(has, _) | ImplementsAbility::SpaceAfter(has, _) => {
                has.normalize(arena)
            }
        }
    }
}

impl<'a> Normalize<'a> for ImplementsAbilities<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match *self {
            ImplementsAbilities::Implements(derived) => {
                ImplementsAbilities::Implements(derived.normalize(arena))
            }
            ImplementsAbilities::SpaceBefore(derived, _)
            | ImplementsAbilities::SpaceAfter(derived, _) => derived.normalize(arena),
        }
    }
}

impl<'a> Normalize<'a> for PatternAs<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        PatternAs {
            spaces_before: &[],
            identifier: self.identifier.normalize(arena),
        }
    }
}

impl<'a> Normalize<'a> for EExpr<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
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
            EExpr::BadOperator(inner_err, _pos) => {
                EExpr::BadOperator(arena.alloc(inner_err.normalize(arena)), Position::zero())
            }
            EExpr::DefMissingFinalExpr(_pos) => EExpr::DefMissingFinalExpr(Position::zero()),
            EExpr::DefMissingFinalExpr2(inner_err, _pos) => EExpr::DefMissingFinalExpr2(
                arena.alloc(inner_err.normalize(arena)),
                Position::zero(),
            ),
            EExpr::Type(inner_err, _pos) => {
                EExpr::Type(inner_err.normalize(arena), Position::zero())
            }
            EExpr::Pattern(inner_err, _pos) => {
                EExpr::Pattern(arena.alloc(inner_err.normalize(arena)), Position::zero())
            }
            EExpr::Ability(inner_err, _pos) => {
                EExpr::Ability(inner_err.normalize(arena), Position::zero())
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
            EExpr::DbgContinue(_pos) => EExpr::DbgContinue(Position::zero()),
            EExpr::When(inner_err, _pos) => {
                EExpr::When(inner_err.normalize(arena), Position::zero())
            }
            EExpr::If(inner_err, _pos) => EExpr::If(inner_err.normalize(arena), Position::zero()),
            EExpr::Expect(inner_err, _pos) => {
                EExpr::Expect(inner_err.normalize(arena), Position::zero())
            }
            EExpr::Return(inner_err, _pos) => {
                EExpr::Return(inner_err.normalize(arena), Position::zero())
            }
            EExpr::Dbg(inner_err, _pos) => EExpr::Dbg(inner_err.normalize(arena), Position::zero()),
            EExpr::Import(inner_err, _pos) => {
                EExpr::Import(inner_err.normalize(arena), Position::zero())
            }
            EExpr::Closure(inner_err, _pos) => {
                EExpr::Closure(inner_err.normalize(arena), Position::zero())
            }
            EExpr::Underscore(_pos) => EExpr::Underscore(Position::zero()),
            EExpr::Crash(_pos) => EExpr::Crash(Position::zero()),
            EExpr::Try(_pos) => EExpr::Try(Position::zero()),
            EExpr::InParens(inner_err, _pos) => {
                EExpr::InParens(inner_err.normalize(arena), Position::zero())
            }
            EExpr::Record(inner_err, _pos) => {
                EExpr::Record(inner_err.normalize(arena), Position::zero())
            }
            EExpr::Str(inner_err, _pos) => EExpr::Str(inner_err.normalize(arena), Position::zero()),
            EExpr::Number(inner_err, _pos) => EExpr::Number(inner_err.clone(), Position::zero()),
            EExpr::List(inner_err, _pos) => {
                EExpr::List(inner_err.normalize(arena), Position::zero())
            }
            EExpr::IndentStart(_pos) => EExpr::IndentStart(Position::zero()),
            EExpr::IndentEnd(_pos) => EExpr::IndentEnd(Position::zero()),
            EExpr::UnexpectedComma(_pos) => EExpr::UnexpectedComma(Position::zero()),
            EExpr::UnexpectedTopLevelExpr(_pos) => EExpr::UnexpectedTopLevelExpr(Position::zero()),
            EExpr::RecordUpdateOldBuilderField(_pos) => {
                EExpr::RecordUpdateOldBuilderField(Region::zero())
            }
            EExpr::RecordUpdateIgnoredField(_pos) => {
                EExpr::RecordUpdateIgnoredField(Region::zero())
            }
            EExpr::RecordBuilderOldBuilderField(_pos) => {
                EExpr::RecordBuilderOldBuilderField(Region::zero())
            }
        }
    }
}

impl<'a> Normalize<'a> for EList<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match self {
            EList::Open(_pos) => EList::Open(Position::zero()),
            EList::End(_pos) => EList::End(Position::zero()),
            EList::Space(inner_err, _pos) => EList::Space(*inner_err, Position::zero()),
            EList::Expr(inner_err, _pos) => {
                EList::Expr(arena.alloc(inner_err.normalize(arena)), Position::zero())
            }
        }
    }
}

impl<'a> Normalize<'a> for EString<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
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
                EString::Format(arena.alloc(inner.normalize(arena)), Position::zero())
            }
            EString::FormatEnd(_) => EString::FormatEnd(Position::zero()),
            EString::MultilineInsufficientIndent(_) => {
                EString::MultilineInsufficientIndent(Position::zero())
            }
            EString::ExpectedDoubleQuoteGotSingleQuote(_) => {
                EString::ExpectedDoubleQuoteGotSingleQuote(Position::zero())
            }
            EString::InvalidUnicodeCodepoint(_region) => {
                EString::InvalidUnicodeCodepoint(Region::zero())
            }
            EString::UnicodeEscapeTooLarge(_region) => {
                EString::UnicodeEscapeTooLarge(Region::zero())
            }
        }
    }
}

impl<'a> Normalize<'a> for EClosure<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match self {
            EClosure::Space(inner_err, _) => EClosure::Space(*inner_err, Position::zero()),
            EClosure::Start(_) => EClosure::Start(Position::zero()),
            EClosure::Arrow(_) => EClosure::Arrow(Position::zero()),
            EClosure::Comma(_) => EClosure::Comma(Position::zero()),
            EClosure::Arg(_) => EClosure::Arg(Position::zero()),
            EClosure::Pattern(inner_err, _) => {
                EClosure::Pattern(inner_err.normalize(arena), Position::zero())
            }
            EClosure::Body(inner_err, _) => {
                EClosure::Body(arena.alloc(inner_err.normalize(arena)), Position::zero())
            }
            EClosure::IndentArrow(_) => EClosure::IndentArrow(Position::zero()),
            EClosure::IndentBody(_) => EClosure::IndentBody(Position::zero()),
            EClosure::IndentArg(_) => EClosure::IndentArg(Position::zero()),
        }
    }
}

impl<'a> Normalize<'a> for EInParens<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match self {
            EInParens::End(_) => EInParens::End(Position::zero()),
            EInParens::Open(_) => EInParens::Open(Position::zero()),
            EInParens::Empty(_) => EInParens::Empty(Position::zero()),
            EInParens::Expr(inner_err, _) => {
                EInParens::Expr(arena.alloc(inner_err.normalize(arena)), Position::zero())
            }
            EInParens::Space(inner_err, _) => EInParens::Space(*inner_err, Position::zero()),
        }
    }
}

impl<'a> Normalize<'a> for ERecord<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match self {
            ERecord::End(_) => ERecord::End(Position::zero()),
            ERecord::Open(_) => ERecord::Open(Position::zero()),
            ERecord::Field(_pos) => ERecord::Field(Position::zero()),
            ERecord::UnderscoreField(_pos) => ERecord::Field(Position::zero()),
            ERecord::Colon(_) => ERecord::Colon(Position::zero()),
            ERecord::QuestionMark(_) => ERecord::QuestionMark(Position::zero()),
            ERecord::Arrow(_) => ERecord::Arrow(Position::zero()),
            ERecord::Ampersand(_) => ERecord::Ampersand(Position::zero()),
            ERecord::Expr(inner_err, _) => {
                ERecord::Expr(arena.alloc(inner_err.normalize(arena)), Position::zero())
            }
            ERecord::Space(inner_err, _) => ERecord::Space(*inner_err, Position::zero()),
            ERecord::Prefix(_) => ERecord::Prefix(Position::zero()),
        }
    }
}

impl<'a> Normalize<'a> for EPattern<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match self {
            EPattern::Record(inner_err, _) => {
                EPattern::Record(inner_err.normalize(arena), Position::zero())
            }
            EPattern::List(inner_err, _) => {
                EPattern::List(inner_err.normalize(arena), Position::zero())
            }
            EPattern::AsKeyword(_) => EPattern::AsKeyword(Position::zero()),
            EPattern::AsIdentifier(_) => EPattern::AsIdentifier(Position::zero()),
            EPattern::Underscore(_) => EPattern::Underscore(Position::zero()),
            EPattern::NotAPattern(_) => EPattern::NotAPattern(Position::zero()),
            EPattern::Start(_) => EPattern::Start(Position::zero()),
            EPattern::End(_) => EPattern::End(Position::zero()),
            EPattern::Space(inner_err, _) => EPattern::Space(*inner_err, Position::zero()),
            EPattern::PInParens(inner_err, _) => {
                EPattern::PInParens(inner_err.normalize(arena), Position::zero())
            }
            EPattern::NumLiteral(inner_err, _) => {
                EPattern::NumLiteral(inner_err.clone(), Position::zero())
            }
            EPattern::IndentStart(_) => EPattern::IndentStart(Position::zero()),
            EPattern::IndentEnd(_) => EPattern::IndentEnd(Position::zero()),
            EPattern::AsIndentStart(_) => EPattern::AsIndentStart(Position::zero()),
            EPattern::AccessorFunction(_) => EPattern::AccessorFunction(Position::zero()),
            EPattern::RecordUpdaterFunction(_) => EPattern::RecordUpdaterFunction(Position::zero()),
            EPattern::Str(e, _) => EPattern::Str(e.normalize(arena), Position::zero()),
            EPattern::ParenStart(_) => EPattern::ParenStart(Position::zero()),
            EPattern::ParenEnd(_) => EPattern::ParenEnd(Position::zero()),
        }
    }
}

impl<'a> Normalize<'a> for EImport<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match self {
            EImport::Import(_) => EImport::Import(Position::zero()),
            EImport::IndentStart(_) => EImport::IndentStart(Position::zero()),
            EImport::PackageShorthand(_) => EImport::PackageShorthand(Position::zero()),
            EImport::PackageShorthandDot(_) => EImport::PackageShorthandDot(Position::zero()),
            EImport::ModuleName(_) => EImport::ModuleName(Position::zero()),
            EImport::Params(inner_err, _) => {
                EImport::Params(inner_err.normalize(arena), Position::zero())
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
                EImport::Annotation(inner_err.normalize(arena), Position::zero())
            }
            EImport::Space(inner_err, _) => EImport::Space(*inner_err, Position::zero()),
            EImport::EndNewline(_) => EImport::EndNewline(Position::zero()),
        }
    }
}

impl<'a> Normalize<'a> for EType<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match self {
            EType::Space(inner_err, _) => EType::Space(*inner_err, Position::zero()),
            EType::UnderscoreSpacing(_) => EType::UnderscoreSpacing(Position::zero()),
            EType::TRecord(inner_err, _) => {
                EType::TRecord(inner_err.normalize(arena), Position::zero())
            }
            EType::TTagUnion(inner_err, _) => {
                EType::TTagUnion(inner_err.normalize(arena), Position::zero())
            }
            EType::TInParens(inner_err, _) => {
                EType::TInParens(inner_err.normalize(arena), Position::zero())
            }
            EType::TApply(inner_err, _) => {
                EType::TApply(inner_err.normalize(arena), Position::zero())
            }
            EType::TInlineAlias(inner_err, _) => {
                EType::TInlineAlias(inner_err.normalize(arena), Position::zero())
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
                EType::TAbilityImpl(inner_err.normalize(arena), Position::zero())
            }
            EType::TIndentStart(_) => EType::TIndentStart(Position::zero()),
            EType::TIndentEnd(_) => EType::TIndentEnd(Position::zero()),
            EType::TAsIndentStart(_) => EType::TAsIndentStart(Position::zero()),
        }
    }
}

impl<'a> Normalize<'a> for EImportParams<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match self {
            EImportParams::Indent(_) => EImportParams::Indent(Position::zero()),
            EImportParams::Record(inner_err, _) => {
                EImportParams::Record(inner_err.normalize(arena), Position::zero())
            }
            EImportParams::RecordUpdateFound(_) => EImportParams::RecordUpdateFound(Region::zero()),
            EImportParams::RecordIgnoredFieldFound(_) => {
                EImportParams::RecordIgnoredFieldFound(Region::zero())
            }
            EImportParams::Space(inner_err, _) => {
                EImportParams::Space(*inner_err, Position::zero())
            }
            EImportParams::RecordBuilderFound(_) => {
                EImportParams::RecordBuilderFound(Region::zero())
            }
        }
    }
}

impl<'a> Normalize<'a> for PInParens<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match self {
            PInParens::Empty(_) => PInParens::Empty(Position::zero()),
            PInParens::End(_) => PInParens::End(Position::zero()),
            PInParens::Open(_) => PInParens::Open(Position::zero()),
            PInParens::Pattern(inner_err, _) => {
                PInParens::Pattern(arena.alloc(inner_err.normalize(arena)), Position::zero())
            }
            PInParens::Space(inner_err, _) => PInParens::Space(*inner_err, Position::zero()),
        }
    }
}

impl<'a> Normalize<'a> for ETypeAbilityImpl<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match self {
            ETypeAbilityImpl::End(_) => ETypeAbilityImpl::End(Position::zero()),
            ETypeAbilityImpl::Open(_) => ETypeAbilityImpl::Open(Position::zero()),
            ETypeAbilityImpl::Field(_) => ETypeAbilityImpl::Field(Position::zero()),
            ETypeAbilityImpl::UnderscoreField(_) => {
                ETypeAbilityImpl::UnderscoreField(Position::zero())
            }
            ETypeAbilityImpl::Colon(_) => ETypeAbilityImpl::Colon(Position::zero()),
            ETypeAbilityImpl::Arrow(_) => ETypeAbilityImpl::Arrow(Position::zero()),
            ETypeAbilityImpl::Optional(_) => ETypeAbilityImpl::Optional(Position::zero()),
            ETypeAbilityImpl::Type(inner_err, _) => {
                ETypeAbilityImpl::Type(arena.alloc(inner_err.normalize(arena)), Position::zero())
            }
            ETypeAbilityImpl::Space(inner_err, _) => {
                ETypeAbilityImpl::Space(*inner_err, Position::zero())
            }
            ETypeAbilityImpl::QuestionMark(_) => ETypeAbilityImpl::QuestionMark(Position::zero()),
            ETypeAbilityImpl::Ampersand(_) => ETypeAbilityImpl::Ampersand(Position::zero()),
            ETypeAbilityImpl::Expr(inner_err, _) => {
                ETypeAbilityImpl::Expr(arena.alloc(inner_err.normalize(arena)), Position::zero())
            }
            ETypeAbilityImpl::IndentBar(_) => ETypeAbilityImpl::IndentBar(Position::zero()),
            ETypeAbilityImpl::IndentAmpersand(_) => {
                ETypeAbilityImpl::IndentAmpersand(Position::zero())
            }
            ETypeAbilityImpl::Prefix(_) => ETypeAbilityImpl::Prefix(Position::zero()),
        }
    }
}

impl<'a> Normalize<'a> for ETypeInlineAlias {
    fn normalize(&self, _arena: &'a Bump) -> Self {
        match self {
            ETypeInlineAlias::NotAnAlias(_pos) => ETypeInlineAlias::NotAnAlias(Position::zero()),
            ETypeInlineAlias::Qualified(_pos) => ETypeInlineAlias::Qualified(Position::zero()),
            ETypeInlineAlias::ArgumentNotLowercase(_pos) => {
                ETypeInlineAlias::ArgumentNotLowercase(Position::zero())
            }
        }
    }
}

impl<'a> Normalize<'a> for ETypeApply {
    fn normalize(&self, _arena: &'a Bump) -> Self {
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

impl<'a> Normalize<'a> for ETypeInParens<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match self {
            ETypeInParens::Empty(_) => ETypeInParens::Empty(Position::zero()),
            ETypeInParens::End(_) => ETypeInParens::End(Position::zero()),
            ETypeInParens::Open(_) => ETypeInParens::Open(Position::zero()),
            ETypeInParens::Type(inner_err, _) => {
                ETypeInParens::Type(arena.alloc(inner_err.normalize(arena)), Position::zero())
            }
            ETypeInParens::Space(inner_err, _) => {
                ETypeInParens::Space(*inner_err, Position::zero())
            }
            ETypeInParens::IndentOpen(_) => ETypeInParens::IndentOpen(Position::zero()),
            ETypeInParens::IndentEnd(_) => ETypeInParens::IndentEnd(Position::zero()),
        }
    }
}

impl<'a> Normalize<'a> for ETypeTagUnion<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match self {
            ETypeTagUnion::End(_) => ETypeTagUnion::End(Position::zero()),
            ETypeTagUnion::Open(_) => ETypeTagUnion::Open(Position::zero()),
            ETypeTagUnion::Type(inner_err, _) => {
                ETypeTagUnion::Type(arena.alloc(inner_err.normalize(arena)), Position::zero())
            }
            ETypeTagUnion::Space(inner_err, _) => {
                ETypeTagUnion::Space(*inner_err, Position::zero())
            }
        }
    }
}

impl<'a> Normalize<'a> for ETypeRecord<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match self {
            ETypeRecord::End(_) => ETypeRecord::End(Position::zero()),
            ETypeRecord::Open(_) => ETypeRecord::Open(Position::zero()),
            ETypeRecord::Field(_) => ETypeRecord::Field(Position::zero()),
            ETypeRecord::Colon(_) => ETypeRecord::Colon(Position::zero()),
            ETypeRecord::Optional(_) => ETypeRecord::Optional(Position::zero()),
            ETypeRecord::Type(inner_err, _) => {
                ETypeRecord::Type(arena.alloc(inner_err.normalize(arena)), Position::zero())
            }
            ETypeRecord::Space(inner_err, _) => ETypeRecord::Space(*inner_err, Position::zero()),
            ETypeRecord::IndentOpen(_) => ETypeRecord::IndentOpen(Position::zero()),
            ETypeRecord::IndentColon(_) => ETypeRecord::IndentColon(Position::zero()),
            ETypeRecord::IndentOptional(_) => ETypeRecord::IndentOptional(Position::zero()),
            ETypeRecord::IndentEnd(_) => ETypeRecord::IndentEnd(Position::zero()),
        }
    }
}

impl<'a> Normalize<'a> for PRecord<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match self {
            PRecord::End(_) => PRecord::End(Position::zero()),
            PRecord::Open(_) => PRecord::Open(Position::zero()),
            PRecord::Field(_) => PRecord::Field(Position::zero()),
            PRecord::Colon(_) => PRecord::Colon(Position::zero()),
            PRecord::Optional(_) => PRecord::Optional(Position::zero()),
            PRecord::Pattern(inner_err, _) => {
                PRecord::Pattern(arena.alloc(inner_err.normalize(arena)), Position::zero())
            }
            PRecord::Expr(inner_err, _) => {
                PRecord::Expr(arena.alloc(inner_err.normalize(arena)), Position::zero())
            }
            PRecord::Space(inner_err, _) => PRecord::Space(*inner_err, Position::zero()),
        }
    }
}

impl<'a> Normalize<'a> for PList<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match self {
            PList::End(_) => PList::End(Position::zero()),
            PList::Open(_) => PList::Open(Position::zero()),
            PList::Rest(_) => PList::Rest(Position::zero()),
            PList::Pattern(inner_err, _) => {
                PList::Pattern(arena.alloc(inner_err.normalize(arena)), Position::zero())
            }
            PList::Space(inner_err, _) => PList::Space(*inner_err, Position::zero()),
        }
    }
}

impl<'a> Normalize<'a> for EExpect<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match self {
            EExpect::Space(inner_err, _) => EExpect::Space(*inner_err, Position::zero()),
            EExpect::Dbg(_) => EExpect::Dbg(Position::zero()),
            EExpect::Expect(_) => EExpect::Expect(Position::zero()),
            EExpect::Condition(inner_err, _) => {
                EExpect::Condition(arena.alloc(inner_err.normalize(arena)), Position::zero())
            }
            EExpect::Continuation(inner_err, _) => {
                EExpect::Continuation(arena.alloc(inner_err.normalize(arena)), Position::zero())
            }
            EExpect::IndentCondition(_) => EExpect::IndentCondition(Position::zero()),
        }
    }
}

impl<'a> Normalize<'a> for EReturn<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match self {
            EReturn::Space(inner_err, _) => EReturn::Space(*inner_err, Position::zero()),
            EReturn::Return(_) => EReturn::Return(Position::zero()),
            EReturn::ReturnValue(inner_err, _) => {
                EReturn::ReturnValue(arena.alloc(inner_err.normalize(arena)), Position::zero())
            }
            EReturn::IndentReturnValue(_) => EReturn::IndentReturnValue(Position::zero()),
        }
    }
}

impl<'a> Normalize<'a> for EIf<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match self {
            EIf::Space(inner_err, _) => EIf::Space(*inner_err, Position::zero()),
            EIf::If(_) => EIf::If(Position::zero()),
            EIf::Then(_) => EIf::Then(Position::zero()),
            EIf::Else(_) => EIf::Else(Position::zero()),
            EIf::Condition(inner_err, _) => {
                EIf::Condition(arena.alloc(inner_err.normalize(arena)), Position::zero())
            }
            EIf::ThenBranch(inner_err, _) => {
                EIf::ThenBranch(arena.alloc(inner_err.normalize(arena)), Position::zero())
            }
            EIf::ElseBranch(inner_err, _) => {
                EIf::ElseBranch(arena.alloc(inner_err.normalize(arena)), Position::zero())
            }
            EIf::IndentCondition(_) => EIf::IndentCondition(Position::zero()),
            EIf::IndentIf(_) => EIf::IndentIf(Position::zero()),
            EIf::IndentThenToken(_) => EIf::IndentThenToken(Position::zero()),
            EIf::IndentElseToken(_) => EIf::IndentElseToken(Position::zero()),
            EIf::IndentThenBranch(_) => EIf::IndentThenBranch(Position::zero()),
            EIf::IndentElseBranch(_) => EIf::IndentElseBranch(Position::zero()),
        }
    }
}

impl<'a> Normalize<'a> for EWhen<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match self {
            EWhen::Space(inner_err, _) => EWhen::Space(*inner_err, Position::zero()),
            EWhen::When(_) => EWhen::When(Position::zero()),
            EWhen::Is(_) => EWhen::Is(Position::zero()),
            EWhen::Pattern(inner_err, _) => {
                EWhen::Pattern(inner_err.normalize(arena), Position::zero())
            }
            EWhen::Arrow(_) => EWhen::Arrow(Position::zero()),
            EWhen::Bar(_) => EWhen::Bar(Position::zero()),
            EWhen::IfToken(_) => EWhen::IfToken(Position::zero()),
            EWhen::IfGuard(inner_err, _) => {
                EWhen::IfGuard(arena.alloc(inner_err.normalize(arena)), Position::zero())
            }
            EWhen::Condition(inner_err, _) => {
                EWhen::Condition(arena.alloc(inner_err.normalize(arena)), Position::zero())
            }
            EWhen::Branch(inner_err, _) => {
                EWhen::Branch(arena.alloc(inner_err.normalize(arena)), Position::zero())
            }
            EWhen::IndentCondition(_) => EWhen::IndentCondition(Position::zero()),
            EWhen::IndentPattern(_) => EWhen::IndentPattern(Position::zero()),
            EWhen::IndentArrow(_) => EWhen::IndentArrow(Position::zero()),
            EWhen::IndentBranch(_) => EWhen::IndentBranch(Position::zero()),
            EWhen::IndentIfGuard(_) => EWhen::IndentIfGuard(Position::zero()),
            EWhen::PatternAlignment(_alignment, _) => EWhen::PatternAlignment(0, Position::zero()),
        }
    }
}

impl<'a> Normalize<'a> for EAbility<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match self {
            EAbility::Space(inner_err, _) => EAbility::Space(*inner_err, Position::zero()),
            EAbility::Type(inner_err, _) => {
                EAbility::Type(inner_err.normalize(arena), Position::zero())
            }
            EAbility::DemandAlignment(_alignment, _) => {
                EAbility::DemandAlignment(0, Position::zero())
            }
            EAbility::DemandName(_) => EAbility::DemandName(Position::zero()),
            EAbility::DemandColon(_) => EAbility::DemandColon(Position::zero()),
        }
    }
}

impl<'a> Normalize<'a> for EPackages<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
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
                EPackages::PackageEntry(inner_err.normalize(arena), Position::zero())
            }
        }
    }
}

impl<'a> Normalize<'a> for EHeader<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match self {
            EHeader::Provides(inner_err, _) => {
                EHeader::Provides(inner_err.normalize(arena), Position::zero())
            }
            EHeader::Params(inner_err, _) => {
                EHeader::Params(inner_err.normalize(arena), Position::zero())
            }
            EHeader::Exposes(inner_err, _) => {
                EHeader::Exposes(inner_err.normalize(arena), Position::zero())
            }
            EHeader::Imports(inner_err, _) => {
                EHeader::Imports(inner_err.normalize(arena), Position::zero())
            }
            EHeader::Requires(inner_err, _) => {
                EHeader::Requires(inner_err.normalize(arena), Position::zero())
            }
            EHeader::Packages(inner_err, _) => {
                EHeader::Packages(inner_err.normalize(arena), Position::zero())
            }
            EHeader::Space(inner_err, _) => EHeader::Space(*inner_err, Position::zero()),
            EHeader::Start(_) => EHeader::Start(Position::zero()),
            EHeader::ModuleName(_) => EHeader::ModuleName(Position::zero()),
            EHeader::AppName(inner_err, _) => {
                EHeader::AppName(inner_err.normalize(arena), Position::zero())
            }
            EHeader::PackageName(inner_err, _) => {
                EHeader::PackageName(inner_err.normalize(arena), Position::zero())
            }
            EHeader::PlatformName(inner_err, _) => {
                EHeader::PlatformName(inner_err.normalize(arena), Position::zero())
            }
            EHeader::IndentStart(_) => EHeader::IndentStart(Position::zero()),
            EHeader::InconsistentModuleName(_) => EHeader::InconsistentModuleName(Region::zero()),
        }
    }
}

impl<'a> Normalize<'a> for EPackageName<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match self {
            EPackageName::BadPath(inner_err, _) => {
                EPackageName::BadPath(inner_err.normalize(arena), Position::zero())
            }
            EPackageName::Escapes(_) => EPackageName::Escapes(Position::zero()),
            EPackageName::Multiline(_) => EPackageName::Multiline(Position::zero()),
        }
    }
}

impl<'a> Normalize<'a> for SyntaxError<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
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
            SyntaxError::Type(err) => SyntaxError::Type(err.normalize(arena)),
            SyntaxError::Pattern(err) => SyntaxError::Pattern(err.normalize(arena)),
            SyntaxError::Expr(err, _) => SyntaxError::Expr(err.normalize(arena), Position::zero()),
            SyntaxError::Header(err) => SyntaxError::Header(err.normalize(arena)),
            SyntaxError::Space(inner_err) => SyntaxError::Space(*inner_err),
            SyntaxError::NotEndOfFile(_) => SyntaxError::NotEndOfFile(Position::zero()),
        }
    }
}

impl<'a> Normalize<'a> for EPackageEntry<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match self {
            EPackageEntry::BadPackage(inner_err, _) => {
                EPackageEntry::BadPackage(inner_err.normalize(arena), Position::zero())
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

impl<'a> Normalize<'a> for EProvides<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
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
                EProvides::Package(inner_err.normalize(arena), Position::zero())
            }
            EProvides::Space(inner_err, _) => EProvides::Space(*inner_err, Position::zero()),
        }
    }
}

impl<'a> Normalize<'a> for EParams<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match self {
            EParams::Pattern(inner_err, _) => {
                EParams::Pattern(inner_err.normalize(arena), Position::zero())
            }
            EParams::BeforeArrow(_) => EParams::BeforeArrow(Position::zero()),
            EParams::Arrow(_) => EParams::Arrow(Position::zero()),
            EParams::AfterArrow(_) => EParams::AfterArrow(Position::zero()),
            EParams::Space(inner_err, _) => EParams::Space(*inner_err, Position::zero()),
        }
    }
}

impl<'a> Normalize<'a> for EExposes {
    fn normalize(&self, _arena: &'a Bump) -> Self {
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

impl<'a> Normalize<'a> for EImports {
    fn normalize(&self, _arena: &'a Bump) -> Self {
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

impl<'a> Normalize<'a> for ERequires<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match self {
            ERequires::Requires(_) => ERequires::Requires(Position::zero()),
            ERequires::Open(_) => ERequires::Open(Position::zero()),
            ERequires::IndentRequires(_) => ERequires::IndentRequires(Position::zero()),
            ERequires::IndentListStart(_) => ERequires::IndentListStart(Position::zero()),
            ERequires::ListStart(_) => ERequires::ListStart(Position::zero()),
            ERequires::ListEnd(_) => ERequires::ListEnd(Position::zero()),
            ERequires::TypedIdent(inner_err, _) => {
                ERequires::TypedIdent(inner_err.normalize(arena), Position::zero())
            }
            ERequires::Rigid(_) => ERequires::Rigid(Position::zero()),
            ERequires::Space(inner_err, _) => ERequires::Space(*inner_err, Position::zero()),
        }
    }
}

impl<'a> Normalize<'a> for ETypedIdent<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match self {
            ETypedIdent::Space(inner_err, _) => ETypedIdent::Space(*inner_err, Position::zero()),
            ETypedIdent::HasType(_) => ETypedIdent::HasType(Position::zero()),
            ETypedIdent::IndentHasType(_) => ETypedIdent::IndentHasType(Position::zero()),
            ETypedIdent::Name(_) => ETypedIdent::Name(Position::zero()),
            ETypedIdent::Type(inner_err, _) => {
                ETypedIdent::Type(inner_err.normalize(arena), Position::zero())
            }
            ETypedIdent::IndentType(_) => ETypedIdent::IndentType(Position::zero()),
            ETypedIdent::Identifier(_) => ETypedIdent::Identifier(Position::zero()),
        }
    }
}
