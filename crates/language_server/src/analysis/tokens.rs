use bumpalo::{
    collections::{CollectIn, Vec as BumpVec},
    vec as bumpvec, Bump,
};
use roc_module::called_via::{BinOp, UnaryOp};
use roc_parse::{
    ast::{
        AbilityImpls, AbilityMember, AssignedField, Collection, Defs, Expr, Header, Implements,
        ImplementsAbilities, ImplementsAbility, ImplementsClause, Pattern, PatternAs, Spaced,
        StrLiteral, Tag, TypeAnnotation, TypeDef, TypeHeader, ValueDef, WhenBranch,
    },
    header::{
        AppHeader, ExposedName, HostedHeader, ImportsEntry, ModuleHeader, ModuleName, ModuleParams,
        PackageEntry, PackageHeader, PackageName, PlatformHeader, PlatformRequires, ProvidesTo, To,
        TypedIdent,
    },
    ident::{Accessor, UppercaseIdent},
};
use roc_region::all::{Loc, Region};
use tower_lsp::lsp_types::SemanticTokenType;

macro_rules! tokens {
    ($($(#[$meta:meta])* $token:ident => $lsp_token:literal),* $(,)?) => {
        pub enum Token {
            $(
                $(#[$meta])*
                $token
            ),*
        }

        fn _non_redundant_lsp_tokens() {
            match "" {
                $($lsp_token => (),)*
                _ => (),
            }
        }

        impl Token {
            pub const LEGEND: &'static [SemanticTokenType] = &[
                $(SemanticTokenType::new($lsp_token)),*
            ];
        }
    }
}

// Try to use predefined values at
//   https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_semanticTokens
tokens! {
    Module => "namespace",
    Type => "type",
    Ability => "interface",
    #[allow(unused)]
    TypeVariable => "typeParameter",
    #[allow(unused)]
    Parameter => "parameter",
    Variable => "variable",
    Field => "property",
    Tag => "enumMember",
    Function => "function",
    Keyword => "keyword",
    String => "string",
    Number => "number",
    Operator => "operator",
    Comment => "comment",
    Import => "import",
}

fn onetoken(token: Token, region: Region, arena: &Bump) -> BumpVec<Loc<Token>> {
    bumpvec![in arena; Loc::at(region, token)]
}

fn field_token(region: Region, arena: &Bump) -> BumpVec<Loc<Token>> {
    onetoken(Token::Field, region, arena)
}

trait HasToken {
    fn token(&self) -> Token;
}

impl<T: HasToken> HasToken for Spaced<'_, T> {
    fn token(&self) -> Token {
        self.item().token()
    }
}

impl HasToken for ModuleName<'_> {
    fn token(&self) -> Token {
        Token::Module
    }
}

impl HasToken for &str {
    fn token(&self) -> Token {
        if self.starts_with(|c: char| c.is_uppercase()) {
            Token::Type
        } else {
            Token::Variable
        }
    }
}

impl HasToken for ExposedName<'_> {
    fn token(&self) -> Token {
        self.as_str().token()
    }
}

impl HasToken for PackageName<'_> {
    fn token(&self) -> Token {
        Token::Module
    }
}

impl HasToken for StrLiteral<'_> {
    fn token(&self) -> Token {
        Token::String
    }
}

impl HasToken for UppercaseIdent<'_> {
    fn token(&self) -> Token {
        Token::Type
    }
}

impl HasToken for To<'_> {
    fn token(&self) -> Token {
        match self {
            To::ExistingPackage(_) => Token::Module,
            To::NewPackage(_) => Token::Module,
        }
    }
}

impl HasToken for BinOp {
    fn token(&self) -> Token {
        Token::Operator
    }
}

impl HasToken for UnaryOp {
    fn token(&self) -> Token {
        Token::Operator
    }
}

pub trait IterTokens {
    // Use a vec until "impl trait in trait" is stabilized
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>>;
}

impl<T: HasToken> IterTokens for Loc<T> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        onetoken(self.value.token(), self.region, arena)
    }
}

impl<T: IterTokens> IterTokens for Spaced<'_, T> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        self.item().iter_tokens(arena)
    }
}

impl<T: IterTokens> IterTokens for Collection<'_, T> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        self.items
            .iter()
            .flat_map(|item| item.iter_tokens(arena))
            .collect_in(arena)
    }
}

impl<T: IterTokens> IterTokens for &[T] {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        self.iter()
            .flat_map(|item| item.iter_tokens(arena))
            .collect_in(arena)
    }
}

impl<T: IterTokens, U: IterTokens> IterTokens for (T, U) {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        let (a, b) = self;
        a.iter_tokens(arena)
            .into_iter()
            .chain(b.iter_tokens(arena))
            .collect_in(arena)
    }
}

impl IterTokens for Header<'_> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        match self {
            Header::Module(mh) => mh.iter_tokens(arena),
            Header::App(app) => app.iter_tokens(arena),
            Header::Package(pkg) => pkg.iter_tokens(arena),
            Header::Platform(pf) => pf.iter_tokens(arena),
            Header::Hosted(h) => h.iter_tokens(arena),
        }
    }
}

impl IterTokens for ModuleHeader<'_> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        let Self {
            after_keyword: _,
            params,
            exposes,
            interface_imports: _,
        } = self;

        params
            .iter_tokens(arena)
            .into_iter()
            .chain(exposes.iter_tokens(arena))
            .collect_in(arena)
    }
}

impl<T> IterTokens for Option<T>
where
    T: IterTokens,
{
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        match self {
            Some(params) => params.iter_tokens(arena),
            None => bumpvec![in arena;],
        }
    }
}

impl IterTokens for ModuleParams<'_> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        let Self {
            pattern,
            before_arrow: _,
            after_arrow: _,
        } = self;

        pattern.value.iter_tokens(arena)
    }
}

impl IterTokens for AppHeader<'_> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        let Self {
            before_provides: _,
            provides,
            before_packages: _,
            packages,
            old_imports,
            old_provides_to_new_package: _,
        } = self;

        (provides.iter_tokens(arena).into_iter())
            .chain(packages.value.iter_tokens(arena))
            .chain(provides.iter_tokens(arena))
            .chain(old_imports.iter().flat_map(|i| i.item.iter_tokens(arena)))
            .collect_in(arena)
    }
}

impl IterTokens for PackageHeader<'_> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        let Self {
            before_exposes: _,
            exposes,
            before_packages: _,
            packages,
        } = self;

        (exposes.iter_tokens(arena).into_iter())
            .chain(packages.value.iter_tokens(arena))
            .collect_in(arena)
    }
}

impl IterTokens for PlatformHeader<'_> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        let Self {
            before_name: _,
            name,
            requires,
            exposes,
            packages,
            imports,
            provides,
        } = self;

        (name.iter_tokens(arena).into_iter())
            .chain(requires.item.iter_tokens(arena))
            .chain(exposes.item.iter_tokens(arena))
            .chain(packages.item.iter_tokens(arena))
            .chain(imports.item.iter_tokens(arena))
            .chain(provides.item.iter_tokens(arena))
            .collect_in(arena)
    }
}

impl IterTokens for HostedHeader<'_> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        let Self {
            before_name: _,
            name,
            exposes,
            imports,
        } = self;

        (name.iter_tokens(arena).into_iter())
            .chain(exposes.item.iter_tokens(arena))
            .chain(imports.item.iter_tokens(arena))
            .collect_in(arena)
    }
}

impl IterTokens for Loc<Spaced<'_, ImportsEntry<'_>>> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        match self.value.item() {
            ImportsEntry::Module(_module_name, names) => names.iter_tokens(arena),
            ImportsEntry::Package(_pkg, _module_name, names) => names.iter_tokens(arena),
            ImportsEntry::IngestedFile(_str, idents) => idents.iter_tokens(arena),
        }
    }
}

impl IterTokens for Loc<Spaced<'_, PackageEntry<'_>>> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        let PackageEntry {
            shorthand: _,
            spaces_after_shorthand: _,
            platform_marker: _,
            package_name,
        } = self.value.item();

        package_name.iter_tokens(arena)
    }
}

impl IterTokens for Loc<Spaced<'_, TypedIdent<'_>>> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        self.value.item().iter_tokens(arena)
    }
}

impl IterTokens for TypedIdent<'_> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        let Self {
            ident,
            spaces_before_colon: _,
            ann,
        } = self;

        (ident.iter_tokens(arena).into_iter())
            .chain(ann.iter_tokens(arena))
            .collect_in(arena)
    }
}

impl IterTokens for ProvidesTo<'_> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        let Self {
            provides_keyword: _,
            entries,
            types,
            to_keyword: _,
            to,
        } = self;

        (entries.iter_tokens(arena).into_iter())
            .chain(types.iter().flat_map(|t| t.iter_tokens(arena)))
            .chain(to.iter_tokens(arena))
            .collect_in(arena)
    }
}

impl IterTokens for PlatformRequires<'_> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        let Self { rigids, signatures } = self;

        (rigids.iter_tokens(arena).into_iter())
            .chain(signatures.iter_tokens(arena))
            .collect_in(arena)
    }
}

impl IterTokens for Loc<TypeAnnotation<'_>> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        match self.value {
            TypeAnnotation::Function(params, _arrow, ret) => {
                (params.iter_tokens(arena).into_iter())
                    .chain(ret.iter_tokens(arena))
                    .collect_in(arena)
            }
            TypeAnnotation::Apply(_mod, _type, args) => args.iter_tokens(arena),
            TypeAnnotation::BoundVariable(_) => onetoken(Token::Type, self.region, arena),
            TypeAnnotation::As(ty, _, as_ty) => (ty.iter_tokens(arena).into_iter())
                .chain(as_ty.iter_tokens(arena))
                .collect_in(arena),
            TypeAnnotation::Record { fields, ext } => (fields.iter_tokens(arena).into_iter())
                .chain(ext.iter().flat_map(|t| t.iter_tokens(arena)))
                .collect_in(arena),
            TypeAnnotation::Tuple { elems, ext } => (elems.iter_tokens(arena).into_iter())
                .chain(ext.iter().flat_map(|t| t.iter_tokens(arena)))
                .collect_in(arena),
            TypeAnnotation::TagUnion { tags, ext } => (tags.iter_tokens(arena).into_iter())
                .chain(ext.iter().flat_map(|t| t.iter_tokens(arena)))
                .collect_in(arena),
            TypeAnnotation::Inferred => onetoken(Token::Type, self.region, arena),
            TypeAnnotation::Wildcard => onetoken(Token::Type, self.region, arena),
            TypeAnnotation::Where(ty, implements) => (ty.iter_tokens(arena).into_iter())
                .chain(implements.iter_tokens(arena))
                .collect_in(arena),
            TypeAnnotation::SpaceBefore(ty, _) | TypeAnnotation::SpaceAfter(ty, _) => {
                Loc::at(self.region, *ty).iter_tokens(arena)
            }
            TypeAnnotation::Malformed(_) => bumpvec![in arena;],
        }
    }
}

impl<T> IterTokens for Loc<AssignedField<'_, T>>
where
    Loc<T>: IterTokens,
{
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        self.value.iter_tokens(arena)
    }
}

impl<T> IterTokens for AssignedField<'_, T>
where
    Loc<T>: IterTokens,
{
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        match self {
            AssignedField::RequiredValue(field, _, ty)
            | AssignedField::OptionalValue(field, _, ty)
            | AssignedField::IgnoredValue(field, _, ty) => (field_token(field.region, arena)
                .into_iter())
            .chain(ty.iter_tokens(arena))
            .collect_in(arena),
            AssignedField::LabelOnly(s) => s.iter_tokens(arena),
            AssignedField::SpaceBefore(af, _) | AssignedField::SpaceAfter(af, _) => {
                af.iter_tokens(arena)
            }
        }
    }
}

impl IterTokens for Loc<Tag<'_>> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        self.value.iter_tokens(arena)
    }
}

impl IterTokens for Tag<'_> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        match self {
            Tag::Apply { name, args } => (onetoken(Token::Tag, name.region, arena).into_iter())
                .chain(args.iter_tokens(arena))
                .collect_in(arena),
            Tag::SpaceBefore(t, _) | Tag::SpaceAfter(t, _) => t.iter_tokens(arena),
        }
    }
}

impl IterTokens for TypeHeader<'_> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        let Self { name, vars } = self;

        (name.iter_tokens(arena).into_iter())
            .chain(vars.iter().map(|v| v.with_value(Token::Type)))
            .collect_in(arena)
    }
}

impl IterTokens for Loc<ImplementsClause<'_>> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        self.value.iter_tokens(arena)
    }
}

impl IterTokens for ImplementsClause<'_> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        let Self { var, abilities } = self;
        (var.iter_tokens(arena).into_iter())
            .chain(abilities.iter_tokens(arena))
            .collect_in(arena)
    }
}

impl IterTokens for Defs<'_> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        self.defs()
            .flat_map(|item| match item {
                Ok(type_def) => type_def.iter_tokens(arena),
                Err(value_def) => value_def.iter_tokens(arena),
            })
            .collect_in(arena)
    }
}

impl IterTokens for TypeDef<'_> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        match self {
            TypeDef::Alias { header, ann } => (header.iter_tokens(arena).into_iter())
                .chain(ann.iter_tokens(arena))
                .collect_in(arena),
            TypeDef::Opaque {
                header,
                typ,
                derived,
            } => (header.iter_tokens(arena).into_iter())
                .chain(typ.iter_tokens(arena))
                .chain(derived.iter().flat_map(|t| t.iter_tokens(arena)))
                .collect_in(arena),
            TypeDef::Ability {
                header: TypeHeader { name, vars },
                loc_implements,
                members,
            } => (onetoken(Token::Ability, name.region, arena).into_iter())
                .chain(vars.iter().map(|v| v.with_value(Token::Type)))
                .chain(loc_implements.iter_tokens(arena))
                .chain(members.iter_tokens(arena))
                .collect_in(arena),
        }
    }
}

impl IterTokens for Loc<ImplementsAbilities<'_>> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        self.value.iter_tokens(arena)
    }
}

impl IterTokens for ImplementsAbilities<'_> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        match self {
            ImplementsAbilities::Implements(impls) => impls.iter_tokens(arena),
            ImplementsAbilities::SpaceBefore(i, _) | ImplementsAbilities::SpaceAfter(i, _) => {
                i.iter_tokens(arena)
            }
        }
    }
}

impl IterTokens for Loc<ImplementsAbility<'_>> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        self.value.iter_tokens(arena)
    }
}

impl IterTokens for ImplementsAbility<'_> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        match self {
            ImplementsAbility::ImplementsAbility { ability, impls } => {
                (ability.iter_tokens(arena).into_iter())
                    .chain(impls.iter().flat_map(|i| i.iter_tokens(arena)))
                    .collect_in(arena)
            }
            ImplementsAbility::SpaceBefore(ia, _) | ImplementsAbility::SpaceAfter(ia, _) => {
                ia.iter_tokens(arena)
            }
        }
    }
}

impl IterTokens for Loc<AbilityImpls<'_>> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        self.value.iter_tokens(arena)
    }
}

impl IterTokens for AbilityImpls<'_> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        match self {
            AbilityImpls::AbilityImpls(fields) => fields.iter_tokens(arena),
            AbilityImpls::SpaceBefore(ai, _) | AbilityImpls::SpaceAfter(ai, _) => {
                ai.iter_tokens(arena)
            }
        }
    }
}

impl IterTokens for Loc<Implements<'_>> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        match self.value {
            Implements::Implements => onetoken(Token::Keyword, self.region, arena),
            Implements::SpaceBefore(i, _) | Implements::SpaceAfter(i, _) => {
                Loc::at(self.region, *i).iter_tokens(arena)
            }
        }
    }
}

impl IterTokens for AbilityMember<'_> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        let Self { name, typ } = self;
        (onetoken(Token::Function, name.region, arena).into_iter())
            .chain(typ.iter_tokens(arena))
            .collect_in(arena)
    }
}

impl IterTokens for ValueDef<'_> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        match self {
            ValueDef::Annotation(pattern, annotation) => (pattern.iter_tokens(arena).into_iter())
                .chain(annotation.iter_tokens(arena))
                .collect_in(arena),
            ValueDef::Body(pattern, body) => (pattern.iter_tokens(arena).into_iter())
                .chain(body.iter_tokens(arena))
                .collect_in(arena),
            ValueDef::AnnotatedBody {
                ann_pattern,
                ann_type,
                lines_between: _,
                body_pattern,
                body_expr,
            } => (ann_pattern.iter_tokens(arena).into_iter())
                .chain(ann_type.iter_tokens(arena))
                .chain(body_pattern.iter_tokens(arena))
                .chain(body_expr.iter_tokens(arena))
                .collect_in(arena),
            ValueDef::Dbg {
                preceding_comment,
                condition,
            }
            | ValueDef::Expect {
                preceding_comment,
                condition,
            } => (onetoken(Token::Comment, *preceding_comment, arena).into_iter())
                .chain(condition.iter_tokens(arena))
                .collect_in(arena),
            ValueDef::ModuleImport(import) => onetoken(Token::Import, import.name.region, arena),
            ValueDef::IngestedFileImport(import) => {
                onetoken(Token::Import, import.name.item.region, arena)
            }
            ValueDef::Stmt(loc_expr) => loc_expr.iter_tokens(arena),
            ValueDef::StmtAfterExpr => BumpVec::new_in(arena),
        }
    }
}

impl IterTokens for &Loc<Expr<'_>> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        (**self).iter_tokens(arena)
    }
}

impl IterTokens for Loc<Expr<'_>> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        let region = self.region;
        match self.value {
            Expr::Float(_) => onetoken(Token::Number, region, arena),
            Expr::Num(_) => onetoken(Token::Number, region, arena),
            Expr::NonBase10Int { .. } => onetoken(Token::Number, region, arena),
            Expr::Str(_) => onetoken(Token::String, region, arena),
            Expr::SingleQuote(_) => onetoken(Token::String, region, arena),
            Expr::RecordAccess(rcd, _field) => Loc::at(region, *rcd).iter_tokens(arena),
            Expr::AccessorFunction(accessor) => Loc::at(region, accessor).iter_tokens(arena),
            Expr::RecordUpdater(updater) => Loc::at(region, updater).iter_tokens(arena),
            Expr::TupleAccess(tup, _field) => Loc::at(region, *tup).iter_tokens(arena),
            Expr::TrySuffix { expr: inner, .. } => Loc::at(region, *inner).iter_tokens(arena),
            Expr::List(lst) => lst.iter_tokens(arena),
            Expr::RecordUpdate { update, fields } => (update.iter_tokens(arena).into_iter())
                .chain(fields.iter().flat_map(|f| f.iter_tokens(arena)))
                .collect_in(arena),
            Expr::Record(rcd) => rcd.iter_tokens(arena),
            Expr::Tuple(tup) => tup.iter_tokens(arena),
            Expr::RecordBuilder { mapper, fields } => (mapper.iter_tokens(arena).into_iter())
                .chain(fields.iter().flat_map(|f| f.iter_tokens(arena)))
                .collect_in(arena),
            Expr::Var { .. } => onetoken(Token::Variable, region, arena),
            Expr::Underscore(_) => onetoken(Token::Variable, region, arena),
            Expr::Crash => onetoken(Token::Keyword, region, arena),
            Expr::Tag(_) => onetoken(Token::Tag, region, arena),
            Expr::OpaqueRef(_) => onetoken(Token::Type, region, arena),
            Expr::Closure(patterns, body) => (patterns.iter_tokens(arena).into_iter())
                .chain(body.iter_tokens(arena))
                .collect_in(arena),
            Expr::Defs(defs, exprs) => (defs.iter_tokens(arena).into_iter())
                .chain(exprs.iter_tokens(arena))
                .collect_in(arena),
            Expr::Dbg => onetoken(Token::Keyword, region, arena),
            Expr::DbgStmt {
                first,
                extra_args,
                continuation,
            } => (first.iter_tokens(arena).into_iter())
                .chain(extra_args.iter_tokens(arena))
                .chain(continuation.iter_tokens(arena))
                .collect_in(arena),
            Expr::LowLevelDbg(_, e1, e2) => (e1.iter_tokens(arena).into_iter())
                .chain(e2.iter_tokens(arena))
                .collect_in(arena),
            Expr::Try => onetoken(Token::Keyword, region, arena),
            Expr::LowLevelTry(e1, _) => e1.iter_tokens(arena),
            Expr::Apply(e1, e2, _called_via) => (e1.iter_tokens(arena).into_iter())
                .chain(e2.iter_tokens(arena))
                .collect_in(arena),
            Expr::BinOps(e1, e2) => (e1.iter_tokens(arena).into_iter())
                .chain(e2.iter_tokens(arena))
                .collect_in(arena),
            Expr::UnaryOp(e1, op) => (op.iter_tokens(arena).into_iter())
                .chain(e1.iter_tokens(arena))
                .collect_in(arena),
            Expr::If {
                if_thens: e1,
                final_else: e2,
                ..
            } => (e1.iter_tokens(arena).into_iter())
                .chain(e2.iter_tokens(arena))
                .collect_in(arena),
            Expr::When(e, branches) => (e.iter_tokens(arena).into_iter())
                .chain(branches.iter_tokens(arena))
                .collect_in(arena),
            Expr::Return(ret_expr, after_ret) => ret_expr
                .iter_tokens(arena)
                .into_iter()
                .chain(after_ret.iter_tokens(arena))
                .collect_in(arena),
            Expr::SpaceBefore(e, _) | Expr::SpaceAfter(e, _) => {
                Loc::at(region, *e).iter_tokens(arena)
            }
            Expr::ParensAround(e) => Loc::at(region, *e).iter_tokens(arena),
            Expr::EmptyRecordBuilder(e) => e.iter_tokens(arena),
            Expr::SingleFieldRecordBuilder(e) => e.iter_tokens(arena),
            Expr::OptionalFieldInRecordBuilder(_name, e) => e.iter_tokens(arena),
            Expr::MalformedIdent(_, _)
            | Expr::PrecedenceConflict(_)
            | Expr::MalformedSuffixed(_) => {
                bumpvec![in arena;]
            }
        }
    }
}

impl IterTokens for Loc<Accessor<'_>> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        match self.value {
            Accessor::RecordField(_) => onetoken(Token::Function, self.region, arena),
            Accessor::TupleIndex(_) => onetoken(Token::Function, self.region, arena),
        }
    }
}

impl IterTokens for &WhenBranch<'_> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        let WhenBranch {
            patterns,
            value,
            guard,
        } = self;

        (patterns.iter_tokens(arena).into_iter())
            .chain(value.iter_tokens(arena))
            .chain(guard.iter().flat_map(|g| g.iter_tokens(arena)))
            .collect_in(arena)
    }
}

impl IterTokens for Loc<Pattern<'_>> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        let region = self.region;
        match self.value {
            Pattern::Identifier { .. } => onetoken(Token::Variable, region, arena),
            Pattern::Tag(_) => onetoken(Token::Tag, region, arena),
            Pattern::OpaqueRef(_) => onetoken(Token::Type, region, arena),
            Pattern::Apply(p1, p2, _) => (p1.iter_tokens(arena).into_iter())
                .chain(p2.iter_tokens(arena))
                .collect_in(arena),
            Pattern::RecordDestructure(ps) => ps.iter_tokens(arena),
            Pattern::RequiredField(_field, p) => p.iter_tokens(arena),
            Pattern::OptionalField(_field, p) => p.iter_tokens(arena),
            Pattern::NumLiteral(_) => onetoken(Token::Number, region, arena),
            Pattern::NonBase10Literal { .. } => onetoken(Token::Number, region, arena),
            Pattern::FloatLiteral(_) => onetoken(Token::Number, region, arena),
            Pattern::StrLiteral(_) => onetoken(Token::String, region, arena),
            Pattern::Underscore(_) => onetoken(Token::Variable, region, arena),
            Pattern::SingleQuote(_) => onetoken(Token::String, region, arena),
            Pattern::Tuple(ps) => ps.iter_tokens(arena),
            Pattern::List(ps) => ps.iter_tokens(arena),
            Pattern::ListRest(None) => bumpvec![in arena;],
            Pattern::ListRest(Some((_, pas))) => pas.iter_tokens(arena),
            Pattern::As(p1, pas) => (p1.iter_tokens(arena).into_iter())
                .chain(pas.iter_tokens(arena))
                .collect_in(arena),
            Pattern::SpaceBefore(p, _) | Pattern::SpaceAfter(p, _) => {
                Loc::at(region, *p).iter_tokens(arena)
            }
            Pattern::QualifiedIdentifier { .. } => onetoken(Token::Variable, region, arena),
            Pattern::Malformed(_) | Pattern::MalformedIdent(_, _) => bumpvec![in arena;],
        }
    }
}

impl IterTokens for PatternAs<'_> {
    fn iter_tokens<'a>(&self, arena: &'a Bump) -> BumpVec<'a, Loc<Token>> {
        let Self {
            spaces_before: _,
            identifier,
        } = self;

        onetoken(Token::Variable, identifier.region, arena)
    }
}
