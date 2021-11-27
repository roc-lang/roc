use std::path::PathBuf;

use bumpalo::collections::{String, Vec};
use bumpalo::Bump;
use roc_fmt::def::fmt_def;
use roc_fmt::module::fmt_module;
use roc_module::called_via::{BinOp, UnaryOp};
use roc_parse::ast::{AssignedField, Collection, Expr, Pattern, Tag, TypeAnnotation, WhenBranch};
use roc_parse::header::{
    AppHeader, Effects, ExposesEntry, ImportsEntry, InterfaceHeader, ModuleName, PackageEntry,
    PlatformHeader, PlatformRequires, PlatformRigid, TypedIdent,
};
use roc_parse::{
    ast::{Def, Module},
    module::{self, module_defs},
    parser::{Parser, State, SyntaxError},
};
use roc_region::all::Located;
use roc_reporting::{internal_error, user_error};

pub fn format(files: std::vec::Vec<PathBuf>) {
    for file in files {
        let arena = Bump::new();

        let src = std::fs::read_to_string(&file).unwrap();

        let ast = arena.alloc(parse_all(&arena, &src).unwrap_or_else(|e| {
            user_error!("Unexpected parse failure when parsing this formatting:\n\n{:?}\n\nParse error was:\n\n{:?}\n\n", src, e)
        }));
        let mut buf = String::new_in(&arena);
        fmt_all(&arena, &mut buf, ast);

        let reparsed_ast = arena.alloc(parse_all(&arena, &buf).unwrap_or_else(|e| {
            let mut fail_file = file.clone();
            fail_file.set_extension("roc-format-failed");
            std::fs::write(&fail_file, &buf).unwrap();
            internal_error!(
                "Formatting bug; formatted code isn't valid\n\n\
                I wrote the incorrect result to this file for debugging purposes:\n{}\n\n\
                Parse error was: {:?}\n\n",
                fail_file.display(),
                e
            );
        }));

        let ast = ast.remove_spaces(&arena);
        let reparsed_ast = reparsed_ast.remove_spaces(&arena);

        // HACK!
        // We compare the debug format strings of the ASTs, because I'm finding in practice that _somewhere_ deep inside the ast,
        // the PartialEq implementation is returning `false` even when the Debug-formatted impl is exactly the same.
        // I don't have the patience to debug this right now, so let's leave it for another day...
        // TODO: fix PartialEq impl on ast types
        if format!("{:?}", ast) != format!("{:?}", reparsed_ast) {
            let mut fail_file = file.clone();
            fail_file.set_extension("roc-format-failed");
            std::fs::write(&fail_file, &buf).unwrap();

            let mut before_file = file.clone();
            before_file.set_extension("roc-format-failed-ast-before");
            std::fs::write(&before_file, &format!("{:#?}\n", ast)).unwrap();

            let mut after_file = file.clone();
            after_file.set_extension("roc-format-failed-ast-after");
            std::fs::write(&after_file, &format!("{:#?}\n", reparsed_ast)).unwrap();

            internal_error!(
                "Formatting bug; formatting didn't reparse as the same tree\n\n\
                I wrote the incorrect result to this file for debugging purposes:\n{}\n\n\
                I wrote the tree before and after formatting to these files for debugging purposes:\n{}\n{}\n\n",
                fail_file.display(),
                before_file.display(),
                after_file.display());
        }

        std::fs::write(&file, &buf).unwrap();
    }
}

#[derive(Debug, PartialEq)]
struct Ast<'a> {
    module: Module<'a>,
    defs: Vec<'a, Located<Def<'a>>>,
}

fn parse_all<'a>(arena: &'a Bump, src: &'a str) -> Result<Ast<'a>, SyntaxError<'a>> {
    let (module, state) =
        module::parse_header(arena, State::new(src.as_bytes())).map_err(SyntaxError::Header)?;

    let (_, defs, _) = module_defs().parse(arena, state).map_err(|(_, e, _)| e)?;

    Ok(Ast { module, defs })
}

fn fmt_all<'a>(arena: &'a Bump, buf: &mut String<'a>, ast: &'a Ast) {
    fmt_module(buf, &ast.module);
    for def in &ast.defs {
        fmt_def(buf, arena.alloc(def.value), 0);
    }
}

/// RemoveSpaces normalizes the ast to something that we _expect_ to be invariant under formatting.
///
/// Currently this consists of:
/// * Removing newlines
/// * Removing comments
/// * Removing parens in Exprs
///
/// Long term, we actuall want this transform to preserve comments (so we can assert they're maintained by formatting)
/// - but there are currently several bugs where they're _not_ preserved.
/// TODO: ensure formatting retains comments
trait RemoveSpaces<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self;
}

impl<'a> RemoveSpaces<'a> for Ast<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        Ast {
            module: self.module.remove_spaces(arena),
            defs: {
                let mut defs = Vec::with_capacity_in(self.defs.len(), arena);
                for d in &self.defs {
                    defs.push(d.remove_spaces(arena))
                }
                defs
            },
        }
    }
}

impl<'a> RemoveSpaces<'a> for Module<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match self {
            Module::Interface { header } => Module::Interface {
                header: InterfaceHeader {
                    name: header.name,
                    exposes: header.exposes.remove_spaces(arena),
                    imports: header.imports.remove_spaces(arena),
                    before_header: &[],
                    after_interface_keyword: &[],
                    before_exposes: &[],
                    after_exposes: &[],
                    before_imports: &[],
                    after_imports: &[],
                },
            },
            Module::App { header } => Module::App {
                header: AppHeader {
                    name: header.name,
                    packages: header.packages.remove_spaces(arena),
                    imports: header.imports.remove_spaces(arena),
                    provides: header.provides.remove_spaces(arena),
                    to: header.to,
                    before_header: &[],
                    after_app_keyword: &[],
                    before_packages: &[],
                    after_packages: &[],
                    before_imports: &[],
                    after_imports: &[],
                    before_provides: &[],
                    after_provides: &[],
                    before_to: &[],
                    after_to: &[],
                },
            },
            Module::Platform { header } => Module::Platform {
                header: PlatformHeader {
                    name: header.name,
                    requires: header.requires.remove_spaces(arena),
                    exposes: header.exposes.remove_spaces(arena),
                    packages: header.packages.remove_spaces(arena),
                    imports: header.imports.remove_spaces(arena),
                    provides: header.provides.remove_spaces(arena),
                    effects: Effects {
                        spaces_before_effects_keyword: &[],
                        spaces_after_effects_keyword: &[],
                        spaces_after_type_name: &[],
                        effect_shortname: header.effects.effect_shortname.remove_spaces(arena),
                        effect_type_name: header.effects.effect_type_name.remove_spaces(arena),
                        entries: header.effects.entries.remove_spaces(arena),
                    },
                    before_header: &[],
                    after_platform_keyword: &[],
                    before_requires: &[],
                    after_requires: &[],
                    before_exposes: &[],
                    after_exposes: &[],
                    before_packages: &[],
                    after_packages: &[],
                    before_imports: &[],
                    after_imports: &[],
                    before_provides: &[],
                    after_provides: &[],
                },
            },
        }
    }
}

impl<'a> RemoveSpaces<'a> for &'a str {
    fn remove_spaces(&self, _arena: &'a Bump) -> Self {
        self
    }
}

impl<'a, T: RemoveSpaces<'a> + Copy> RemoveSpaces<'a> for ExposesEntry<'a, T> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            ExposesEntry::Exposed(a) => ExposesEntry::Exposed(a.remove_spaces(arena)),
            ExposesEntry::SpaceBefore(a, _) => a.remove_spaces(arena),
            ExposesEntry::SpaceAfter(a, _) => a.remove_spaces(arena),
        }
    }
}

impl<'a> RemoveSpaces<'a> for ModuleName<'a> {
    fn remove_spaces(&self, _arena: &'a Bump) -> Self {
        *self
    }
}

impl<'a> RemoveSpaces<'a> for TypedIdent<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            TypedIdent::Entry {
                ident,
                spaces_before_colon: _,
                ann,
            } => TypedIdent::Entry {
                ident: ident.remove_spaces(arena),
                spaces_before_colon: &[],
                ann: ann.remove_spaces(arena),
            },
            TypedIdent::SpaceBefore(a, _) => a.remove_spaces(arena),
            TypedIdent::SpaceAfter(a, _) => a.remove_spaces(arena),
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

impl<'a> RemoveSpaces<'a> for PlatformRigid<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            PlatformRigid::Entry { rigid, alias } => PlatformRigid::Entry { rigid, alias },
            PlatformRigid::SpaceBefore(a, _) => a.remove_spaces(arena),
            PlatformRigid::SpaceAfter(a, _) => a.remove_spaces(arena),
        }
    }
}

impl<'a> RemoveSpaces<'a> for PackageEntry<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            PackageEntry::Entry {
                shorthand,
                spaces_after_shorthand: _,
                package_or_path,
            } => PackageEntry::Entry {
                shorthand,
                spaces_after_shorthand: &[],
                package_or_path,
            },
            PackageEntry::SpaceBefore(a, _) => a.remove_spaces(arena),
            PackageEntry::SpaceAfter(a, _) => a.remove_spaces(arena),
        }
    }
}

impl<'a> RemoveSpaces<'a> for ImportsEntry<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            ImportsEntry::Module(a, b) => ImportsEntry::Module(a, b.remove_spaces(arena)),
            ImportsEntry::Package(a, b, c) => ImportsEntry::Package(a, b, c.remove_spaces(arena)),
            ImportsEntry::SpaceBefore(a, _) => a.remove_spaces(arena),
            ImportsEntry::SpaceAfter(a, _) => a.remove_spaces(arena),
        }
    }
}

impl<'a, T: RemoveSpaces<'a>> RemoveSpaces<'a> for Option<T> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        self.as_ref().map(|a| a.remove_spaces(arena))
    }
}

impl<'a, T: RemoveSpaces<'a> + std::fmt::Debug> RemoveSpaces<'a> for Located<T> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        let res = self.value.remove_spaces(arena);
        Located::new(0, 0, 0, 0, res)
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

impl<'a> RemoveSpaces<'a> for Def<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            Def::Annotation(a, b) => {
                Def::Annotation(a.remove_spaces(arena), b.remove_spaces(arena))
            }
            Def::Alias { name, vars, ann } => Def::Alias {
                name: name.remove_spaces(arena),
                vars: vars.remove_spaces(arena),
                ann: ann.remove_spaces(arena),
            },
            Def::Body(a, b) => Def::Body(
                arena.alloc(a.remove_spaces(arena)),
                arena.alloc(b.remove_spaces(arena)),
            ),
            Def::AnnotatedBody {
                ann_pattern,
                ann_type,
                comment: _,
                body_pattern,
                body_expr,
            } => Def::AnnotatedBody {
                ann_pattern: arena.alloc(ann_pattern.remove_spaces(arena)),
                ann_type: arena.alloc(ann_type.remove_spaces(arena)),
                comment: None,
                body_pattern: arena.alloc(body_pattern.remove_spaces(arena)),
                body_expr: arena.alloc(body_expr.remove_spaces(arena)),
            },
            Def::Expect(a) => Def::Expect(arena.alloc(a.remove_spaces(arena))),
            Def::NotYetImplemented(a) => Def::NotYetImplemented(a),
            Def::SpaceBefore(a, _) | Def::SpaceAfter(a, _) => a.remove_spaces(arena),
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
            Expr::Str(a) => Expr::Str(a),
            Expr::Access(a, b) => Expr::Access(arena.alloc(a.remove_spaces(arena)), b),
            Expr::AccessorFunction(a) => Expr::AccessorFunction(a),
            Expr::List(a) => Expr::List(a.remove_spaces(arena)),
            Expr::RecordUpdate { update, fields } => Expr::RecordUpdate {
                update: arena.alloc(update.remove_spaces(arena)),
                fields: fields.remove_spaces(arena),
            },
            Expr::Record(a) => Expr::Record(a.remove_spaces(arena)),
            Expr::Var { module_name, ident } => Expr::Var { module_name, ident },
            Expr::Underscore(a) => Expr::Underscore(a),
            Expr::GlobalTag(a) => Expr::GlobalTag(a),
            Expr::PrivateTag(a) => Expr::PrivateTag(a),
            Expr::Closure(a, b) => Expr::Closure(
                arena.alloc(a.remove_spaces(arena)),
                arena.alloc(b.remove_spaces(arena)),
            ),
            Expr::Defs(a, b) => {
                Expr::Defs(a.remove_spaces(arena), arena.alloc(b.remove_spaces(arena)))
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
            Expr::MalformedIdent(a, b) => Expr::MalformedIdent(a, b),
            Expr::MalformedClosure => Expr::MalformedClosure,
            Expr::PrecedenceConflict(a) => Expr::PrecedenceConflict(a),
            Expr::SpaceBefore(a, _) => a.remove_spaces(arena),
            Expr::SpaceAfter(a, _) => a.remove_spaces(arena),
        }
    }
}

impl<'a> RemoveSpaces<'a> for Pattern<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            Pattern::Identifier(a) => Pattern::Identifier(a),
            Pattern::GlobalTag(a) => Pattern::GlobalTag(a),
            Pattern::PrivateTag(a) => Pattern::PrivateTag(a),
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
            Pattern::MalformedIdent(a, b) => Pattern::MalformedIdent(a, b),
            Pattern::QualifiedIdentifier { module_name, ident } => {
                Pattern::QualifiedIdentifier { module_name, ident }
            }
            Pattern::SpaceBefore(a, _) => a.remove_spaces(arena),
            Pattern::SpaceAfter(a, _) => a.remove_spaces(arena),
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
            TypeAnnotation::As(a, _, c) => TypeAnnotation::As(
                arena.alloc(a.remove_spaces(arena)),
                &[],
                arena.alloc(c.remove_spaces(arena)),
            ),
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
            TypeAnnotation::SpaceBefore(a, _) => a.remove_spaces(arena),
            TypeAnnotation::SpaceAfter(a, _) => a.remove_spaces(arena),
            TypeAnnotation::Malformed(a) => TypeAnnotation::Malformed(a),
        }
    }
}
impl<'a> RemoveSpaces<'a> for Tag<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            Tag::Global { name, args } => Tag::Global {
                name: name.remove_spaces(arena),
                args: args.remove_spaces(arena),
            },
            Tag::Private { name, args } => Tag::Private {
                name: name.remove_spaces(arena),
                args: args.remove_spaces(arena),
            },
            Tag::Malformed(a) => Tag::Malformed(a),
            Tag::SpaceBefore(a, _) => a.remove_spaces(arena),
            Tag::SpaceAfter(a, _) => a.remove_spaces(arena),
        }
    }
}
