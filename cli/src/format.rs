use std::ffi::OsStr;
use std::path::{Path, PathBuf};

use crate::FormatMode;
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_error_macros::{internal_error, user_error};
use roc_fmt::def::fmt_def;
use roc_fmt::module::fmt_module;
use roc_fmt::Buf;
use roc_module::called_via::{BinOp, UnaryOp};
use roc_parse::ast::{
    AbilityMember, AssignedField, Collection, Expr, Has, HasClause, Pattern, Spaced, StrLiteral,
    StrSegment, Tag, TypeAnnotation, TypeDef, TypeHeader, ValueDef, WhenBranch,
};
use roc_parse::header::{
    AppHeader, ExposedName, HostedHeader, ImportsEntry, InterfaceHeader, ModuleName, PackageEntry,
    PackageName, PlatformHeader, PlatformRequires, To, TypedIdent,
};
use roc_parse::{
    ast::{Def, Module},
    ident::UppercaseIdent,
    module::{self, module_defs},
    parser::{Parser, SyntaxError},
    state::State,
};
use roc_region::all::{Loc, Region};

fn flatten_directories(files: std::vec::Vec<PathBuf>) -> std::vec::Vec<PathBuf> {
    let mut to_flatten = files;
    let mut files = vec![];

    while let Some(path) = to_flatten.pop() {
        if path.is_dir() {
            match path.read_dir() {
                Ok(directory) => {
                    for item in directory {
                        match item {
                            Ok(file) => {
                                let file_path = file.path();
                                if file_path.is_dir() {
                                    to_flatten.push(file_path);
                                } else if is_roc_file(&file_path) {
                                    files.push(file_path);
                                }
                            }

                            Err(error) => internal_error!(
                                "There was an error while trying to read a file from a directory: {:?}",
                                error
                            ),
                        }
                    }
                }

                Err(error) => internal_error!(
                    "There was an error while trying to read the contents of a directory: {:?}",
                    error
                ),
            }
        } else if is_roc_file(&path) {
            files.push(path);
        }
    }

    files
}

fn is_roc_file(path: &Path) -> bool {
    let ext = path.extension().and_then(OsStr::to_str);
    return matches!(ext, Some("roc"));
}

pub fn format(files: std::vec::Vec<PathBuf>, mode: FormatMode) -> Result<(), String> {
    let files = flatten_directories(files);

    for file in files {
        let arena = Bump::new();

        let src = std::fs::read_to_string(&file).unwrap();

        let ast = arena.alloc(parse_all(&arena, &src).unwrap_or_else(|e| {
            user_error!("Unexpected parse failure when parsing this formatting:\n\n{:?}\n\nParse error was:\n\n{:?}\n\n", src, e)
        }));
        let mut buf = Buf::new_in(&arena);
        fmt_all(&arena, &mut buf, ast);

        let reparsed_ast = arena.alloc(parse_all(&arena, buf.as_str()).unwrap_or_else(|e| {
            let mut fail_file = file.clone();
            fail_file.set_extension("roc-format-failed");
            std::fs::write(&fail_file, buf.as_str()).unwrap();
            internal_error!(
                "Formatting bug; formatted code isn't valid\n\n\
                I wrote the incorrect result to this file for debugging purposes:\n{}\n\n\
                Parse error was: {:?}\n\n",
                fail_file.display(),
                e
            );
        }));

        let ast_normalized = ast.remove_spaces(&arena);
        let reparsed_ast_normalized = reparsed_ast.remove_spaces(&arena);

        // HACK!
        // We compare the debug format strings of the ASTs, because I'm finding in practice that _somewhere_ deep inside the ast,
        // the PartialEq implementation is returning `false` even when the Debug-formatted impl is exactly the same.
        // I don't have the patience to debug this right now, so let's leave it for another day...
        // TODO: fix PartialEq impl on ast types
        if format!("{:?}", ast_normalized) != format!("{:?}", reparsed_ast_normalized) {
            let mut fail_file = file.clone();
            fail_file.set_extension("roc-format-failed");
            std::fs::write(&fail_file, buf.as_str()).unwrap();

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

        // Now verify that the resultant formatting is _stable_ - i.e. that it doesn't change again if re-formatted
        let mut reformatted_buf = Buf::new_in(&arena);
        fmt_all(&arena, &mut reformatted_buf, reparsed_ast);
        if buf.as_str() != reformatted_buf.as_str() {
            let mut unstable_1_file = file.clone();
            unstable_1_file.set_extension("roc-format-unstable-1");
            std::fs::write(&unstable_1_file, buf.as_str()).unwrap();

            let mut unstable_2_file = file.clone();
            unstable_2_file.set_extension("roc-format-unstable-2");
            std::fs::write(&unstable_2_file, reformatted_buf.as_str()).unwrap();

            internal_error!(
                "Formatting bug; formatting is not stable. Reformatting the formatted file changed it again.\n\n\
                I wrote the result of formatting to this file for debugging purposes:\n{}\n\n\
                I wrote the result of double-formatting here:\n{}\n\n",
                unstable_1_file.display(),
                unstable_2_file.display());
        }

        match mode {
            FormatMode::CheckOnly => {
                // If we notice that this file needs to be formatted, return early
                if buf.as_str() != src {
                    return Err("One or more files need to be reformatted.".to_string());
                }
            }

            FormatMode::Format => {
                // If all the checks above passed, actually write out the new file.
                std::fs::write(&file, buf.as_str()).unwrap();
            }
        }
    }

    Ok(())
}

#[derive(Debug, PartialEq)]
struct Ast<'a> {
    module: Module<'a>,
    defs: Vec<'a, Loc<Def<'a>>>,
}

fn parse_all<'a>(arena: &'a Bump, src: &'a str) -> Result<Ast<'a>, SyntaxError<'a>> {
    let (module, state) = module::parse_header(arena, State::new(src.as_bytes()))
        .map_err(|e| SyntaxError::Header(e.problem))?;

    let (_, defs, _) = module_defs().parse(arena, state).map_err(|(_, e, _)| e)?;

    Ok(Ast { module, defs })
}

fn fmt_all<'a>(arena: &'a Bump, buf: &mut Buf<'a>, ast: &'a Ast) {
    fmt_module(buf, &ast.module);
    for def in &ast.defs {
        fmt_def(buf, arena.alloc(def.value), 0);
    }

    buf.fmt_end_of_file();
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
                    name: header.name.remove_spaces(arena),
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
                    name: header.name.remove_spaces(arena),
                    packages: header.packages.remove_spaces(arena),
                    imports: header.imports.remove_spaces(arena),
                    provides: header.provides.remove_spaces(arena),
                    provides_types: header.provides_types.map(|ts| ts.remove_spaces(arena)),
                    to: header.to.remove_spaces(arena),
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
                    name: header.name.remove_spaces(arena),
                    requires: header.requires.remove_spaces(arena),
                    exposes: header.exposes.remove_spaces(arena),
                    packages: header.packages.remove_spaces(arena),
                    imports: header.imports.remove_spaces(arena),
                    provides: header.provides.remove_spaces(arena),
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
            Module::Hosted { header } => Module::Hosted {
                header: HostedHeader {
                    name: header.name.remove_spaces(arena),
                    exposes: header.exposes.remove_spaces(arena),
                    imports: header.imports.remove_spaces(arena),
                    generates: header.generates.remove_spaces(arena),
                    generates_with: header.generates_with.remove_spaces(arena),
                    before_header: &[],
                    after_hosted_keyword: &[],
                    before_exposes: &[],
                    after_exposes: &[],
                    before_imports: &[],
                    after_imports: &[],
                    before_generates: &[],
                    after_generates: &[],
                    before_with: &[],
                    after_with: &[],
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
            package_name: self.package_name.remove_spaces(arena),
        }
    }
}

impl<'a> RemoveSpaces<'a> for ImportsEntry<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            ImportsEntry::Module(a, b) => ImportsEntry::Module(a, b.remove_spaces(arena)),
            ImportsEntry::Package(a, b, c) => ImportsEntry::Package(a, b, c.remove_spaces(arena)),
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
            } => Opaque {
                header: TypeHeader {
                    name: name.remove_spaces(arena),
                    vars: vars.remove_spaces(arena),
                },
                typ: typ.remove_spaces(arena),
            },
            Ability {
                header: TypeHeader { name, vars },
                loc_has,
                members,
            } => Ability {
                header: TypeHeader {
                    name: name.remove_spaces(arena),
                    vars: vars.remove_spaces(arena),
                },
                loc_has: loc_has.remove_spaces(arena),
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
                comment: _,
                body_pattern,
                body_expr,
            } => AnnotatedBody {
                ann_pattern: arena.alloc(ann_pattern.remove_spaces(arena)),
                ann_type: arena.alloc(ann_type.remove_spaces(arena)),
                comment: None,
                body_pattern: arena.alloc(body_pattern.remove_spaces(arena)),
                body_expr: arena.alloc(body_expr.remove_spaces(arena)),
            },
            Expect(a) => Expect(arena.alloc(a.remove_spaces(arena))),
        }
    }
}

impl<'a> RemoveSpaces<'a> for Def<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            Def::Type(def) => Def::Type(def.remove_spaces(arena)),
            Def::Value(def) => Def::Value(def.remove_spaces(arena)),
            Def::NotYetImplemented(a) => Def::NotYetImplemented(a),
            Def::SpaceBefore(a, _) | Def::SpaceAfter(a, _) => a.remove_spaces(arena),
        }
    }
}

impl<'a> RemoveSpaces<'a> for Has<'a> {
    fn remove_spaces(&self, _arena: &'a Bump) -> Self {
        Has::Has
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
            Expr::Tag(a) => Expr::Tag(a),
            Expr::OpaqueRef(a) => Expr::OpaqueRef(a),
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
            Expr::SingleQuote(a) => Expr::Num(a),
        }
    }
}

impl<'a> RemoveSpaces<'a> for Pattern<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            Pattern::Identifier(a) => Pattern::Identifier(a),
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
            Pattern::SingleQuote(a) => Pattern::NumLiteral(a),
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
            TypeAnnotation::As(a, _, c) => {
                TypeAnnotation::As(arena.alloc(a.remove_spaces(arena)), &[], c)
            }
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

impl<'a> RemoveSpaces<'a> for HasClause<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        HasClause {
            var: self.var.remove_spaces(arena),
            ability: self.ability.remove_spaces(arena),
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
