use core::fmt::{self, Write};

pub struct TypeRenderer {
    indent: Indentation,

    /// Whether the type needs to be wrapped in parens (only matters if the rendered type contains spaces,
    /// e.g. function application or tags with payloads)
    wrap_in_parens: WrapInParens,
}

impl Default for TypeRenderer {
    fn default() -> Self {
        Self {
            indent: Indentation::default(),
            wrap_in_parens: WrapInParens::Unnecessary,
        }
    }
}

// use Type::*;

// let todo = (); // TODO use wrap_in_parens

// match typ {
//     EmptyRec => self.render_record_type(buf, indent, iter::empty()),
//     EmptyTagUnion => self.render_tag_union_type(buf, indent, iter::empty()),
//     Function { args, ret } => {
//         if is_multiline(typ) {
//             buf.push_str("(\n");
//             self.render_function_type(buf, indent.increment(), args.iter(), &*ret);
//             let _ = write!(buf, "{indent}(\n");
//         } else {
//             buf.push_str("(");
//             self.render_function_type(buf, indent, args.iter(), &*ret);
//             buf.push_str(")");
//         }
//     }
//     Record(fields, _ext) => self.render_record_type(
//         buf,
//         indent,
//         fields.iter().map(|(field_name, field)| {
//             use roc_types::types::RecordField::*;

//             match field {
//                 Required(typ) | RigidRequired(typ) | Demanded(typ) => RecordField {
//                     field_name: field_name.as_str(),
//                     value_type: typ,
//                     is_required: true,
//                 },
//                 Optional(typ) | RigidOptional(typ) => RecordField {
//                     field_name: field_name.as_str(),
//                     value_type: typ,
//                     is_required: false,
//                 },
//             }
//         }),
//     ),
//     Tuple(_, _) => todo!(),
//     TagUnion(tags, _ext) => self.render_tag_union_type(
//         buf,
//         indent,
//         tags.iter()
//             .map(|(tag_name, payloads)| (tag_name.0.as_str(), payloads.as_slice())),
//     ),
//     FunctionOrTagUnion(_, _, _) => todo!(),
//     ClosureTag {
//         name: _,
//         captures: _,
//         ambient_function: _,
//     } => todo!(),
//     UnspecializedLambdaSet { unspecialized: _ } => todo!(),
//     DelayedAlias(_) => todo!(),
//     Alias {
//         symbol: _,
//         type_arguments: _,
//         lambda_set_variables: _,
//         infer_ext_in_output_types: _,
//         actual: _,
//         kind: _,
//     } => todo!(),
//     RecursiveTagUnion(_, _, _) => todo!(),
//     Apply(_, _, _) => todo!(),
//     Variable(_) => todo!(),
//     RangedNumber(_) => todo!(),
//     Error => todo!(),
// }

// fn render_function_type(
//     &self,
//     buf: &mut String<'_>,
//     indent: Indentation,
//     mut args: impl ExactSizeIterator<Item = &'a Type>,
//     ret: &'a Type<'a>,
// ) {
//     let args_len = args.len();

//     // Render args as multiline if the function has more than 3 args, or if any args are multiline
//     if args_len > 3 || args.any(is_multiline) {
//         let indent = indent.increment();

//         for (index, arg) in args.enumerate() {
//             let _ = write!(buf, "\n{indent}");

//             self.render_type(buf, indent, arg, WrapInParens::Unnecessary);

//             if index < args_len - 1 {
//                 // Put a comma at the end of each line except the last one,
//                 // because the -> is next.
//                 buf.push_str(",");
//             }
//         }

//         let _ = write!(buf, "\n{indent}->");
//     } else {
//         for (index, arg) in args.enumerate() {
//             self.render_type(buf, indent, arg, WrapInParens::Unnecessary);

//             if index < args_len - 1 {
//                 // Put a comma at the end of each line except the last one,
//                 // because the -> is next.
//                 buf.push_str(", ");
//             }
//         }

//         buf.push_str(" ->");
//     }

//     let indent = if is_multiline(ret) {
//         let _ = write!(buf, "\n{indent}");

//         indent.increment()
//     } else {
//         buf.push_str(" ");

//         indent
//     };

//     self.render_type(buf, indent, ret, WrapInParens::Unnecessary);
// }

// fn render_record_type(
//     &self,
//     buf: &mut String<'_>,
//     indent: Indentation,
//     mut fields: impl ExactSizeIterator<Item = RecordField<'a>>,
// ) {
//     const BRACES_CLASS_NAME: &str = "literal";
//     const OPEN_BRACE_HTML: &str = formatcp!("<span class='{BRACES_CLASS_NAME}'>{{</span>");
//     const CLOSE_BRACE_HTML: &str = formatcp!("<span class='{BRACES_CLASS_NAME}'>}}</span>");

//     match fields.next() {
//         None => {
//             // Empty records are just "{}"
//             let _ = write!(buf, "<span class='{BRACES_CLASS_NAME}'>{{}}</span>");
//         }
//         Some(RecordField {
//             field_name,
//             value_type,
//             is_required,
//         }) if fields.len() == 1 && !is_multiline(value_type) => {
//             let colon_or_question_mark = if is_required { ":" } else { "?" };

//             // If the record has one field, and that field's value is single-line,
//             // then we print the record on one line with spaces inside the braces
//             let _ = write!(
//                 buf,
//                 "{OPEN_BRACE_HTML} {field_name} {colon_or_question_mark} "
//             );
//             self.render_type(buf, indent, value_type, WrapInParens::Unnecessary);
//             let _ = write!(buf, " {CLOSE_BRACE_HTML}");
//         }
//         Some(first) => {
//             // Multi-field records are on multiple lines, with each line indented and ending in a trailing comma
//             let _ = write!(buf, "{indent}{OPEN_BRACE_HTML}");

//             {
//                 // Indent one extra level while we're inside the braces.
//                 let indent = indent.increment();

//                 for RecordField {
//                     field_name,
//                     value_type,
//                     is_required,
//                 } in iter::once(first).chain(fields)
//                 {
//                     let colon_or_question_mark = if is_required { ":" } else { "?" };

//                     let _ = write!(buf, "{indent}{field_name} {colon_or_question_mark} ");

//                     if is_multiline(value_type) {
//                         buf.push_str("\n");
//                     } else {
//                         buf.push_str(" ");
//                     }

//                     self.render_type(buf, indent, value_type, WrapInParens::Unnecessary);

//                     // Put a trailing comma at the end of each line.
//                     buf.push_str(",");
//                 }
//             }

//             // The closing brace goes on its own line, indented.
//             let _ = write!(buf, "{indent}{CLOSE_BRACE_HTML}");
//         }
//     }
// }

// fn render_tag_union_type(
//     &self,
//     buf: &mut String<'_>,
//     indent: Indentation,
//     mut tags: impl ExactSizeIterator<Item = (&'a str, &'a [Type])>,
// ) {
//     const BRACES_CLASS_NAME: &str = "literal";
//     const TAG_CLASS_NAME: &str = "literal";
//     const OPEN_BRACE_HTML: &str = formatcp!("<span class='{BRACES_CLASS_NAME}'>[</span>");
//     const CLOSE_BRACE_HTML: &str = formatcp!("<span class='{BRACES_CLASS_NAME}'>]</span>");

//     match tags.next() {
//         None => {
//             // Empty tag unions are just "[]"
//             let _ = write!(buf, "<span class='{BRACES_CLASS_NAME}'>[]</span>");
//         }
//         Some((tag, payloads)) if tags.len() == 1 && !payloads.iter().any(is_multiline) => {
//             // Single-line tag unions don't have spaces inside the braces
//             let _ = write!(
//                 buf,
//                 "{OPEN_BRACE_HTML}<span class='{TAG_CLASS_NAME}'>{tag}</span>"
//             );

//             for typ in payloads.iter() {
//                 buf.push_str(" ");
//                 self.render_type(buf, indent, typ, WrapInParens::NeededIfWhitespace);
//             }

//             buf.push_str(CLOSE_BRACE_HTML);
//         }
//         Some(first) => {
//             // Multi-tag unions are on multiple lines, with each line indented and ending in a trailing comma
//             let _ = write!(buf, "{indent}{OPEN_BRACE_HTML}");

//             {
//                 // Indent one extra level while we're inside the braces.
//                 let indent = indent.increment();

//                 for (tag, payloads) in iter::once(first).chain(tags) {
//                     let _ = write!(buf, "{indent}<span class='{TAG_CLASS_NAME}'>{tag}</span>");

//                     for typ in payloads.iter() {
//                         buf.push_str(" ");
//                         self.render_type(buf, indent, typ, WrapInParens::NeededIfWhitespace);
//                     }

//                     // Put a trailing comma at the end of each line.
//                     let _ = buf.push_str(",");
//                 }
//             }

//             // The closing brace goes on its own line, indented.
//             let _ = write!(buf, "{indent}{CLOSE_BRACE_HTML}");
//         }
//     }
// }

#[derive(Clone, Copy, Debug, Default)]
pub struct Indentation {
    level: u32,
}

#[derive(Clone, Copy)]
pub enum WrapInParens {
    Unnecessary,
    NeededIfWhitespace,
}

impl Indentation {
    const INDENT_STR: &'static str = "    ";

    pub fn increment(self) -> Self {
        Self {
            level: self.level.saturating_add(1),
        }
    }

    pub fn decrement(self) -> Self {
        Self {
            level: self.level.saturating_sub(1),
        }
    }
}

impl fmt::Display for Indentation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Always start with a newline before indenting
        f.write_char('\n')?;

        for _ in 0..self.level {
            f.write_str(Self::INDENT_STR)?;
        }

        Ok(())
    }
}
