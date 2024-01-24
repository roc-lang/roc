use std::fmt::{Display, Formatter, Write};

use crate::api::{
    BlockExpr, CalleeSpecBuf, ConstDef, ConstNameBuf, ContinuationId, EntryPointNameBuf,
    ExprBuilder, FuncDef, FuncNameBuf, ModDef, ModNameBuf, Op, Program, TypeDef, TypeId,
    TypeNameBuf, TypeOp, UpdateModeBuf, ValueId,
};
use crate::util::id_type::Id;
use crate::util::op_graph::{Node, OpGraph};

#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
struct RenderContext {
    content: String,
    spaces_per_level: usize,
    indent_level: usize,
    pending_indent: bool,
}

impl RenderContext {
    fn new(spaces_per_level: usize) -> RenderContext {
        RenderContext {
            content: String::new(),
            spaces_per_level,
            indent_level: 0,
            pending_indent: true,
        }
    }

    fn with_indent<R>(&mut self, body: impl for<'a> FnOnce(&'a mut RenderContext) -> R) -> R {
        self.indent_level += 1;
        let result = body(self);
        self.indent_level -= 1;
        result
    }

    fn write(&mut self, to_write: impl std::fmt::Display) {
        if self.pending_indent {
            self.content
                .extend((0..self.indent_level * self.spaces_per_level).map(|_| ' '));
            self.pending_indent = false;
        }
        write!(&mut self.content, "{to_write}").expect("writing to string failed");
    }

    fn writeln(&mut self, to_write: impl std::fmt::Display) {
        self.write(to_write);
        self.content.push('\n');
        self.pending_indent = true;
    }

    fn finish(self) -> String {
        self.content
    }
}

#[derive(Clone, Copy, Debug)]
struct Ident {
    prefix: &'static str,
    id: usize,
}

impl Display for Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}_{}", self.prefix, self.id)
    }
}

fn val_ident(value: ValueId) -> Ident {
    Ident {
        prefix: "val",
        id: value.to_index(),
    }
}

fn type_ident(type_: TypeId) -> Ident {
    Ident {
        prefix: "type",
        id: type_.to_index(),
    }
}

fn continuation_ident(continuation: ContinuationId) -> Ident {
    Ident {
        prefix: "cont",
        id: continuation.to_index(),
    }
}

#[derive(Clone, Copy, Debug)]
struct ByteString<'a>(&'a [u8]);

impl<'a> Display for ByteString<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"")?;
        for &byte in self.0 {
            write!(f, "{}", std::ascii::escape_default(byte))?;
        }
        write!(f, "\"")?;
        Ok(())
    }
}

fn update_mode_string(buf: &UpdateModeBuf) -> ByteString {
    ByteString(&buf.0)
}

fn callee_spec_string(buf: &CalleeSpecBuf) -> ByteString {
    ByteString(&buf.0)
}

fn mod_name_string(mod_name: &ModNameBuf) -> ByteString {
    ByteString(&mod_name.0)
}

fn func_name_string(func_name: &FuncNameBuf) -> ByteString {
    ByteString(&func_name.0)
}

fn type_name_string(type_name: &TypeNameBuf) -> ByteString {
    ByteString(&type_name.0)
}

fn const_name_string(const_name: &ConstNameBuf) -> ByteString {
    ByteString(&const_name.0)
}

fn entry_point_name_string(entry_point: &EntryPointNameBuf) -> ByteString {
    ByteString(&entry_point.0)
}

fn delimit<T>(
    ctx: &mut RenderContext,
    items: impl Iterator<Item = T>,
    mut render_delim: impl for<'a> FnMut(&'a mut RenderContext),
    mut render_item: impl for<'a> FnMut(&'a mut RenderContext, T),
) {
    for (i, item) in items.enumerate() {
        if i != 0 {
            render_delim(ctx);
        }
        render_item(ctx, item);
    }
}

fn render_op(builder: &ExprBuilder, ctx: &mut RenderContext, op: &Op) {
    match op {
        Op::Arg | Op::ContinuationArg | Op::DeclareContinuation { .. } => {
            ctx.write(format_args!(
                "/* internal error: {op:?} should not be rendered as a value */"
            ));
        }

        Op::Jump {
            continuation,
            unreachable_result_type,
        } => {
            ctx.write(format_args!(
                "jump<{}> {}",
                type_ident(*unreachable_result_type),
                continuation_ident(*continuation),
            ));
        }

        Op::UnknownWith { result_type } => {
            ctx.write(format_args!("unknown_with<{}>", type_ident(*result_type)));
        }

        Op::Call {
            callee_spec_var,
            callee_mod,
            callee,
        } => {
            ctx.write(format_args!(
                "call[{}] {}::{}",
                callee_spec_string(&builder.callee_spec_vars[callee_spec_var]),
                mod_name_string(callee_mod),
                func_name_string(callee),
            ));
        }

        Op::ConstRef { const_mod, const_ } => {
            ctx.write(format_args!(
                "const_ref {}::{}",
                mod_name_string(const_mod),
                const_name_string(const_),
            ));
        }

        Op::Choice { cases } => {
            ctx.writeln("choice {");
            ctx.with_indent(|ctx| {
                for &case in cases {
                    ctx.write("case ");
                    render_block_expr(builder, ctx, case);
                    ctx.writeln(",");
                }
            });
            ctx.write("}");
        }

        Op::SubBlock { sub_block } => {
            render_block_expr(builder, ctx, *sub_block);
        }

        Op::Terminate {
            unreachable_result_type,
        } => {
            ctx.write(format_args!(
                "terminate<{}>",
                type_ident(*unreachable_result_type),
            ));
        }

        Op::NewHeapCell => {
            ctx.write("new_heap_cell");
        }

        Op::Touch => {
            ctx.write("touch");
        }

        Op::RecursiveTouch => {
            ctx.write("recursive_touch");
        }

        Op::UpdateWriteOnly { update_mode_var } => {
            ctx.write(format_args!(
                "update_write_only[{}]",
                update_mode_string(&builder.update_mode_vars[update_mode_var]),
            ));
        }

        Op::EmptyBag { item_type } => {
            ctx.write(format_args!("empty_bag<{}>", type_ident(*item_type)));
        }

        Op::BagInsert => {
            ctx.write("bag_insert");
        }

        Op::BagGet => {
            ctx.write("bag_get");
        }

        Op::BagRemove => {
            ctx.write("bag_remove");
        }

        Op::MakeTuple => {
            ctx.write("make_tuple");
        }

        Op::GetTupleField { field_idx } => {
            ctx.write(format_args!("get_tuple_field {field_idx}"));
        }

        Op::MakeUnion {
            variant_types,
            variant_idx,
        } => {
            ctx.write("make_union<");
            delimit(
                ctx,
                variant_types.iter(),
                |ctx| {
                    ctx.write(", ");
                },
                |ctx, variant_type| {
                    ctx.write(type_ident(*variant_type));
                },
            );
            ctx.write(format_args!("> {variant_idx}"));
        }

        Op::UnwrapUnion { variant_idx } => {
            ctx.write(format_args!("unwrap_union {variant_idx}"));
        }

        Op::MakeNamed { named_mod, named } => {
            ctx.write(format_args!(
                "make_named {}::{}",
                mod_name_string(named_mod),
                type_name_string(named),
            ));
        }

        Op::UnwrapNamed { named_mod, named } => {
            ctx.write(format_args!(
                "unwrap_named {}::{}",
                mod_name_string(named_mod),
                type_name_string(named),
            ));
        }
    };
}

fn render_statement(builder: &ExprBuilder, ctx: &mut RenderContext, value_id: ValueId) {
    match builder.vals.node(value_id) {
        Node {
            op: Op::DeclareContinuation { continuation },
            inputs: [],
        } => {
            render_continuation_def(builder, ctx, *continuation);
        }

        Node { op, inputs } => {
            ctx.write(format_args!("let {} = ", val_ident(value_id)));
            render_op(builder, ctx, op);
            ctx.write(" (");
            delimit(
                ctx,
                inputs.iter(),
                |ctx| {
                    ctx.write(", ");
                },
                |ctx, input| {
                    ctx.write(val_ident(*input));
                },
            );
            ctx.writeln(");");
        }
    }
}

fn render_continuation_def(
    builder: &ExprBuilder,
    ctx: &mut RenderContext,
    continuation: ContinuationId,
) {
    let info = builder.continuations[continuation];
    ctx.write(format_args!(
        "continuation {} ({}: {}) -> {} ",
        continuation_ident(continuation),
        val_ident(info.arg),
        type_ident(info.arg_type),
        type_ident(info.ret_type),
    ));
    render_block_expr(
        builder,
        ctx,
        info.body
            .expect("builder API should have verified that continuation body is defined"),
    );
    ctx.writeln("");
}

fn render_block_expr(builder: &ExprBuilder, ctx: &mut RenderContext, block_expr: BlockExpr) {
    let BlockExpr(block, ret_val) = block_expr;
    ctx.writeln("{");
    ctx.with_indent(|ctx| {
        for value in builder.blocks.block_values(block) {
            render_statement(builder, ctx, value);
        }
        ctx.writeln(val_ident(ret_val))
    });
    ctx.write("}");
}

fn render_type_node(ctx: &mut RenderContext, node: Node<TypeId, TypeOp>) {
    match node {
        Node {
            op: TypeOp::Named { named_mod, named },
            inputs,
        } => {
            debug_assert!(inputs.is_empty());
            ctx.write(format_args!(
                "{}::{}",
                mod_name_string(named_mod),
                type_name_string(named)
            ));
        }

        Node {
            op: TypeOp::Tuple,
            inputs,
        } => {
            ctx.write("(");
            delimit(
                ctx,
                inputs.iter(),
                |ctx| {
                    ctx.write(", ");
                },
                |ctx, input| {
                    ctx.write(type_ident(*input));
                },
            );
            ctx.write(")");
        }

        Node {
            op: TypeOp::Union,
            inputs,
        } => {
            ctx.write("union { ");
            delimit(
                ctx,
                inputs.iter(),
                |ctx| {
                    ctx.write(", ");
                },
                |ctx, input| {
                    ctx.write(type_ident(*input));
                },
            );
            if !inputs.is_empty() {
                ctx.write(" ");
            }
            ctx.write("}");
        }

        Node {
            op: TypeOp::HeapCell,
            inputs,
        } => {
            debug_assert!(inputs.is_empty());
            ctx.write("heap_cell");
        }

        Node {
            op: TypeOp::Bag,
            inputs,
        } => {
            debug_assert_eq!(inputs.len(), 1);
            ctx.write(format_args!("bag<{}>", type_ident(inputs[0])));
        }
    }
}

fn render_type_bindings(ctx: &mut RenderContext, types: &OpGraph<TypeId, TypeOp>) {
    ctx.writeln("{");
    ctx.with_indent(|ctx| {
        for type_id in types.count().iter() {
            ctx.write(format_args!("type {} = ", type_ident(type_id)));
            render_type_node(ctx, types.node(type_id));
            ctx.writeln(";");
        }
    });
    ctx.write("}");
}

fn render_func_def(ctx: &mut RenderContext, name: &FuncNameBuf, func_def: &FuncDef) {
    ctx.write(format_args!(
        "fn {} ({}: {}) -> {} ",
        func_name_string(name),
        val_ident(func_def.builder.argument),
        type_ident(func_def.arg_type),
        type_ident(func_def.ret_type),
    ));
    render_block_expr(&func_def.builder.expr_builder, ctx, func_def.root);
    ctx.write(" where ");
    render_type_bindings(ctx, &func_def.builder.expr_builder.type_builder.types);
    ctx.writeln("");
}

fn render_const_def(ctx: &mut RenderContext, name: &ConstNameBuf, const_def: &ConstDef) {
    ctx.write(format_args!(
        "const {}: {} = ",
        const_name_string(name),
        type_ident(const_def.type_),
    ));
    render_block_expr(&const_def.builder.expr_builder, ctx, const_def.root);
    ctx.write(" where ");
    render_type_bindings(ctx, &const_def.builder.expr_builder.type_builder.types);
    ctx.writeln("");
}

fn render_type_def(ctx: &mut RenderContext, name: &TypeNameBuf, type_def: &TypeDef) {
    ctx.write(format_args!(
        "type {} = {} where ",
        type_name_string(name),
        type_ident(type_def.root),
    ));
    render_type_bindings(ctx, &type_def.builder.types);
    ctx.writeln("");
}

fn render_mod_def(ctx: &mut RenderContext, name: &ModNameBuf, mod_def: &ModDef) {
    ctx.writeln(format_args!("mod {} {{", mod_name_string(name)));
    ctx.with_indent(|ctx| {
        let mut first_item = true;
        let mut write_spacing = |ctx: &mut RenderContext| {
            if !first_item {
                ctx.writeln("");
            }
            first_item = false;
        };

        for (type_name, type_def) in &mod_def.type_defs {
            write_spacing(ctx);
            render_type_def(ctx, type_name, type_def);
        }

        for (const_name, const_def) in &mod_def.const_defs {
            write_spacing(ctx);
            render_const_def(ctx, const_name, const_def);
        }

        for (func_name, func_def) in &mod_def.func_defs {
            write_spacing(ctx);
            render_func_def(ctx, func_name, func_def);
        }
    });
    ctx.writeln("}");
}

fn render_entry_point(
    ctx: &mut RenderContext,
    name: &EntryPointNameBuf,
    entry_point: &(ModNameBuf, FuncNameBuf),
) {
    let (mod_, func) = entry_point;
    ctx.writeln(format_args!(
        "entry_point {} = {}::{};",
        entry_point_name_string(name),
        mod_name_string(mod_),
        func_name_string(func)
    ));
}

fn render_program(ctx: &mut RenderContext, program: &Program) {
    ctx.writeln("program {");
    ctx.with_indent(|ctx| {
        let mut first_item = true;
        let mut write_spacing = |ctx: &mut RenderContext| {
            if !first_item {
                ctx.writeln("");
            }
            first_item = false;
        };

        for (mod_name, mod_def) in &program.mods {
            write_spacing(ctx);
            render_mod_def(ctx, mod_name, mod_def);
        }

        for (entry_point_name, entry_point) in &program.entry_points {
            write_spacing(ctx);
            render_entry_point(ctx, entry_point_name, entry_point);
        }
    });
    ctx.write("}");
}

pub fn render_program_to_string(program: &Program) -> String {
    let mut ctx = RenderContext::new(2);
    render_program(&mut ctx, program);
    ctx.finish()
}
