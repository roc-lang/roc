//! This is an example of the [Kaleidoscope tutorial](https://llvm.org/docs/tutorial/)
//! made in Rust, using Inkwell.
//! Currently, all features up to the [7th chapter](https://llvm.org/docs/tutorial/LangImpl07.html)
//! are available.
//! This example is supposed to be ran as a executable, which launches a REPL.
//! The source code is in the following order:
//! - Lexer,
//! - Parser,
//! - Emitter,
//! - Program.
//!
//! Both the `Parser` and the `Emitter` may fail, in which case they would return
//! an error represented by `Result<T, &'static str>`, for easier error reporting.

extern crate inkwell;

use std::borrow::Borrow;
use std::collections::HashMap;
use std::io::Write;
// use std::iter::Peekable;
// use std::ops::DerefMut;
// use std::str::Chars;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValueEnum, FloatValue, FunctionValue, PointerValue};
use inkwell::FloatPredicate;

// ======================================================================================
// LEXER ================================================================================
// ======================================================================================

/// Represents a primitive syntax token.
#[derive(Debug, Clone)]
pub enum Token {
    Binary,
    Comma,
    Comment,
    Def,
    Else,
    EOF,
    Extern,
    For,
    Ident(String),
    If,
    In,
    LParen,
    Number(f64),
    Op(char),
    RParen,
    Then,
    Unary,
    Var,
}

/// Defines the prototype (name and parameters) of a function.
#[derive(Debug)]
pub struct Prototype {
    pub name: String,
    pub args: Vec<String>,
}

/// Defines a user-defined or external function.
#[derive(Debug)]
pub struct Function {
    pub prototype: Prototype,
    pub body: Option<Expr>,
    pub is_anon: bool,
}

pub struct Emitter<'a> {
    pub context: &'a Context,
    pub builder: &'a Builder,
    pub fpm: &'a PassManager<FunctionValue>,
    pub module: &'a Module,
    pub function: &'a Function,

    variables: HashMap<String, PointerValue>,
    fn_value_opt: Option<FunctionValue>,
}

#[derive(Debug)]
pub enum Expr {
    Binary {
        op: char,
        left: Box<Expr>,
        right: Box<Expr>,
    },

    Call {
        fn_name: String,
        args: Vec<Expr>,
    },

    Conditional {
        cond: Box<Expr>,
        consequence: Box<Expr>,
        alternative: Box<Expr>,
    },

    For {
        var_name: String,
        start: Box<Expr>,
        end: Box<Expr>,
        step: Option<Box<Expr>>,
        body: Box<Expr>,
    },

    Number(f64),

    Variable(String),

    VarIn {
        variables: Vec<(String, Option<Expr>)>,
        body: Box<Expr>,
    },
}

impl<'a> Emitter<'a> {
    /// Gets a defined function given its name.
    #[inline]
    fn get_function(&self, name: &str) -> Option<FunctionValue> {
        self.module.get_function(name)
    }

    /// Returns the `FunctionValue` representing the function being compiled.
    #[inline]
    fn fn_value(&self) -> FunctionValue {
        // TODO shouldn't there be like a stack of these? What about functions inside functions?
        // Also, do we even need this?
        self.fn_value_opt.unwrap()
    }

    /// Creates a new stack allocation instruction in the entry block of the function.
    fn create_entry_block_alloca(&self, name: &str) -> PointerValue {
        let builder = self.context.create_builder();

        let entry = self.fn_value().get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(&entry),
        }

        builder.build_alloca(self.context.f64_type(), name)
    }

    /// Compiles the specified `Expr` into an LLVM `FloatValue`.
    fn compile_expr(&mut self, expr: &Expr) -> Result<FloatValue, &'static str> {
        match *expr {
            // Expr::IntLiteral(num) => Ok(self.context.i64_type().const_int(num as u64, false)),
            Expr::Number(num) => Ok(self.context.f64_type().const_float(num)),

            Expr::Variable(ref name) => match self.variables.get(name.as_str()) {
                Some(var) => Ok(self
                    .builder
                    .build_load(*var, name.as_str())
                    .into_float_value()),
                None => Err("Could not find a matching variable."),
            },

            Expr::VarIn {
                ref variables,
                ref body,
            } => {
                let mut old_bindings = Vec::new();

                for &(ref var_name, ref initializer) in variables {
                    let var_name = var_name.as_str();

                    let initial_val = match *initializer {
                        Some(ref init) => self.compile_expr(init)?,
                        None => self.context.f64_type().const_float(0.),
                    };

                    let alloca = self.create_entry_block_alloca(var_name);

                    self.builder.build_store(alloca, initial_val);

                    if let Some(old_binding) = self.variables.remove(var_name) {
                        old_bindings.push(old_binding);
                    }

                    self.variables.insert(var_name.to_string(), alloca);
                }

                let body = self.compile_expr(body)?;

                for binding in old_bindings {
                    self.variables
                        .insert(binding.get_name().to_str().unwrap().to_string(), binding);
                }

                Ok(body)
            }

            Expr::Binary {
                op,
                ref left,
                ref right,
            } => {
                if op == '=' {
                    // handle assignement
                    let var_name = match *left.borrow() {
                        Expr::Variable(ref var_name) => var_name,
                        _ => {
                            return Err("Expected variable as left-hand operator of assignement.");
                        }
                    };

                    let var_val = self.compile_expr(right)?;
                    let var = self
                        .variables
                        .get(var_name.as_str())
                        .ok_or("Undefined variable.")?;

                    self.builder.build_store(*var, var_val);

                    Ok(var_val)
                } else {
                    let lhs = self.compile_expr(left)?;
                    let rhs = self.compile_expr(right)?;

                    match op {
                        '+' => Ok(self.builder.build_float_add(lhs, rhs, "tmpadd")),
                        '-' => Ok(self.builder.build_float_sub(lhs, rhs, "tmpsub")),
                        '*' => Ok(self.builder.build_float_mul(lhs, rhs, "tmpmul")),
                        '/' => Ok(self.builder.build_float_div(lhs, rhs, "tmpdiv")),
                        '<' => Ok({
                            let cmp = self.builder.build_float_compare(
                                FloatPredicate::ULT,
                                lhs,
                                rhs,
                                "tmpcmp",
                            );

                            self.builder.build_unsigned_int_to_float(
                                cmp,
                                self.context.f64_type(),
                                "tmpbool",
                            )
                        }),
                        '>' => Ok({
                            let cmp = self.builder.build_float_compare(
                                FloatPredicate::ULT,
                                rhs,
                                lhs,
                                "tmpcmp",
                            );

                            self.builder.build_unsigned_int_to_float(
                                cmp,
                                self.context.f64_type(),
                                "tmpbool",
                            )
                        }),

                        custom => {
                            let mut name = String::from("binary");

                            name.push(custom);

                            match self.get_function(name.as_str()) {
                                Some(fun) => {
                                    match self
                                        .builder
                                        .build_call(fun, &[lhs.into(), rhs.into()], "tmpbin")
                                        .try_as_basic_value()
                                        .left()
                                    {
                                        Some(value) => Ok(value.into_float_value()),
                                        None => Err("Invalid call produced."),
                                    }
                                }

                                None => Err("Undefined binary operator."),
                            }
                        }
                    }
                }
            }

            Expr::Call {
                ref fn_name,
                ref args,
            } => match self.get_function(fn_name.as_str()) {
                Some(fun) => {
                    let mut compiled_args = Vec::with_capacity(args.len());

                    for arg in args {
                        compiled_args.push(self.compile_expr(arg)?);
                    }

                    let argsv: Vec<BasicValueEnum> = compiled_args
                        .iter()
                        .by_ref()
                        .map(|&val| val.into())
                        .collect();

                    match self
                        .builder
                        .build_call(fun, argsv.as_slice(), "tmp")
                        .try_as_basic_value()
                        .left()
                    {
                        Some(value) => Ok(value.into_float_value()),
                        None => Err("Invalid call produced."),
                    }
                }
                None => Err("Unknown function."),
            },

            Expr::Conditional {
                ref cond,
                ref consequence,
                ref alternative,
            } => {
                let parent = self.fn_value();
                let zero_const = self.context.f64_type().const_float(0.0);

                // create condition by comparing without 0.0 and returning an int
                let cond = self.compile_expr(cond)?;
                let cond = self.builder.build_float_compare(
                    FloatPredicate::ONE,
                    cond,
                    zero_const,
                    "ifcond",
                );

                // build branch
                let then_bb = self.context.append_basic_block(&parent, "then");
                let else_bb = self.context.append_basic_block(&parent, "else");
                let cont_bb = self.context.append_basic_block(&parent, "ifcont");

                self.builder
                    .build_conditional_branch(cond, &then_bb, &else_bb);

                // build then block
                self.builder.position_at_end(&then_bb);
                let then_val = self.compile_expr(consequence)?;
                self.builder.build_unconditional_branch(&cont_bb);

                let then_bb = self.builder.get_insert_block().unwrap();

                // build else block
                self.builder.position_at_end(&else_bb);
                let else_val = self.compile_expr(alternative)?;
                self.builder.build_unconditional_branch(&cont_bb);

                let else_bb = self.builder.get_insert_block().unwrap();

                // emit merge block
                self.builder.position_at_end(&cont_bb);

                let phi = self.builder.build_phi(self.context.f64_type(), "iftmp");

                phi.add_incoming(&[(&then_val, &then_bb), (&else_val, &else_bb)]);

                Ok(phi.as_basic_value().into_float_value())
            }

            Expr::For {
                ref var_name,
                ref start,
                ref end,
                ref step,
                ref body,
            } => {
                let parent = self.fn_value();

                let start_alloca = self.create_entry_block_alloca(var_name);
                let start = self.compile_expr(start)?;

                self.builder.build_store(start_alloca, start);

                // go from current block to loop block
                let loop_bb = self.context.append_basic_block(&parent, "loop");

                self.builder.build_unconditional_branch(&loop_bb);
                self.builder.position_at_end(&loop_bb);

                let old_val = self.variables.remove(var_name.as_str());

                self.variables.insert(var_name.to_owned(), start_alloca);

                // emit body
                self.compile_expr(body)?;

                // emit step
                let step = match *step {
                    Some(ref step) => self.compile_expr(step)?,
                    None => self.context.f64_type().const_float(1.0),
                };

                // compile end condition
                let end_cond = self.compile_expr(end)?;

                let curr_var = self.builder.build_load(start_alloca, var_name);
                let next_var =
                    self.builder
                        .build_float_add(curr_var.into_float_value(), step, "nextvar");

                self.builder.build_store(start_alloca, next_var);

                let end_cond = self.builder.build_float_compare(
                    FloatPredicate::ONE,
                    end_cond,
                    self.context.f64_type().const_float(0.0),
                    "loopcond",
                );
                let after_bb = self.context.append_basic_block(&parent, "afterloop");

                self.builder
                    .build_conditional_branch(end_cond, &loop_bb, &after_bb);
                self.builder.position_at_end(&after_bb);

                self.variables.remove(var_name);

                if let Some(val) = old_val {
                    self.variables.insert(var_name.to_owned(), val);
                }

                Ok(self.context.f64_type().const_float(0.0))
            }
        }
    }

    /// Compiles the specified `Prototype` into an extern LLVM `FunctionValue`.
    fn compile_prototype(&self, proto: &Prototype) -> Result<FunctionValue, &'static str> {
        let ret_type = self.context.f64_type();
        let args_types = std::iter::repeat(ret_type)
            .take(proto.args.len())
            .map(|f| f.into())
            .collect::<Vec<BasicTypeEnum>>();
        let args_types = args_types.as_slice();

        let fn_type = self.context.f64_type().fn_type(args_types, false);
        let fn_val = self.module.add_function(proto.name.as_str(), fn_type, None);

        // set arguments names
        for (i, arg) in fn_val.get_param_iter().enumerate() {
            arg.into_float_value().set_name(proto.args[i].as_str());
        }

        // finally return built prototype
        Ok(fn_val)
    }

    /// Compiles the specified `Function` into an LLVM `FunctionValue`.
    fn compile_fn(&mut self) -> Result<FunctionValue, &'static str> {
        let proto = &self.function.prototype;
        let function = self.compile_prototype(proto)?;

        // got external function, returning only compiled prototype
        if self.function.body.is_none() {
            return Ok(function);
        }

        let entry = self.context.append_basic_block(&function, "entry");

        self.builder.position_at_end(&entry);

        // update fn field
        self.fn_value_opt = Some(function);

        // build variables map
        self.variables.reserve(proto.args.len());

        for (i, arg) in function.get_param_iter().enumerate() {
            let arg_name = proto.args[i].as_str();
            let alloca = self.create_entry_block_alloca(arg_name);

            self.builder.build_store(alloca, arg);

            self.variables.insert(proto.args[i].clone(), alloca);
        }

        // compile body
        let body = self.compile_expr(self.function.body.as_ref().unwrap())?;

        self.builder.build_return(Some(&body));

        // return the whole thing after verification and optimization
        if function.verify(true) {
            self.fpm.run_on(&function);

            Ok(function)
        } else {
            unsafe {
                function.delete();
            }

            Err("Invalid generated function.")
        }
    }

    /// Compiles the specified `Function` in the given `Context` and using the specified `Builder`, `PassManager`, and `Module`.
    pub fn compile(
        context: &'a Context,
        builder: &'a Builder,
        pass_manager: &'a PassManager<FunctionValue>,
        module: &'a Module,
        function: &Function,
    ) -> Result<FunctionValue, &'static str> {
        let mut compiler = Emitter {
            context: context,
            builder: builder,
            fpm: pass_manager,
            module: module,
            function: function,
            fn_value_opt: None,
            variables: HashMap::new(),
        };

        compiler.compile_fn()
    }
}

// ======================================================================================
// PROGRAM ==============================================================================
// ======================================================================================

// macro used to print & flush without printing a new line
macro_rules! print_flush {
    ( $( $x:expr ),* ) => {
        print!( $($x, )* );

        std::io::stdout().flush().expect("Could not flush to standard output.");
    };
}

#[no_mangle]
pub extern "C" fn putchard(x: f64) -> f64 {
    print_flush!("{}", x as u8 as char);
    x
}

#[no_mangle]
pub extern "C" fn printd(x: f64) -> f64 {
    println!("{}", x);
    x
}

// Adding the functions above to a global array,
// so Rust compiler won't remove them.
#[used]
static EXTERNAL_FNS: [extern "C" fn(f64) -> f64; 2] = [putchard, printd];

#[test]
fn gen() {
    use inkwell::OptimizationLevel;
    // use self::inkwell::support::add_symbol;

    let context = Context::create();
    let module = context.create_module("repl");
    let builder = context.create_builder();

    // Create FPM
    let fpm = PassManager::create(&module);

    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.add_gvn_pass();
    fpm.add_cfg_simplification_pass();
    fpm.add_basic_alias_analysis_pass();
    fpm.add_promote_memory_to_register_pass();
    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();

    fpm.initialize();

    // make module
    let module = context.create_module("tmp");

    let name = "main".to_string();
    let prototype = Prototype {
        name,
        args: Vec::new(),
    };
    let function = Function {
        prototype,
        body: Some(Expr::Number(12345.0)),
        is_anon: false,
    };

    // TODO remove this dead code
    for prev in &Vec::new() {
        Emitter::compile(&context, &builder, &fpm, &module, prev)
            .expect("Cannot re-add previously compiled function.");
    }

    // make main(), a function which returns an f64

    Emitter::compile(&context, &builder, &fpm, &module, &function).expect("Error compiling main");

    // let fn_type = context.f64_type().fn_type(&Vec::new(), false);
    // let function = module.add_function(proto.name.as_str(), fn_type, None);

    // make execution engine
    let ee = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();

    let maybe_fn = unsafe { ee.get_function::<unsafe extern "C" fn() -> f64>("main") };
    let compiled_fn = match maybe_fn {
        Ok(f) => f,
        Err(err) => {
            panic!("!> Error during execution: {:?}", err);
        }
    };

    unsafe {
        assert_eq!(12345.0, compiled_fn.call());
    }
}
