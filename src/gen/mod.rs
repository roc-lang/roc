extern crate inkwell;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValueEnum, FloatValue, FunctionValue, IntValue, PointerValue};

use can::expr::Expr;
use can::procedure::Procedure;
use can::symbol::Symbol;
use collections::ImMap;
use collections::MutMap;
use subs::FlatType::*;
use subs::{Content, Subs, Variable};
use types;

/// Defines the prototype (name and parameters) of a function.
#[derive(Debug)]
pub struct Prototype {
    pub name: String,
    pub args: Vec<(String, BasicTypeEnum)>,
    pub ret: BasicTypeEnum,
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
    // variables: HashMap<String, PointerValue>,
    // fn_value_opt: Option<FunctionValue>,
}

pub struct ModuleBuilder<'a> {
    pub context: &'a Context,
    pub builder: &'a Builder,
    pub fpm: &'a PassManager<FunctionValue>,
    pub module: &'a Module,
    pub function: &'a Function,

    procedures: &'a MutMap<Symbol, Procedure>,
    fn_value_opt: Option<FunctionValue>,
}

enum TypedVal {
    FloatConst(FloatValue),
    IntConst(IntValue),
    Typed(Variable, BasicValueEnum),
}

impl Into<BasicValueEnum> for TypedVal {
    fn into(self) -> BasicValueEnum {
        use self::TypedVal::*;

        match self {
            FloatConst(val) => val.into(),
            IntConst(val) => val.into(),
            Typed(_, val) => val,
        }
    }
}

impl<'a> ModuleBuilder<'a> {
    #[inline]
    fn get_function(&self, name: &str) -> Option<FunctionValue> {
        self.module.get_function(name)
    }

    /// Compiles the specified `Function` in the given `Context` and using the specified `Builder`, `PassManager`, and `Module`.
    pub fn build(
        context: &'a Context,
        builder: &'a Builder,
        pass_manager: &'a PassManager<FunctionValue>,
        procedures: &'a MutMap<Symbol, Procedure>,
        module: &'a Module,
        function: &Function,
    ) -> Result<FunctionValue, &'static str> {
        let mut compiler = ModuleBuilder {
            context,
            builder,
            fpm: pass_manager,
            module,
            function,
            procedures,
            fn_value_opt: None,
        };

        compiler.compile_fn()
    }

    /// Compiles the specified `Prototype` into an extern LLVM `FunctionValue`.
    fn compile_prototype(&self, proto: &Prototype) -> Result<FunctionValue, &'static str> {
        let arg_types = proto
            .args
            .iter()
            .map(|(_, typ)| typ.clone())
            .collect::<Vec<BasicTypeEnum>>();

        let fn_type = proto.ret.fn_type(&arg_types, false);
        let fn_val = self.module.add_function(proto.name.as_str(), fn_type, None);

        // set arguments names
        // for (i, arg) in fn_val.get_param_iter().enumerate() {
        //     arg.set_name(proto.args[i].0.as_str());
        // }

        // finally return built prototype
        Ok(fn_val)
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
        let mut vars = ImMap::default(); // TODO with_capacity(proto.args.len())

        for (i, arg) in function.get_param_iter().enumerate() {
            let arg_name = proto.args[i].0.as_str();
            let alloca = self.create_entry_block_alloca(arg_name);

            self.builder.build_store(alloca, arg);

            vars.insert(
                Symbol::from_parts(&[], proto.args[i].0.clone().as_str()),
                alloca,
            );
        }

        // compile body
        let body = self.compile_expr(self.function.body.as_ref().unwrap(), &mut vars);
        let basic_val: BasicValueEnum = body.into();

        self.builder.build_return(Some(&basic_val));

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

    fn compile_expr(&mut self, expr: &Expr, vars: &mut ImMap<Symbol, PointerValue>) -> TypedVal {
        use self::TypedVal::*;
        use can::expr::Expr::*;

        match *expr {
            Int(num) => IntConst(self.context.i64_type().const_int(num as u64, false)),
            Float(num) => FloatConst(self.context.f64_type().const_float(num)),

            // Var and FunctionPointer do the same thing; they are only different
            // for the benefit of canonicalization, which uses FunctionPointer
            // to name functions.
            Var(type_var, ref symbol) | FunctionPointer(type_var, ref symbol) => {
                match vars.get(symbol) {
                    Some(var) => Typed(
                        type_var,
                        self.builder
                            .build_load(*var, &*(symbol.clone()).into_boxed_str()),
                    ),
                    None => panic!(
                        "Roc compiler bug: could not find symbol `{:?}` with type var `{:?}`",
                        symbol, type_var
                    ),
                }
            }

            Defs(_, _, _) => panic!("TODO gen defs"),
            CallByName(ref symbol, ref loc_args, _) => {
                let func = self
                    .get_function(&*(symbol.clone()).into_boxed_str())
                    .unwrap_or_else(|| {
                        panic!("Roc compiler error: Unrecognized function `{:?}`", symbol)
                    });
                let enum_args: Vec<BasicValueEnum> = loc_args
                    .iter()
                    .map(|loc_arg| self.compile_expr(&loc_arg.value, vars).into())
                    .collect();
                let proc = self.procedures.get(&symbol).unwrap_or_else(|| {
                    panic!("Roc compiler error: Unrecognized procedure `{:?}`", symbol)
                });

                Typed(
                    proc.ret_var,
                    self.builder
                        .build_call(func, &enum_args, "tmp") // TODO replace "tmp"
                        .try_as_basic_value()
                        .left()
                        .unwrap_or_else(|| panic!("Roc compiler error: Invalid call.")),
                )
            }
            Str(_)
            | CallPointer(_, _, _)
            | List(_, _)
            | Case(_, _, _)
            | Record(_, _)
            | EmptyRecord
            | RuntimeError(_) => {
                panic!("TODO compile_expr for {:?}", expr);
            }
        }
    }
}

pub fn content_to_basic_type(
    content: Content,
    subs: &mut Subs,
    context: &Context,
) -> Result<BasicTypeEnum, String> {
    match content {
        Content::Structure(flat_type) => match flat_type {
            Apply {
                module_name,
                name,
                args,
            } => {
                if &*module_name == types::MOD_NUM && &*name == types::TYPE_NUM {
                    num_to_basic_type(subs.get(*args.iter().next().unwrap()).content, context)
                } else {
                    panic!(
                        "TODO handle content_to_basic_type for flat_type {}.{} with args {:?}",
                        module_name, name, args
                    );
                }
            }
            other => panic!("TODO handle content_to_basic_type for {:?}", other),
        },
        other => Err(format!("Cannot convert {:?} to BasicTypeEnum", other)),
    }
}

pub fn num_to_basic_type(content: Content, context: &Context) -> Result<BasicTypeEnum, String> {
    match content {
        Content::Structure(flat_type) => match flat_type {
            Apply {
                module_name,
                name,
                args,
            } => {
                if &*module_name == types::MOD_FLOAT && &*name == types::TYPE_FLOATINGPOINT && args.is_empty() {
                    debug_assert!(args.is_empty());
                    Ok(BasicTypeEnum::FloatType(context.f64_type()))
                } else if &*module_name == types::MOD_INT && &*name == types::TYPE_INTEGER && args.is_empty() {
                    debug_assert!(args.is_empty());
                    Ok(BasicTypeEnum::IntType(context.i64_type()))
                } else {
                    Err(format!("Unrecognized numeric type: Num {}.{} with args {:?}", module_name, name, args))
                }
            }
            other => panic!(
                "TODO handle content_to_basic_type (branch 0) for {:?} which is NESTED inside Num.Num",
                other
            ),
        },

        other => panic!(
            "TODO handle content_to_basic_type (branch 1) for {:?} which is NESTED inside Num.Num",
            other
        ),
    }
}
