# Hello, World!

Right now, there is only one way to build Roc programs: the Rube Goldberg Build Process.
(In the future, it will be nicer. At the moment, the nicer build system exists only 
in our imaginations...so Rube Goldberg it is!)

## Ingredients

1. A host. For this example, our host is implemented in the file `host.rs`.
2. A Roc function. For this example, we'll use a function which returns the Roc string `"Hello, World!"`
3. Having `gcc` installed. This will not be necessary in the future, but the Rube Goldberg build process needs it.

## Steps

1. `cd` into `examples/hello-world/`
2. Run `cargo run hello.roc` to compile the Roc source code into a `hello.o` file.
3. Run `gcc -shared hello.o -o libhello_from_roc.so` to generate `libhello_from_roc.so`. (This filename must begin with `lib` and end in `.so` or else `host.rs` won't be able to find it!)
4. Move `libhello_from_roc.so` onto the system library path, e.g. with `sudo mv libhello_from_roc.so /usr/local/lib/`
5. Run `rustc host.rs -o hello` to generate the `hello` executable.
6. Run `./hello` to see the greeting!

To run in release mode instead, do:

```bash
cargo run --release hello.roc
```

## Design Notes

This demonstrates the basic design of hosts: Roc code gets compiled into a pure 
function (in this case, a thunk that always returns `"Hello, World!"`) and
then the host calls that function. Fundamentally, that's the whole idea! The host
might not even have a `main` - it could be a library, a plugin, anything.
Everything else is built on this basic "hosts calling linked pure functions" design.

For example, things get more interesting when the compiled Roc function returns
a `Task` - that is, a tagged union data structure containing function pointers 
to callback closures. This lets the Roc pure function describe arbitrary 
chainable effects, which the host can interpret to perform I/O as requested by 
the Roc program.  (The tagged union `Task` would have a variant for each supported 
I/O operation.)

In this trivial example, it's very easy to line up the API between the host and
the Roc program. In a more involved host, this would be much trickier - especially
if the API were changing frequently during development.

The idea there is to have a first-class concept of "glue code" which host authors
can write (it would be plain Roc code, but with some extra keywords that aren't
available in normal modules - kinda like `port module` in Elm), and which
describe both the Roc-host/C boundary as well as the Roc-host/Roc-app boundary.
Roc application authors only care about the Roc-host/Roc-app portion, and the
host author only cares about the Roc-host/C bounary when implementing the host.

Using this glue code, the Roc compiler can generate C header files describing the
boundary. This not only gets us host compatibility with C compilers, but also 
Rust FFI for free, because [`rust-bindgen`](https://github.com/rust-lang/rust-bindgen) 
generates correct Rust FFI bindings from C headers.

The whole "calling gcc and rustc" part of the current build process is obviously
not something Roc application authors should ever need to do. Rather, the idea
would be to have the host precompiled into an object file (eliminating the
need for Roc authors to run `rustc` in this example) and for the Roc compiler
to not only generate the object file for the Roc file, but also to link it with
the host object file to produce an executable (eliminating the need for `gcc`)
such that Roc application authors can concern themselves exclusively with Roc code
and need only the Roc compiler to build and to execute it.

Of course, none of those niceties exist yet. But we'll get there!

## The test that builds things

```rust
let src = indoc!(
    r#"
    "Hello, World!"
    "#
);

// Build the expr
let arena = Bump::new();
let (loc_expr, _output, _problems, subs, var, constraint, home, interns) = uniq_expr(src);

let mut unify_problems = Vec::new();
let (content, mut subs) = infer_expr(subs, &mut unify_problems, &constraint, var);

let context = Context::create();
let module = context.create_module("app");
let builder = context.create_builder();
let fpm = PassManager::create(&module);

roc_gen::llvm::build::add_passes(&fpm);

fpm.initialize();

// Compute main_fn_type before moving subs to Env
let layout = Layout::from_content(&arena, content, &subs, crate::helpers::eval::POINTER_SIZE)
.unwrap_or_else(|err| panic!("Code gen error in test: could not convert to layout. Err was {:?} and Subs were {:?}", err, subs));

let execution_engine = module
    .create_jit_execution_engine(OptimizationLevel::None)
    .expect("Error creating JIT execution engine for test");

let ptr_bytes = execution_engine
    .get_target_data()
    .get_pointer_byte_size(None);
let main_fn_type =
    basic_type_from_layout(&arena, &context, &layout, ptr_bytes).fn_type(&[], false);
let main_fn_name = "$Test.main";

// Compile and add all the Procs before adding main
let mut env = roc_gen::llvm::build::Env {
    arena: &arena,
    builder: &builder,
    context: &context,
    interns,
    module: arena.alloc(module),
    ptr_bytes,
};
let mut procs = Procs::default();
let mut ident_ids = env.interns.all_ident_ids.remove(&home).unwrap();

// Populate Procs and get the low-level Expr from the canonical Expr
let main_body = Expr::new(
    &arena,
    &mut subs,
    loc_expr.value,
    &mut procs,
    home,
    &mut ident_ids,
    crate::helpers::eval::POINTER_SIZE,
);

// Put this module's ident_ids back in the interns, so we can use them in env.
env.interns.all_ident_ids.insert(home, ident_ids);

let mut headers = Vec::with_capacity(procs.len());

// Add all the Proc headers to the module.
// We have to do this in a separate pass first,
// because their bodies may reference each other.
for (symbol, opt_proc) in procs.as_map().into_iter() {
    if let Some(proc) = opt_proc {
        let (fn_val, arg_basic_types) = build_proc_header(&env, symbol, &proc);

        headers.push((proc, fn_val, arg_basic_types));
    }
}

// Build each proc using its header info.
for (proc, fn_val, arg_basic_types) in headers {
    // NOTE: This is here to be uncommented in case verification fails.
    // (This approach means we don't have to defensively clone name here.)
    //
    // println!("\n\nBuilding and then verifying function {}\n\n", name);
    build_proc(&env, proc, &procs, fn_val, arg_basic_types);

    if fn_val.verify(true) {
        fpm.run_on(&fn_val);
    } else {
        // NOTE: If this fails, uncomment the above println to debug.
        panic!("Non-main function failed LLVM verification. Uncomment the above println to debug!");
    }
}

// Add main to the module.
let main_fn = env.module.add_function(main_fn_name, main_fn_type, None);

main_fn.set_call_conventions(crate::helpers::eval::MAIN_CALLING_CONVENTION);
main_fn.set_linkage(Linkage::External);

// Add main's body
let basic_block = context.append_basic_block(main_fn, "entry");

builder.position_at_end(basic_block);

let ret = roc_gen::llvm::build::build_expr(
    &env,
    &ImMap::default(),
    main_fn,
    &main_body,
    &mut Procs::default(),
);

builder.build_return(Some(&ret));

// Uncomment this to see the module's un-optimized LLVM instruction output:
// env.module.print_to_stderr();

if main_fn.verify(true) {
    fpm.run_on(&main_fn);
} else {
    panic!("Function {} failed LLVM verification.", main_fn_name);
}

// Verify the module
if let Err(errors) = env.module.verify() {
    panic!("Errors defining module: {:?}", errors);
}

// Uncomment this to see the module's optimized LLVM instruction output:
// env.module.print_to_stderr();

// Emit
Target::initialize_x86(&InitializationConfig::default());

let opt = OptimizationLevel::Default;
let reloc = RelocMode::Default;
let model = CodeModel::Default;
let target = Target::from_name("x86-64").unwrap();
let target_machine = target
    .create_target_machine(
        &TargetTriple::create("x86_64-pc-linux-gnu"),
        "x86-64",
        "+avx2",
        opt,
        reloc,
        model,
    )
    .unwrap();

let path = Path::new("../../hello.o");

assert!(target_machine
    .write_to_file(&env.module, FileType::Object, &path)
    .is_ok());

let path = Path::new("../../hello.asm");

assert!(target_machine
    .write_to_file(&env.module, FileType::Assembly, &path)
    .is_ok());
```
