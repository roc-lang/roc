# So you want to add a builtin?

Builtins are the functions and modules that are implicitly imported into every module. All of them compile down to llvm, but some are implemented directly as llvm and others in terms of intermediate functions. Either way, making a new builtin means touching many files. Lets make it easy for you and just list out which modules you need to visit to make a builtin. Here is what it takes:

## A builtin written only in Roc

Edit the appropriate `roc/*.roc` file with your new implementation. All normal rules for writing Roc code apply. Be sure to add a declaration, definition, some documentation and add it to the exposes list it in the module head.

Next, look towards the bottom of the `compiler/module/src/symbol.rs` file. Inside the `define_builtins!` macro, there is a list for each of the builtin modules and the function or value names it contains. Add a new entry to the appropriate list for your new function.

For each of the builtin modules, there is a file in `compiler/test_gen/src/` like `gen_num.rs`, `gen_str.rs` etc. Add new tests for the module you are changing to the appropriate file here. You can look at the existing test cases for examples and inspiration.

You can run your new tests locally using `cargo test-gen-llvm`. You can add a filter like `cargo test-gen-llvm gen_str` (to only run tests defined in `gen_str.rs`) or `cargo test-gen-llvm gen_str::str_split_on` (to only run tests defined in `gen_str` whose names start with `str_split`). More details can be found in the README in the `compiler/test_gen` directory.

## A builtin implemented directly as LLVM

### module/src/symbol.rs

Towards the bottom of `symbol.rs` there is a `define_builtins!` macro being used that takes many modules and function names. The first level (`List`, `Int` ..) is the module name, and the second level is the function or value name (`reverse`, `rem` ..). If you wanted to add a `Int` function called `add_two` go to `2 Int: "Int" => {` and inside that case add to the bottom `38 INT_ADD_TWO: "add_two"` (assuming there are 37 existing ones).

Some of these have `#` inside their name (`first#list`, `#lt` ..). This is a trick we are doing to hide implementation details from Roc programmers. To a Roc programmer, a name with `#` in it is invalid, because `#` means everything after it is parsed to a comment. We are constructing these functions manually, so we are circumventing the parsing step and don't have such restrictions. We get to make functions and values with `#` which as a consequence are not accessible to Roc programmers. Roc programmers simply cannot reference them.

But we can use these values and some of these are necessary for implementing builtins. For example, `List.get` returns tags, and it is not easy for us to create tags when composing LLVM. What is easier however, is:

- ..writing `List.#get_unsafe` that has the dangerous signature of `List elem, U64 -> elem` in LLVM
- ..writing `List elem, U64 -> Result elem [OutOfBounds]*` in a type safe way that uses `get_unsafe` internally, only after it checks if the `elem` at `U64` index exists.

### can/src/builtins.rs

Right at the top of this module is a function called `builtin_defs`. All this is doing is mapping the `Symbol` defined in `module/src/symbol.rs` to its implementation. Some of the builtins are quite complex, such as `list_get`. What makes `list_get` is that it returns tags, and in order to return tags it first has to defer to lower-level functions via an if statement.

Lets look at `List.repeat : elem, U64 -> List elem`, which is more straightforward, and points directly to its lower level implementation:

```rust
fn list_repeat(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let elem_var = var_store.fresh();
    let len_var = var_store.fresh();
    let list_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::ListRepeat,
        args: vec![
            (elem_var, Var(Symbol::ARG_1)),
            (len_var, Var(Symbol::ARG_2)),
        ],
        ret_var: list_var,
    };

    defn(
        symbol,
        vec![(elem_var, Symbol::ARG_1), (len_var, Symbol::ARG_2)],
        var_store,
        body,
        list_var,
    )
}
```

In these builtin definitions you will need to allocate for and list the arguments. For `List.repeat`, the arguments are the `elem_var` and the `len_var`. So in both the `body` and `defn` we list these arguments in a vector, with the `Symbol::ARG_1` and `Symvol::ARG_2` designating which argument is which.

Since `List.repeat` is implemented entirely as low level functions, its `body` is a `RunLowLevel`, and the `op` is `LowLevel::ListRepeat`. Lets talk about `LowLevel` in the next section.

## Connecting the definition to the implementation

### module/src/low_level.rs

This `LowLevel` thing connects the builtin defined in this module to its implementation. It's referenced in `can/src/builtins.rs` and it is used in `gen/src/llvm/build.rs`.

## Bottom level LLVM values and functions

### gen/src/llvm/build.rs

This is where bottom-level functions that need to be written as LLVM are created. If the function leads to a tag that's a good sign it should not be written here in `build.rs`. If it's simple fundamental stuff like `INT_ADD` then it certainly should be written here.

## Letting the compiler know these functions exist

### builtins/src/std.rs

It's one thing to actually write these functions, it's _another_ thing to let the Roc compiler know they exist as part of the standard library. You have to tell the compiler "Hey, this function exists, and it has this type signature". That happens in `std.rs`.

## Specifying how we pass args to the function

### builtins/mono/src/inc_dec.rs

After we have all of this, we need to specify if the arguments we're passing are owned, borrowed or irrelevant. Towards the bottom of this file, add a new case for your builtin and specify each arg. Be sure to read the comment, as it explains this in more detail.

## Testing it

### solve/tests/solve_expr.rs

To make sure that Roc is properly inferring the type of the new builtin, add a test to this file similar to:

```rust
 #[test]
fn atan() {
    infer_eq_without_problem(
        indoc!(
            r#"
            Num.atan
            "#
        ),
        "Frac a -> Frac a",
    );
}
```

But replace `Num.atan` and the type signature with the new builtin.

### test_gen/test/\*.rs

In this directory, there are a couple files like `gen_num.rs`, `gen_str.rs`, etc. For the `Str` module builtins, put the test in `gen_str.rs`, etc. Find the one for the new builtin, and add a test like:

```rust
#[test]
fn atan() {
    assert_evals_to!("Num.atan 10", 1.4711276743037347, f64);
}
```

But replace `Num.atan`, the return value, and the return type with your new builtin.

## Mistakes that are easy to make!!

When implementing a new builtin, it is often easy to copy and paste the implementation for an existing builtin. This can take you quite far since many builtins are very similar, but it also risks forgetting to change one small part of what you copy and pasted and losing a lot of time later on when you cant figure out why things don't work. So, speaking from experience, even if you are copying an existing builtin, try and implement it manually without copying and pasting. Two recent instances of this (as of September 7th, 2020):

- `List.keep_if` did not work for a long time because in builtins its `LowLevel` was `ListMap`. This was because I copy and pasted the `List.map` implementation in `builtins.rs
- `List.walk_backwards` had mysterious memory bugs for a little while because in `unique.rs` its return type was `list_type(flex(b))` instead of `flex(b)` since it was copy and pasted from `List.keep_if`.
