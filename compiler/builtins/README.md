# So you want to add a builtin?

Builtins are the functions and modules that are implicitly imported into every module. Some of them compile down to llvm, others need to be constructed and defined. Making a new builtin means touching many files. Here is what it takes:

### module/src/symbol.rs

Towards the bottom of a file there is a `define_builtins!` macro being used that takes many modules and function names. The first level (`List`, `Int` ..) is the module name, and the second level is the function or value name (`reverse`, `mod` ..). If you wanted to add a `Int` function called `addTwo` go to `2 Int: "Int" => {` and inside that case add to the bottom `38 INT_ADD_TWO: "addTwo"` (assuming there are 37 existing ones). 

Some of these have `#` inside their name (`first#list`, #lt` ..). This is a trick we are doing to hide implementation details from Roc programmers. To a Roc programmer, a name with `#` in it is invalid, because `#` means everything after it is parsed to a comment. We are constructing these functions manually, so we are circumventing the parsing step and dont have such restrictions. We get to make functions and values with `#` which as a consequence are not accessible to Roc programmers. Roc programmers simply cannot reference them.

But we can use these values and some of these are necessary for implementing builtins. For example, `List.get` returns tags, and it is not easy for us to create tags when composing LLVM. What is easier however, is:
- ..writing `List.#getUnsafe` that has the dangerous signature of `List elem, Int -> elem` in LLVM
- ..writing `List elem, Int -> Result elem [ OutOfBounds ]*` in a type safe way that uses `getUnsafe` internally, only after it checks if the `elem` at `Int` index exists.

## Bottom level LLVM values and functions
### gen/src/llvm/build.rs
This is where bottom-level functions that need to be written as LLVM are created. If the function leads to a tag thats a good sign it should not be written here in `build.rs`. If its simple fundamental stuff like `INT_ADD` then it certainly should be written here.

## More abstract values and functions that likely return tags.
### can/src/builtins.rs
If the function you are making is _not_ low level or returns something like a tag, then it should probably be written here by means of lower level functions written in `build.rs`.

## Letting the compiler know these functions exist
Its one thing to actually write these functions, its _another_ thing to let the Roc compiler know they exist. You have to tell the compiler "Hey, this function exists, and it has this type signature". That happens in these modules:
### builtins/src/std.rs
### builtins/src/unique.rs