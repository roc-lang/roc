# Important things to add (Not necessarily in order)

- Expand to way more builtins and assembly calls.
- Deal with function calling, basic layouts, and all the fun basics of argument passing.
- Add floats and some basic operations with them.
- Add some more complex memory layouts and data types.
- Add builtin function creation and calling.
  For many builtins, we should only need to create them if they are used as a function pointer.
  This may not be know at gen time for the specific function, so we might just have to add them all.
  Otherwise, many will always be inlined.
- Add basic const folding? It should be really easy to add as an optimization.
  It should be a nice optimization for little cost. Just be sure to make it optional, otherwise our tests will do nothing.
- Automatically build the zig builtins .o file and make it available here.
  We will need to link against it and use it whenever we call specific builtins.
- Add unwind tables and landing pads.
- Add ability to wrap functions with exceptions or return a results.
  Will need to start dealing with overflows and such to return errors.
