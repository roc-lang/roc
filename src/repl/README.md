## REPL (Read-Eval-Print Loop)

The REPL provides an interactive environment for evaluating Roc expressions and building up state through assignments. It maintains a cumulative compilation context that allows expressions to reference previously defined variables.

### REPL Features

#### **Variable Definitions and References**
- **Assignments**: Use `x = 5` to define variables that persist across REPL steps
- **Variable References**: Use defined variables in subsequent expressions like `x + 1`
- **Redefinition**: Variables can be redefined, affecting all future references
- **Cumulative State**: Each REPL step builds on previous definitions

#### **Expression Evaluation**
- **Simple Expressions**: `1 + 2`, `"Hello"`, `True`
- **Variable References**: `x`, `y` (must be previously defined)
- **Complex Expressions**: `x + y * 2`, `if x > 0 then "positive" else "negative"`

#### **Output Format**
- **Assignments**: Produce descriptive output like `assigned 'x'`
- **Expressions**: Show the evaluated result
- **Errors**: Display error messages for invalid inputs
- **Step Separation**: Multiple outputs separated by `---`

#### **Special Commands**
- `:help` - Show help information
- `:exit`, `:quit`, `:q` - Exit the REPL
- Empty input - No output (silent)

### REPL Implementation Details

#### **Cumulative Compilation State**
The REPL maintains a `ModuleEnv` that accumulates:
- **Variable Definitions**: Map of variable names to source strings
- **Compilation Context**: Type information, canonical forms, and evaluation state
- **Debug HTML Storage**: Pre-rendered CAN and TYPES HTML for snapshot generation

#### **Evaluation Pipeline**
1. **Input Parsing**: Parse as statement (assignment) or expression
2. **State Building**: For expressions, build full source with all definitions
3. **Compilation**: Parse, canonicalize, type-check, and evaluate
4. **Debug Storage**: Store CAN/TYPES HTML if debug mode enabled
5. **Output Generation**: Format and return result

#### **Debug HTML Storage**
When enabled via `enableDebugSnapshots()`, the REPL stores:
- **CAN HTML**: Pre-rendered canonical forms for each step
- **TYPES HTML**: Pre-rendered type information for each step
- **Cumulative State**: Each step builds on previous definitions

### Example REPL Session

```roc
» x = 5
assigned `x`
» y = x + 1
assigned `y`
» x = 3
assigned `x`
» y
4
» x + y
7
```
>>>>>>> repl-eval

## Validation and Testing

The primary method for validating eval behavior is through **REPL snapshots**. These are comprehensive integration tests that capture the complete evaluation pipeline from source code to final output.

#### Running REPL Snapshots

Run all REPL snapshots to check for any changes in expected output:
```bash
zig build snapshot
```

Run a specific REPL snapshot with trace evaluation for debugging:
```bash
zig build snapshot -- --trace-eval src/snapshots/repl/your_test.md
```

#### REPL Snapshot Format

REPL snapshots are markdown files that capture the complete evaluation pipeline, including cumulative state and variable definitions. They show the canonical forms and type information for each REPL step.

```markdown
# META
~~~ini
description=Simple variable definitions and expressions
type=repl
~~~
# SOURCE
~~~roc
» x = 1
» y = 2
» x + y
~~~
# OUTPUT
assigned `x`
---
assigned `y`
---
3
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(e-block @1.1-5.2
	(s-let @2.5-2.10
		(p-assign @2.5-2.6 (ident "x"))
		(e-int @2.9-2.10 (value "1")))
	(s-let @3.5-3.10
		(p-assign @3.5-3.6 (ident "y"))
		(e-int @3.9-3.10 (value "2")))
	(e-binop @4.5-4.10 (op "add")
		(e-lookup-local @4.5-4.6
			(p-assign @2.5-2.6 (ident "x")))
		(e-lookup-local @4.9-4.10
			(p-assign @3.5-3.6 (ident "y")))))
~~~
# TYPES
~~~clojure
(expr @1.1-5.2 (type "Num(_size)"))
~~~
```

#### **Key Features of REPL Snapshots:**

- **Cumulative State**: Each step builds on previous variable definitions
- **Variable Resolution**: CAN/TYPES sections show proper variable lookup (no "ident_not_in_scope" errors)
- **Descriptive Output**: Assignments show `assigned 'x'` format
- **Step-by-Step Tracking**: Each REPL input generates one output and one CAN/TYPES entry
- **Debug HTML Storage**: Pre-rendered HTML for efficient snapshot generation

#### Creating New REPL Snapshots

1. **Create a new `.md` file** in `src/snapshots/repl/`
2. **Add your test cases** in the SOURCE section using `» ` prefix for each REPL input
3. **Run `zig build snapshot`** to generate expected outputs with cumulative state
4. **Use `--trace-eval`** for debugging specific scenarios and seeing evaluation traces

#### **REPL Snapshot Best Practices:**

- **Test Variable Definitions**: Include assignments to test cumulative state
- **Test Variable References**: Use defined variables in subsequent expressions
- **Test Redefinition**: Redefine variables to test state updates
- **Test Complex Expressions**: Include expressions that reference multiple variables
- **Test Error Cases**: Include invalid inputs to test error handling

#### **Example Test Scenarios:**

```roc
# Test basic assignments and references
» x = 5
» y = x + 1
» x + y

# Test variable redefinition
» x = 3
» y

# Test complex expressions
» z = x * y + 10
» if z > 20 then "large" else "small"
```

### Technical Implementation

#### **Debug HTML Storage System**

The REPL includes a sophisticated debug HTML storage system for efficient snapshot generation:

- **`enableDebugSnapshots()`**: Enables storage of pre-rendered HTML
- **`debug_can_html`**: Stores canonical forms for each REPL step
- **`debug_types_html`**: Stores type information for each REPL step
- **`generateAndStoreDebugHtml()`**: Called during evaluation to store HTML
- **`getDebugCanHtml()` / `getDebugTypesHtml()`**: Retrieve stored HTML for snapshots

#### **Cumulative State Management**

- **Variable Definitions**: Stored in `definitions` HashMap
- **ModuleEnv Persistence**: `last_module_env` maintains compilation state
- **Block Expression Building**: Expressions are wrapped with all definitions
- **State Transfer**: ModuleEnv ownership transferred to heap for persistence

#### **Evaluation Pipeline Integration**

The REPL integrates with the full compilation pipeline:
1. **Parse**: Statement or expression parsing
2. **Canonicalize**: Convert to canonical form
3. **Type Check**: Verify types and resolve variables
4. **Evaluate**: Runtime evaluation with interpreter
5. **Store**: Save debug HTML and ModuleEnv state

#### **Debugging Features**

- **`--trace-eval`**: Shows detailed evaluation traces
- **Debug HTML Storage**: Pre-rendered HTML for efficient snapshots
- **Cumulative State Tracking**: Each step builds on previous state
- **Variable Resolution**: Proper lookup in cumulative context
