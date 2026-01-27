app [make_glue] { pf: platform "../platform/main.roc" }

import pf.Types exposing [Types]
import pf.File exposing [File]

make_glue : List(Types) -> Try(List(File), Str)
make_glue = |types_list| {
    # Debug: print the types list (includes module info)
    dbg types_list

    # Collect all hosted functions from all modules, with module name prefix
    var $hosted_functions = []
    for types in types_list {
        for mod in types.modules {
            for func in mod.hosted_functions {
                # Prefix function name with module name for uniqueness
                full_name = "${mod.name}.${func.name}"
                $hosted_functions = $hosted_functions.append({ index: func.index, name: full_name, type_str: func.type_str })
            }
        }
    }

    # Generate the header content
    header_content = generate_header($hosted_functions)

    Ok([{ name: "roc_platform_abi.h", content: header_content }])
}

# Parse a type string like "Str => {}" into { args: List Str, ret: Str }
parse_type_str : Str -> { args : List(Str), ret : Str }
parse_type_str = |type_str| {
    # Split on " => " to separate args from return type
    parts = Str.split_on(type_str, " => ")

    match parts {
        [args_part, ret_part] => {
            args = parse_args(args_part)
            { args, ret: ret_part }
        }

        _ => {
            # If no "=>" found, assume it is a pure function with "->"
            arrow_parts = Str.split_on(type_str, " -> ")
            match arrow_parts {
                [args_part2, ret_part2] => {
                    args = parse_args(args_part2)
                    { args, ret: ret_part2 }
                }

                _ => { args: [], ret: type_str }
            }
        }
    }
}

# Get first element or empty string
get_first_or_empty : List(Str) -> Str
get_first_or_empty = |list| {
    match list.first() {
        Ok(s) => s
        Err(_) => ""
    }
}

# Get last element or empty string
get_last_or_empty : List(Str) -> Str
get_last_or_empty = |list| {
    match list.last() {
        Ok(s) => s
        Err(_) => ""
    }
}

# Parse args portion of type string
parse_args : Str -> List(Str)
parse_args = |args_part| {
    trimmed = Str.trim(args_part)
    if trimmed == "()" or trimmed == "" or trimmed == "({})" or trimmed == "{}" {
        []
    } else {
        # Strip parentheses if present
        stripped =
            if Str.starts_with(trimmed, "(") and Str.ends_with(trimmed, ")") {
                drop_first_last(trimmed)
            } else {
                trimmed
            }
        # Handle comma-separated args like "Str, U64"
        var $result = []
        for s in Str.split_on(stripped, ", ") {
            t = Str.trim(s)
            if t != "" and t != "{}" {
                $result = $result.append(t)
            }
        }
        $result
    }
}

# Drop first and last character from string
drop_first_last : Str -> Str
drop_first_last = |s| {
    bytes = Str.to_utf8(s)
    len = List.len(bytes)
    if len <= 2 {
        ""
    } else {
        # Drop first and last byte using sublist
        new_bytes = List.sublist(bytes, { start: 1, len: len - 2 })
        match Str.from_utf8(new_bytes) {
            Ok(str) => str
            Err(_) => s
        }
    }
}

# Map a Roc type to its C equivalent
roc_type_to_c : Str -> Str
roc_type_to_c = |roc_type| {
    trimmed = Str.trim(roc_type)

    if trimmed == "Str" {
        "RocStr"
    } else if trimmed == "Bool" {
        "bool"
    } else if trimmed == "U8" {
        "uint8_t"
    } else if trimmed == "U16" {
        "uint16_t"
    } else if trimmed == "U32" {
        "uint32_t"
    } else if trimmed == "U64" {
        "uint64_t"
    } else if trimmed == "U128" {
        "unsigned __int128"
    } else if trimmed == "I8" {
        "int8_t"
    } else if trimmed == "I16" {
        "int16_t"
    } else if trimmed == "I32" {
        "int32_t"
    } else if trimmed == "I64" {
        "int64_t"
    } else if trimmed == "I128" {
        "__int128"
    } else if trimmed == "F32" {
        "float"
    } else if trimmed == "F64" {
        "double"
    } else if trimmed == "{}" or trimmed == "()" {
        "void"
    } else if Str.starts_with(trimmed, "List") {
        "RocList"
    } else {
        # Records, tag unions, and other complex types become void*
        "void*"
    }
}

# Convert function name to uppercase identifier (e.g., "Stdout.line!" -> "STDOUT_LINE")
name_to_upper_ident : Str -> Str
name_to_upper_ident = |name| {
    step1 = str_replace_all(name, ".", "_")
    step2 = str_replace_all(step1, "!", "")
    to_screaming_snake_case(step2)
}

# Replace all occurrences of a substring
str_replace_all : Str, Str, Str -> Str
str_replace_all = |s, from, to| {
    parts = Str.split_on(s, from)
    Str.join_with(parts, to)
}

# Convert a string to SCREAMING_SNAKE_CASE
to_screaming_snake_case : Str -> Str
to_screaming_snake_case = |s| {
    bytes = Str.to_utf8(s)
    var $output = []
    var $prev_was_lower = Bool.False

    for byte in bytes {
        is_upper = byte >= 65 and byte <= 90 # A-Z
        is_lower = byte >= 97 and byte <= 122 # a-z

        new_byte =
            if is_lower {
                byte - 32 # Convert to uppercase
            } else {
                byte
            }

        if is_upper and $prev_was_lower {
            # Insert underscore before uppercase following lowercase
            $output = $output.append(95) # 95 is underscore
        }
        $output = $output.append(new_byte)
        $prev_was_lower = is_lower
    }

    match Str.from_utf8($output) {
        Ok(str) => str
        Err(_) => s
    }
}

# Convert function name to a C-friendly struct name (e.g., "Stdout.line!" -> "StdoutLine")
# Keep module prefix to avoid conflicts between modules with same function names
name_to_struct_name : Str -> Str
name_to_struct_name = |name| {
    # For names like "line!" without module prefix, just remove special chars
    # For names like "Stdout.line!", convert to "StdoutLine"
    step1 = str_replace_all(name, ".", "")
    str_replace_all(step1, "!", "")
}

# Convert function name to C field name with module prefix (e.g., "Stdout.line!" -> "Stdout_line")
name_to_field_name : Str -> Str
name_to_field_name = |name| {
    step1 = str_replace_all(name, ".", "_")
    str_replace_all(step1, "!", "")
}

# Generate args struct for a function if it has arguments
generate_args_struct : { index : U64, name : Str, type_str : Str } -> Str
generate_args_struct = |func| {
    parsed = parse_type_str(func.type_str)

    if List.is_empty(parsed.args) {
        ""
    } else {
        struct_name = name_to_struct_name(func.name)
        var $fields = ""
        var $idx = 0
        for arg in parsed.args {
            c_type = roc_type_to_c(arg)
            if $idx > 0 {
                $fields = Str.concat($fields, "\n")
            }
            $fields = Str.concat($fields, "    ${c_type} arg${U64.to_str($idx)};")
            $idx = $idx + 1
        }

        "// Arguments for ${func.name}\ntypedef struct {\n${$fields}\n} ${struct_name}Args;\n\n"
    }
}

# Generate the complete C header file
generate_header : List({ index : U64, name : Str, type_str : Str }) -> Str
generate_header = |hosted_functions| {
    # Generate defines for function indices
    defines = generate_defines(hosted_functions)

    count = List.len(hosted_functions)

    # Generate args structs for functions with arguments
    args_structs = generate_all_args_structs(hosted_functions)

    # Generate static asserts for struct sizes
    static_asserts = generate_static_asserts(hosted_functions)

    # Generate HostedFunctions struct fields
    hosted_fn_fields = generate_hosted_fn_fields(hosted_functions)

    header_guard_top({})
        .concat(includes_section({}))
        .concat(extern_c_start({}))
        .concat(core_types_section({}))
        .concat(hosted_fn_infrastructure({}))
        .concat(function_count_section(count))
        .concat(defines)
        .concat("\n\n")
        .concat(args_structs_header({}))
        .concat(args_structs)
        .concat(static_asserts)
        .concat("\n")
        .concat(hosted_functions_registry(hosted_fn_fields))
        .concat(extern_c_end({}))
        .concat(header_guard_bottom({}))
}

# Generate defines for function indices
generate_defines : List({ index : U64, name : Str, type_str : Str }) -> Str
generate_defines = |hosted_functions| {
    var $defines = ""
    var $first = Bool.True
    for func in hosted_functions {
        upper_name = name_to_upper_ident(func.name)
        if !$first {
            $defines = Str.concat($defines, "\n")
        }
        $defines = Str.concat($defines, "#define HOSTED_IDX_${upper_name} ${U64.to_str(func.index)}")
        $first = Bool.False
    }
    $defines
}

# Generate all args structs
generate_all_args_structs : List({ index : U64, name : Str, type_str : Str }) -> Str
generate_all_args_structs = |hosted_functions| {
    var $args_structs = ""
    for f in hosted_functions {
        $args_structs = Str.concat($args_structs, generate_args_struct(f))
    }
    $args_structs
}

# Generate static asserts for struct sizes
generate_static_asserts : List({ index : U64, name : Str, type_str : Str }) -> Str
generate_static_asserts = |hosted_functions| {
    var $static_asserts = ""
    var $first = Bool.True
    for f in hosted_functions {
        parsed = parse_type_str(f.type_str)
        if !(List.is_empty(parsed.args)) {
            struct_name = name_to_struct_name(f.name)
            if !$first {
                $static_asserts = Str.concat($static_asserts, "\n")
            }
            $static_asserts = Str.concat($static_asserts, "_Static_assert(sizeof(${struct_name}Args) > 0, \"${struct_name}Args must have non-zero size\");")
            $first = Bool.False
        }
    }
    $static_asserts
}

# Generate HostedFunctions struct fields
generate_hosted_fn_fields : List({ index : U64, name : Str, type_str : Str }) -> Str
generate_hosted_fn_fields = |hosted_functions| {
    var $fields = ""
    var $first = Bool.True
    for f in hosted_functions {
        field_name = name_to_field_name(f.name)
        if !$first {
            $fields = Str.concat($fields, "\n")
        }
        $fields = Str.concat($fields, "    HostedFn ${field_name}; // index ${U64.to_str(f.index)}")
        $first = Bool.False
    }
    $fields
}

# Header sections as separate functions to avoid multi-line string issues

header_guard_top : {} -> Str
header_guard_top = |{}|
    "/**\n * Roc Platform ABI Header\n *\n * This file defines the C interface for hosted functions in a Roc platform.\n * It is automatically generated by the Roc glue generator.\n *\n * USAGE:\n * 1. Include this header in your platform host implementation\n * 2. Implement each hosted function according to its signature\n * 3. Register your implementations with the Roc runtime\n *\n */\n\n#ifndef ROC_PLATFORM_ABI_H\n#define ROC_PLATFORM_ABI_H\n\n"

includes_section : {} -> Str
includes_section = |{}|
    "#include <stdbool.h>\n#include <stdint.h>\n#include <stddef.h>\n\n"

extern_c_start : {} -> Str
extern_c_start = |{}|
    "#ifdef __cplusplus\nextern \"C\" {\n#endif\n\n"

extern_c_end : {} -> Str
extern_c_end = |{}|
    "\n#ifdef __cplusplus\n}\n#endif\n\n"

header_guard_bottom : {} -> Str
header_guard_bottom = |{}|
    "#endif // ROC_PLATFORM_ABI_H\n"

core_types_section : {} -> Str
core_types_section = |{}|
    "// =============================================================================\n// Core Roc Types\n// =============================================================================\n\n/**\n * RocStr - Roc string type\n *\n * A 24-byte structure representing a string. Small strings (up to 23 bytes\n * on 64-bit systems) are stored inline. Larger strings store a pointer to\n * heap-allocated data.\n */\ntypedef struct {\n    uint8_t* bytes;\n    size_t len;\n    size_t capacity;\n} RocStr;\n\n_Static_assert(sizeof(RocStr) == 24, \"RocStr must be 24 bytes\");\n\n/**\n * RocList - Roc list type\n *\n * A 24-byte structure representing a list. Similar to RocStr, but for\n * arbitrary element types.\n */\ntypedef struct {\n    void* elements;\n    size_t len;\n    size_t capacity;\n} RocList;\n\n_Static_assert(sizeof(RocList) == 24, \"RocList must be 24 bytes\");\n\n"

hosted_fn_infrastructure : {} -> Str
hosted_fn_infrastructure = |{}|
    "// =============================================================================\n// Hosted Function Infrastructure\n// =============================================================================\n\n/**\n * Forward declaration for RocOps\n * This structure contains function pointers for Roc runtime operations\n * (allocation, deallocation, etc.)\n */\nstruct RocOps;\n\n/**\n * HostedFn - Function pointer type for hosted functions\n *\n * All hosted functions follow this signature:\n *   - ops: pointer to Roc runtime operations\n *   - args: pointer to function-specific arguments struct (or NULL if no args)\n *   - ret: pointer to return value storage (or NULL if void return)\n */\ntypedef void (*HostedFn)(struct RocOps* ops, void* args, void* ret);\n\n"

function_count_section : U64 -> Str
function_count_section = |count|
    "// =============================================================================\n// Hosted Function Count and Indices\n// =============================================================================\n\n/**\n * Total number of hosted functions in this platform\n */\n#define HOSTED_FUNCTION_COUNT ${U64.to_str(count)}\n\n/**\n * Index constants for each hosted function\n * Use these with the HostedFunctions struct to access specific functions\n */\n"

args_structs_header : {} -> Str
args_structs_header = |{}|
    "// =============================================================================\n// Argument Structures\n// =============================================================================\n\n"

hosted_functions_registry : Str -> Str
hosted_functions_registry = |fields|
    "// =============================================================================\n// HostedFunctions Registry\n// =============================================================================\n\n/**\n * HostedFunctions - Registry of all hosted function implementations\n *\n * Platforms should create an instance of this struct and populate it with\n * function pointers for each hosted function they implement.\n */\ntypedef struct {\n${fields}\n} HostedFunctions;\n"
