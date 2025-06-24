#!/usr/bin/env python3.12
try:
    import lldb # For type hinting and when LLDB executes the functions.
except ImportError:
    # This allows the script to run standalone for the argparse and subprocess part.
    pass
import os
import argparse
import subprocess
import sys

# Global variable to store the source file path provided by the user.
TARGET_SOURCE_FILE_PATH_FOR_CALLBACK = None

# Global variable to track the current function name
CURRENT_FUNCTION_NAME = None
PREV_LINE_NUM = None

def print_line_and_continue(frame, bp_loc, internal_dict):
    """
    LLDB Callback: Called when a line breakpoint is hit. Prints the current line and continues.
    """
    global TARGET_SOURCE_FILE_PATH_FOR_CALLBACK, CURRENT_FUNCTION_NAME, PREV_LINE_NUM
    if not frame or TARGET_SOURCE_FILE_PATH_FOR_CALLBACK is None:
        if frame:
             process = frame.GetThread().GetProcess()
             if process and process.is_alive:
                process.Continue()
        return True

    line_entry = frame.GetLineEntry()
    if line_entry.IsValid():
        file_spec = line_entry.GetFileSpec()
        filename_from_debug = file_spec.GetFilename()
        directory_from_debug = file_spec.GetDirectory()
        line_num = line_entry.GetLine()

        path_in_debug_info = os.path.join(directory_from_debug, filename_from_debug) if directory_from_debug else filename_from_debug

        # Normalize paths for robust comparison
        norm_debug_path = os.path.normpath(path_in_debug_info)
        norm_target_path_for_callback = os.path.normpath(TARGET_SOURCE_FILE_PATH_FOR_CALLBACK)

        # Check if the path from debug info ends with our target path
        if norm_debug_path.endswith(norm_target_path_for_callback):
            # Get function name from the current frame
            function_name = frame.GetFunctionName()
            if not function_name:
                function_name = "<unknown>"
            
            # Check if we've entered a new function
            if CURRENT_FUNCTION_NAME != function_name:
                # Split on '::' and take the second part if it exists
                split_function = function_name.split('::')
                fun_display_name_list = split_function[1:-1] if '::' in function_name and len(split_function) > 3 else split_function
                fun_display_name = "::".join(fun_display_name_list)
                print(f"=> function: {fun_display_name}")
                CURRENT_FUNCTION_NAME = function_name
            
            line_content = ""
            source_file_to_read = os.path.abspath(TARGET_SOURCE_FILE_PATH_FOR_CALLBACK)

            try:
                if os.path.exists(source_file_to_read):
                    with open(source_file_to_read, 'r') as f:
                        lines = f.readlines()
                    if 0 < line_num <= len(lines):
                        line_content = lines[line_num - 1].rstrip()
                    else:
                        line_content = f"<line {line_num} out of bounds in {source_file_to_read}>"
                else:
                    line_content = f"<source file not found at {source_file_to_read}>"
            except Exception as e:
                line_content = f"<error reading source {source_file_to_read}: {e}>"

            if line_num != PREV_LINE_NUM:
                print(f"* {line_num:>5} {line_content}")
            
            PREV_LINE_NUM = line_num

    process = frame.GetThread().GetProcess()
    if process and process.is_alive:
        process.Continue()
    return True


def set_breakpoints_for_file(debugger, user_provided_source_file_path, internal_dict_ignored, command_ignored=None):
    """
    LLDB Callback: Sets a breakpoint on each line of the specified file.
    """
    path_for_line_counting = os.path.abspath(user_provided_source_file_path)

    if not os.path.exists(path_for_line_counting):
        error_msg = f"Error: Source file '{path_for_line_counting}' not found. Cannot set line breakpoints."
        print(error_msg)
        if 'result' in internal_dict_ignored and hasattr(internal_dict_ignored['result'], 'SetError'):
             internal_dict_ignored['result'].SetError(error_msg)
        return

    try:
        with open(path_for_line_counting, 'r') as f:
            lines = f.readlines()
        num_lines = len(lines)
    except Exception as e:
        error_msg = f"Error reading {path_for_line_counting}: {e}"
        print(error_msg)
        if 'result' in internal_dict_ignored and hasattr(internal_dict_ignored['result'], 'SetError'):
            internal_dict_ignored['result'].SetError(error_msg)
        return

    if num_lines == 0:
        print(f"Note: {path_for_line_counting} is empty. No breakpoints set.")
        return

    print(f"Setting breakpoints for {num_lines} lines in {user_provided_source_file_path} (resolved to {path_for_line_counting})...")

    target = debugger.GetSelectedTarget()
    if not target:
        print("Error: No target selected in debugger.")
        return

    lldb_module_name = os.path.splitext(os.path.basename(__file__))[0]
    callback_function_name = f"{lldb_module_name}.print_line_and_continue"

    for i in range(1, num_lines + 1):
        breakpoint = target.BreakpointCreateByLocation(user_provided_source_file_path, i)

        if breakpoint and breakpoint.IsValid():
            if breakpoint.GetNumLocations() > 0:
                breakpoint.SetScriptCallbackFunction(callback_function_name)

    print(f"Finished setting breakpoints for {user_provided_source_file_path}.")


def __lldb_init_module(debugger, internal_dict):
    """
    LLDB auto-calls this when the script is imported via `command script import`.
    """
    pass


def parse_arguments():
    """
    Parse command line arguments, handling the -- separator for binary arguments.
    """
    # Find the position of '--' in sys.argv
    separator_index = None
    try:
        separator_index = sys.argv.index('--')
    except ValueError:
        # No '--' found, parse normally
        pass
    
    if separator_index is not None:
        # Split arguments at the '--' separator
        script_args = sys.argv[1:separator_index]
        binary_args = sys.argv[separator_index + 1:]
    else:
        # No separator found, all args are for the script
        script_args = sys.argv[1:]
        binary_args = []
    
    # Parse the script arguments
    parser = argparse.ArgumentParser(
        description="Trace line execution of a binary using LLDB. This script generates and runs an LLDB command script.\n\n"
                   "Usage examples:\n"
                   "  %(prog)s src/main.rs target/debug/mybinary\n"
                   "  %(prog)s src/main.rs target/debug/mybinary -- arg1 arg2 arg3\n",
        formatter_class=argparse.RawTextHelpFormatter
    )
    parser.add_argument("source_file", help="The source file to trace (e.g., src/main.rs or an absolute path)")
    parser.add_argument("binary_path", help="Path to the target binary executable")
    
    # Parse only the script arguments
    args = parser.parse_args(script_args)
    
    # Add binary arguments to the args object
    args.binary_args = binary_args
    
    return args


if __name__ == "__main__":
    # This block executes when the script is run directly from the shell
    args = parse_arguments()

    this_script_path = os.path.abspath(__file__)
    this_script_module_name = os.path.splitext(os.path.basename(this_script_path))[0]

    user_source_file_norm = os.path.normpath(args.source_file)

    # Prepare the process launch command with arguments
    if args.binary_args:
        # Escape arguments for LLDB command
        escaped_args = []
        for arg in args.binary_args:
            # Simple escaping - wrap in quotes if contains spaces
            if ' ' in arg or '"' in arg:
                escaped_arg = '"' + arg.replace('"', '\\"') + '"'
            else:
                escaped_arg = arg
            escaped_args.append(escaped_arg)
        
        process_launch_cmd = f"process launch -- {' '.join(escaped_args)}"
        print(f"INFO: Will launch binary with arguments: {args.binary_args}")
    else:
        process_launch_cmd = "process launch"
        print("INFO: Will launch binary without arguments")

    # LLDB commands to be executed
    lldb_commands_string = f"""
command script import "{this_script_path}"
script {this_script_module_name}.TARGET_SOURCE_FILE_PATH_FOR_CALLBACK = "{user_source_file_norm}"
script {this_script_module_name}.CURRENT_FUNCTION_NAME = None
script {this_script_module_name}.set_breakpoints_for_file(lldb.debugger, "{user_source_file_norm}", {{}}, None)
{process_launch_cmd}
quit
"""

    temp_lldb_script_file = "temp_lldb_driver_script.lldb"
    with open(temp_lldb_script_file, 'w') as f:
        f.write(lldb_commands_string)

    print(f"INFO: Generated temporary LLDB script: {temp_lldb_script_file}")
    lldb_exe_path = "lldb"

    # Construct the command to run LLDB
    command_list = [lldb_exe_path, "-b", "-s", temp_lldb_script_file, "--", args.binary_path]

    print(f"INFO: Executing LLDB: {' '.join(command_list)}")
    print("--- LLDB Output Start ---")
    try:
        process_result = subprocess.run(command_list, capture_output=True, text=True, check=False)

        print(process_result.stdout)
        if process_result.stderr:
            print("--- LLDB Stderr ---")
            print(process_result.stderr)

        if process_result.returncode != 0:
            print(f"INFO: LLDB exited with code {process_result.returncode}")

    except FileNotFoundError:
        print(f"Error: LLDB executable ('{lldb_exe_path}') not found. Please ensure LLDB is installed and in your PATH.")
    except Exception as e:
        print(f"An error occurred while running LLDB: {e}")
    finally:
        if os.path.exists(temp_lldb_script_file):
            os.remove(temp_lldb_script_file)
    print("--- LLDB Output End ---")
