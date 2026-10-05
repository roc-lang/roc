#!/usr/bin/env python3
# encoding: utf-8

# Based on http://pymotw.com/2/sys/tracing.html, "Tracing a program as it runs"
# and http://hg.python.org/cpython/file/2.7/Lib/trace.py

import imp
import sys
import os
import struct

fifo_file = None
report_trace_real = None

try:
    # In Py 2.x, the builtins were in __builtin__
    BUILTINS = sys.modules['__builtin__']
except KeyError:
    # In Py 3.x, they're in builtins
    BUILTINS = sys.modules['builtins']


def report_trace3(file, line):
    size = len(file) + 1 + 8 + 4 + 4
    data = struct.pack(">QLL%dsb" % len(file), 0x6d6574616c6c6775, size, int(line), bytes(file, 'utf-8'), 0)

    fifo_file.write(data)

def report_trace2(file, line):
    size = len(file) + 1 + 8 + 4 + 4
    data = struct.pack(">QLL%dsb" % len(file), 0x6d6574616c6c6775, size, int(line), file, 0)

    fifo_file.write(data)

def report_trace(file, line):
    try:
        report_trace_real(file, line)
        fifo_file.flush()
    except:
        # Ignore errors
        pass

def trace_lines(frame, event, arg):
    if event != 'line':
        return
    co = frame.f_code
    func_name = co.co_name
    line_no = frame.f_lineno
    filename = co.co_filename
    report_trace(filename, line_no)

def trace_calls(frame, event, arg):
    if event != 'call':
        return
    co = frame.f_code
    func_name = co.co_name
    line_no = frame.f_lineno
    filename = co.co_filename
    report_trace(filename, line_no)
    return trace_lines

def runctx(cmd, globals):
    sys.settrace(trace_calls)
    try:
        exec(cmd, globals)
    finally:
        sys.settrace(None)

if __name__ == "__main__":
    if sys.version_info >= (3, 0):
        report_trace_real = report_trace3
    else:
        report_trace_real = report_trace2

    prog_argv = sys.argv[1:]

    sys.argv = prog_argv
    progname = prog_argv[0]
    sys.path[0] = os.path.split(progname)[0]

    fifo_path = os.getenv("KCOV_PYTHON_PIPE_PATH")
    if fifo_path == None:
        sys.stderr.write("the KCOV_PYTHON_PIPE_PATH environment variable is not set")
        sys.exit(127)
    try:
        fifo_file = open(fifo_path, "wb")
    except:
        sys.stderr.write("Can't open fifo file")
        sys.exit(127)

    main_mod = imp.new_module('__main__')
    old_main_mod = sys.modules['__main__']
    sys.modules['__main__'] = main_mod
    main_mod.__file__ = progname
    main_mod.__package__ = None
    main_mod.__builtins__ = BUILTINS

    try:
        with open(progname) as fp:
            # try to emulate __main__ namespace as much as possible

            code = compile(fp.read(), progname, 'exec')

            runctx(code, main_mod.__dict__)
    except IOError:
        sys.stderr.write("Cannot run file %r" % (sys.argv[0]))
        sys.exit(127)
    finally:
        sys.modules['__main__'] = old_main_mod
