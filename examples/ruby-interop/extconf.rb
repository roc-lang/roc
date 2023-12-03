#!/usr/bin/env ruby
require 'mkmf'

# preparation for compilation goes here
dir_config('') # include the current directory in the library search path
have_library('hello') # depend on `libhello.dylib` being in the current path
                      # (.dylib is macOS-specific; other OSes would have different extensions)

create_header
create_makefile 'demo'

