#!/usr/bin/env ruby
require 'mkmf'

# preparation for compilation goes here
# HACK: pass 'demo.c' as a header because otherwise ruby complains that roc_*alloc are not defined
have_library('hello', nil, 'demo.c')

create_header
create_makefile 'demo'

