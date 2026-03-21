set pagination off
set debuginfod enabled off
break *0x385342
commands
  silent
  printf "\nstr_incref helper slot=%p data=%p len=%#lx cap=%#lx caller=%p\n", $rdi, *(void**)$rdi, *((unsigned long*)$rdi + 1), *((unsigned long*)$rdi + 2), *(void**)$rsp
  continue
end
break *0x385407
commands
  silent
  printf "\nstr_decref helper slot=%p data=%p len=%#lx cap=%#lx caller=%p\n", $rdi, *(void**)$rdi, *((unsigned long*)$rdi + 1), *((unsigned long*)$rdi + 2), *(void**)$rsp
  continue
end
run
