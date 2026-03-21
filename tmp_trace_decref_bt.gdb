set pagination off
set debuginfod enabled off
break utils.decrefDataPtrC
commands
  silent
  printf "\ndecref_data regs rdi=%p rsi=%#lx rdx=%#lx rcx=%p\n", $rdi, $rsi, $rdx, $rcx
  bt 6
  continue
end
run
