set pagination off
set debuginfod enabled off
break dev_wrappers.roc_builtins_box_decref_with
commands
  silent
  printf "box_drop regs rdi=%p rsi=%#lx rdx=%p rcx=%p\n", $rdi, $rsi, $rdx, $rcx
  continue
end
break utils.decrefDataPtrC
commands
  silent
  printf "decref_data regs rdi=%p rsi=%#lx rdx=%#lx rcx=%p\n", $rdi, $rsi, $rdx, $rcx
  continue
end
run
