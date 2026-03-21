set pagination off
set debuginfod enabled off
set follow-fork-mode child
set detach-on-fork off
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
run tmp_box_list_app.roc --opt=dev
