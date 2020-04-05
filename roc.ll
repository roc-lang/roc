; ModuleID = 'app'
source_filename = "roc_builtins_bitcode.d1f89fme-cgu.0"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: norecurse nounwind nonlazybind readnone uwtable
define double @i64_to_f64_(i64 %num) unnamed_addr #0 {
start:
  %0 = sitofp i64 %num to double
  ret double %0
}

define { double, double, double } @"$Test.main"() {
entry:
  ret { double, double, double } { double 3.100000e+00, double 5.100000e+00, double 1.710000e+01 }
}

attributes #0 = { norecurse nounwind nonlazybind readnone uwtable "probe-stack"="__rust_probestack" "target-cpu"="skylake" }

!llvm.module.flags = !{!0, !1}

!0 = !{i32 7, !"PIC Level", i32 2}
!1 = !{i32 2, !"RtLibUseGOT", i32 1}
