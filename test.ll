; ModuleID = 'test.c'
source_filename = "test.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.triple = type { double, double, double }

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @f(%struct.triple* noalias sret) #0 {
  %2 = getelementptr inbounds %struct.triple, %struct.triple* %0, i32 0, i32 0
  store double 1.100000e+00, double* %2, align 8
  %3 = getelementptr inbounds %struct.triple, %struct.triple* %0, i32 0, i32 1
  store double 2.200000e+00, double* %3, align 8
  %4 = getelementptr inbounds %struct.triple, %struct.triple* %0, i32 0, i32 2
  store double 3.300000e+00, double* %4, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main() #0 {
  %1 = alloca i32, align 4
  %2 = alloca %struct.triple, align 8
  store i32 0, i32* %1, align 4
  call void @f(%struct.triple* sret %2)
  ret i32 0
}

attributes #0 = { noinline nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 8.0.1-3build1 (tags/RELEASE_801/final)"}
