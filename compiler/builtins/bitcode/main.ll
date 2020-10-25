; ModuleID = 'main'
source_filename = "main"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-macos-gnu"

%std.macho.mach_header_64 = type { i32, i32, i32, i32, i32, i32, i32, i32 }

@_mh_execute_header = weak_odr dso_local unnamed_addr global %std.macho.mach_header_64 undef, align 4
@0 = internal unnamed_addr constant [4 x double] [double 0x3FDDAC670561BB4F, double 0x3FE921FB54442D18, double 0x3FEF730BD281F69B, double 0x3FF921FB54442D18], align 8
@1 = internal unnamed_addr constant [4 x double] [double 0x3C7A2B7F222F65E2, double 0x3C81A62633145C07, double 0x3C7007887AF0CBBD, double 0x3C91A62633145C07], align 8

; Function Attrs: nobuiltin nounwind
define double @atan_(double %0) local_unnamed_addr #0 {
Entry:
  %x.sroa.0.i.i.i = alloca i32, align 4
  %1 = bitcast double %0 to i64
  %2 = lshr i64 %1, 32
  %3 = trunc i64 %2 to i32
  %4 = and i32 %3, 2147483647
  %5 = icmp ugt i32 %4, 1141899263
  br i1 %5, label %Then.i.i, label %EndIf5.i.i

Then.i.i:                                         ; preds = %Entry
  %6 = fcmp uno double %0, 0.000000e+00
  br i1 %6, label %std.math.atan.atan.exit, label %Else.i.i

Else.i.i:                                         ; preds = %Then.i.i
  %..i.i = tail call double @llvm.copysign.f64(double 0x3FF921FB54442D18, double %0) #5
  br label %std.math.atan.atan.exit

EndIf5.i.i:                                       ; preds = %Entry
  %7 = icmp ult i32 %4, 1039925248
  br i1 %7, label %Then7.i.i, label %Else12.i.i

Then7.i.i:                                        ; preds = %EndIf5.i.i
  %8 = icmp ult i32 %4, 1048576
  br i1 %8, label %Then8.i.i, label %std.math.atan.atan.exit

Then8.i.i:                                        ; preds = %Then7.i.i
  %x.sroa.0.i.i.i.0.sroa_cast = bitcast i32* %x.sroa.0.i.i.i to i8*
  call void @llvm.lifetime.start.p0i8(i64 4, i8* nonnull %x.sroa.0.i.i.i.0.sroa_cast)
  store volatile i32 undef, i32* %x.sroa.0.i.i.i, align 4
  call void @llvm.lifetime.end.p0i8(i64 4, i8* nonnull %x.sroa.0.i.i.i.0.sroa_cast)
  br label %std.math.atan.atan.exit

Else12.i.i:                                       ; preds = %EndIf5.i.i
  %9 = and i64 %1, 9223372036854775807
  %10 = bitcast i64 %9 to double
  %11 = icmp ult i32 %4, 1072889856
  br i1 %11, label %Then13.i.i, label %Else16.i.i

Then13.i.i:                                       ; preds = %Else12.i.i
  %12 = icmp ult i32 %4, 1072037888
  br i1 %12, label %Then14.i.i, label %Else15.i.i

Then14.i.i:                                       ; preds = %Then13.i.i
  %13 = fmul double %10, 2.000000e+00
  %14 = fadd double %13, -1.000000e+00
  %15 = fadd double %10, 2.000000e+00
  %16 = fdiv double %14, %15
  br label %EndIf23.i.i

Else15.i.i:                                       ; preds = %Then13.i.i
  %17 = fadd double %10, -1.000000e+00
  %18 = fadd double %10, 1.000000e+00
  %19 = fdiv double %17, %18
  br label %EndIf23.i.i

Else16.i.i:                                       ; preds = %Else12.i.i
  %20 = icmp ult i32 %4, 1073971200
  br i1 %20, label %Then17.i.i, label %Else18.i.i

Then17.i.i:                                       ; preds = %Else16.i.i
  %21 = fadd double %10, -1.500000e+00
  %22 = fmul double %10, 1.500000e+00
  %23 = fadd double %22, 1.000000e+00
  %24 = fdiv double %21, %23
  br label %EndIf23.i.i

Else18.i.i:                                       ; preds = %Else16.i.i
  %25 = fdiv double -1.000000e+00, %10
  br label %EndIf23.i.i

EndIf23.i.i:                                      ; preds = %Else18.i.i, %Then17.i.i, %Else15.i.i, %Then14.i.i
  %id.sroa.0.0.i.i = phi i64 [ 0, %Then14.i.i ], [ 1, %Else15.i.i ], [ 2, %Then17.i.i ], [ 3, %Else18.i.i ]
  %x.0.i.i = phi double [ %16, %Then14.i.i ], [ %19, %Else15.i.i ], [ %24, %Then17.i.i ], [ %25, %Else18.i.i ]
  %26 = fmul double %x.0.i.i, %x.0.i.i
  %27 = fmul double %26, %26
  %28 = fmul double %27, 0x3FA2B4442C6A6C2F
  %29 = fsub double 0xBFADDE2D52DEFD9A, %28
  %30 = fmul double %27, %29
  %31 = fadd double %30, 0xBFB3B0F2AF749A6D
  %32 = fmul double %27, %31
  %33 = fadd double %32, 0xBFBC71C6FE231671
  %34 = fmul double %27, %33
  %35 = fadd double %34, 0xBFC999999998EBC4
  %36 = fmul double %27, %35
  %37 = fmul double %27, 0x3F90AD3AE322DA11
  %38 = fadd double %37, 0x3FA97B4B24760DEB
  %39 = fmul double %27, %38
  %40 = fadd double %39, 0x3FB10D66A0D03D51
  %41 = fmul double %27, %40
  %42 = fadd double %41, 0x3FB745CDC54C206E
  %43 = fmul double %27, %42
  %44 = fadd double %43, 0x3FC24924920083FF
  %45 = fmul double %27, %44
  %46 = fadd double %45, 0x3FD555555555550D
  %47 = fmul double %26, %46
  %48 = getelementptr inbounds [4 x double], [4 x double]* @0, i64 0, i64 %id.sroa.0.0.i.i
  %49 = load double, double* %48, align 8
  %50 = fadd double %36, %47
  %51 = fmul double %x.0.i.i, %50
  %52 = getelementptr inbounds [4 x double], [4 x double]* @1, i64 0, i64 %id.sroa.0.0.i.i
  %53 = load double, double* %52, align 8
  %54 = fsub double %51, %53
  %55 = fsub double %54, %x.0.i.i
  %56 = fsub double %49, %55
  %.not.i.i = icmp sgt i64 %1, -1
  %57 = fneg double %56
  %result.1.i.i = select i1 %.not.i.i, double %56, double %57
  br label %std.math.atan.atan.exit

std.math.atan.atan.exit:                          ; preds = %Then.i.i, %Else.i.i, %Then7.i.i, %Then8.i.i, %EndIf23.i.i
  %merge.i.i = phi double [ %0, %Then.i.i ], [ %..i.i, %Else.i.i ], [ %0, %Then7.i.i ], [ %0, %Then8.i.i ], [ %result.1.i.i, %EndIf23.i.i ]
  ret double %merge.i.i
}

; Function Attrs: nobuiltin norecurse nounwind readnone
define i1 @is_finite_(double %0) local_unnamed_addr #1 {
Entry:
  %1 = bitcast double %0 to i64
  %2 = and i64 %1, 9218868437227405312
  %3 = icmp ne i64 %2, 9218868437227405312
  ret i1 %3
}

; Function Attrs: nobuiltin nounwind readnone
define i64 @pow_int_(i64 %0, i64 %1) local_unnamed_addr #2 {
Entry:
  %2 = icmp eq i64 %1, 0
  br i1 %2, label %std.math.pow.pow.exit, label %EndIf.i.i

EndIf.i.i:                                        ; preds = %Entry
  switch i64 %0, label %EndIf5.i.i [
    i64 0, label %std.math.pow.pow.exit
    i64 1, label %SwitchProng35.i.i
    i64 -1, label %Then1.i.i
  ]

Then1.i.i:                                        ; preds = %EndIf.i.i
  %3 = srem i64 %1, 2
  %4 = add nsw i64 %3, 2
  %5 = srem i64 %4, 2
  %6 = icmp slt i64 %1, 0
  %7 = select i1 %6, i64 %5, i64 %3
  %8 = icmp eq i64 %7, 0
  %spec.select.i = select i1 %8, i64 1, i64 -1
  br label %std.math.pow.pow.exit

EndIf5.i.i:                                       ; preds = %EndIf.i.i
  %9 = icmp sgt i64 %0, 0
  %10 = icmp sgt i64 %1, 62
  %11 = and i1 %9, %10
  br i1 %11, label %std.math.pow.pow.exit, label %Else7.i.i

Else7.i.i:                                        ; preds = %EndIf5.i.i
  %12 = icmp slt i64 %0, 0
  %13 = icmp sgt i64 %1, 63
  %14 = and i1 %12, %13
  br i1 %14, label %std.math.pow.pow.exit, label %WhileCond.i.i

WhileCond.i.i:                                    ; preds = %Else7.i.i, %EndIf21.i.i
  %acc.0.i.i = phi i64 [ %acc.1.i.i, %EndIf21.i.i ], [ 1, %Else7.i.i ]
  %exp.0.i.i = phi i64 [ %20, %EndIf21.i.i ], [ %1, %Else7.i.i ]
  %base.0.i.i = phi i64 [ %22, %EndIf21.i.i ], [ %0, %Else7.i.i ]
  %15 = icmp sgt i64 %exp.0.i.i, 1
  br i1 %15, label %WhileBody.i.i, label %WhileEnd.i.i

WhileBody.i.i:                                    ; preds = %WhileCond.i.i
  %16 = and i64 %exp.0.i.i, 1
  %.not.i.i = icmp eq i64 %16, 0
  br i1 %.not.i.i, label %EndIf21.i.i, label %Then14.i.i

Then14.i.i:                                       ; preds = %WhileBody.i.i
  %17 = tail call { i64, i1 } @llvm.smul.with.overflow.i64(i64 %acc.0.i.i, i64 %base.0.i.i) #5
  %18 = extractvalue { i64, i1 } %17, 0
  %19 = extractvalue { i64, i1 } %17, 1
  br i1 %19, label %std.math.pow.pow.exit, label %EndIf21.i.i

EndIf21.i.i:                                      ; preds = %Then14.i.i, %WhileBody.i.i
  %acc.1.i.i = phi i64 [ %18, %Then14.i.i ], [ %acc.0.i.i, %WhileBody.i.i ]
  %20 = lshr i64 %exp.0.i.i, 1
  %21 = tail call { i64, i1 } @llvm.smul.with.overflow.i64(i64 %base.0.i.i, i64 %base.0.i.i) #5
  %22 = extractvalue { i64, i1 } %21, 0
  %23 = extractvalue { i64, i1 } %21, 1
  br i1 %23, label %std.math.pow.pow.exit, label %WhileCond.i.i

WhileEnd.i.i:                                     ; preds = %WhileCond.i.i
  %24 = icmp eq i64 %exp.0.i.i, 1
  %25 = select i1 %24, i64 %base.0.i.i, i64 1
  %spec.select2.i = mul i64 %25, %acc.0.i.i
  br label %std.math.pow.pow.exit

SwitchProng35.i.i:                                ; preds = %EndIf.i.i
  br label %std.math.pow.pow.exit

std.math.pow.pow.exit:                            ; preds = %Then14.i.i, %EndIf21.i.i, %Entry, %EndIf.i.i, %EndIf5.i.i, %Else7.i.i, %SwitchProng35.i.i, %WhileEnd.i.i, %Then1.i.i
  %26 = phi i64 [ %spec.select.i, %Then1.i.i ], [ %spec.select2.i, %WhileEnd.i.i ], [ 1, %SwitchProng35.i.i ], [ 1, %Entry ], [ undef, %EndIf5.i.i ], [ undef, %Else7.i.i ], [ %0, %EndIf.i.i ], [ undef, %EndIf21.i.i ], [ undef, %Then14.i.i ]
  ret i64 %26
}

; Function Attrs: nounwind readnone speculatable willreturn
declare { i64, i1 } @llvm.smul.with.overflow.i64(i64, i64) #3

; Function Attrs: argmemonly nounwind willreturn
declare void @llvm.lifetime.start.p0i8(i64 immarg, i8* nocapture) #4

; Function Attrs: argmemonly nounwind willreturn
declare void @llvm.lifetime.end.p0i8(i64 immarg, i8* nocapture) #4

; Function Attrs: nounwind readnone speculatable willreturn
declare double @llvm.copysign.f64(double, double) #3

attributes #0 = { nobuiltin nounwind }
attributes #1 = { nobuiltin norecurse nounwind readnone }
attributes #2 = { nobuiltin nounwind readnone }
attributes #3 = { nounwind readnone speculatable willreturn }
attributes #4 = { argmemonly nounwind willreturn }
attributes #5 = { nounwind }

!llvm.module.flags = !{!0, !1}
!llvm.dbg.cu = !{!2}

!0 = !{i32 2, !"Debug Info Version", i32 3}
!1 = !{i32 2, !"Dwarf Version", i32 4}
!2 = distinct !DICompileUnit(language: DW_LANG_C99, file: !3, producer: "zig 0.6.0", isOptimized: true, runtimeVersion: 0, emissionKind: NoDebug, enums: !4)
!3 = !DIFile(filename: "main", directory: ".")
!4 = !{!5, !12, !53, !73, !79, !133, !156, !162, !181, !319, !325}
!5 = !DICompositeType(tag: DW_TAG_enumeration_type, name: "std.builtin.OutputMode", scope: !6, file: !6, line: 441, baseType: !7, size: 8, align: 8, elements: !8)
!6 = !DIFile(filename: "builtin.zig", directory: "/nix/store/7k1ipq7x4b61jfhc3kwvm7zqfm5jzj2y-zig-0.6.0/lib/zig/std")
!7 = !DIBasicType(name: "u2", size: 8, encoding: DW_ATE_unsigned)
!8 = !{!9, !10, !11}
!9 = !DIEnumerator(name: "Exe", value: 0)
!10 = !DIEnumerator(name: "Lib", value: 1)
!11 = !DIEnumerator(name: "Obj", value: 2)
!12 = !DICompositeType(tag: DW_TAG_enumeration_type, name: "std.target.Tag", scope: !13, file: !13, line: 23, baseType: !14, size: 8, align: 8, elements: !15)
!13 = !DIFile(filename: "target.zig", directory: "/nix/store/7k1ipq7x4b61jfhc3kwvm7zqfm5jzj2y-zig-0.6.0/lib/zig/std")
!14 = !DIBasicType(name: "u6", size: 8, encoding: DW_ATE_unsigned)
!15 = !{!16, !17, !18, !19, !20, !21, !22, !23, !24, !25, !26, !27, !28, !29, !30, !31, !32, !33, !34, !35, !36, !37, !38, !39, !40, !41, !42, !43, !44, !45, !46, !47, !48, !49, !50, !51, !52}
!16 = !DIEnumerator(name: "freestanding", value: 0)
!17 = !DIEnumerator(name: "ananas", value: 1)
!18 = !DIEnumerator(name: "cloudabi", value: 2)
!19 = !DIEnumerator(name: "dragonfly", value: 3)
!20 = !DIEnumerator(name: "freebsd", value: 4)
!21 = !DIEnumerator(name: "fuchsia", value: 5)
!22 = !DIEnumerator(name: "ios", value: 6)
!23 = !DIEnumerator(name: "kfreebsd", value: 7)
!24 = !DIEnumerator(name: "linux", value: 8)
!25 = !DIEnumerator(name: "lv2", value: 9)
!26 = !DIEnumerator(name: "macos", value: 10)
!27 = !DIEnumerator(name: "netbsd", value: 11)
!28 = !DIEnumerator(name: "openbsd", value: 12)
!29 = !DIEnumerator(name: "solaris", value: 13)
!30 = !DIEnumerator(name: "windows", value: 14)
!31 = !DIEnumerator(name: "haiku", value: 15)
!32 = !DIEnumerator(name: "minix", value: 16)
!33 = !DIEnumerator(name: "rtems", value: 17)
!34 = !DIEnumerator(name: "nacl", value: 18)
!35 = !DIEnumerator(name: "cnk", value: 19)
!36 = !DIEnumerator(name: "aix", value: 20)
!37 = !DIEnumerator(name: "cuda", value: 21)
!38 = !DIEnumerator(name: "nvcl", value: 22)
!39 = !DIEnumerator(name: "amdhsa", value: 23)
!40 = !DIEnumerator(name: "ps4", value: 24)
!41 = !DIEnumerator(name: "elfiamcu", value: 25)
!42 = !DIEnumerator(name: "tvos", value: 26)
!43 = !DIEnumerator(name: "watchos", value: 27)
!44 = !DIEnumerator(name: "mesa3d", value: 28)
!45 = !DIEnumerator(name: "contiki", value: 29)
!46 = !DIEnumerator(name: "amdpal", value: 30)
!47 = !DIEnumerator(name: "hermit", value: 31)
!48 = !DIEnumerator(name: "hurd", value: 32)
!49 = !DIEnumerator(name: "wasi", value: 33)
!50 = !DIEnumerator(name: "emscripten", value: 34)
!51 = !DIEnumerator(name: "uefi", value: 35)
!52 = !DIEnumerator(name: "other", value: 36)
!53 = !DICompositeType(tag: DW_TAG_enumeration_type, name: "std.target.WindowsVersion", scope: !13, file: !13, line: 89, baseType: !54, size: 32, align: 32, elements: !55)
!54 = !DIBasicType(name: "u32", size: 32, encoding: DW_ATE_unsigned)
!55 = !{!56, !57, !58, !59, !60, !61, !62, !63, !64, !65, !66, !67, !68, !69, !70, !71, !72}
!56 = !DIEnumerator(name: "nt4", value: 67108864)
!57 = !DIEnumerator(name: "win2k", value: 83886080)
!58 = !DIEnumerator(name: "xp", value: 83951616)
!59 = !DIEnumerator(name: "ws2003", value: 84017152)
!60 = !DIEnumerator(name: "vista", value: 100663296)
!61 = !DIEnumerator(name: "win7", value: 100728832)
!62 = !DIEnumerator(name: "win8", value: 100794368)
!63 = !DIEnumerator(name: "win8_1", value: 100859904)
!64 = !DIEnumerator(name: "win10", value: 167772160)
!65 = !DIEnumerator(name: "win10_th2", value: 167772161)
!66 = !DIEnumerator(name: "win10_rs1", value: 167772162)
!67 = !DIEnumerator(name: "win10_rs2", value: 167772163)
!68 = !DIEnumerator(name: "win10_rs3", value: 167772164)
!69 = !DIEnumerator(name: "win10_rs4", value: 167772165)
!70 = !DIEnumerator(name: "win10_rs5", value: 167772166)
!71 = !DIEnumerator(name: "win10_19h1", value: 167772167)
!72 = !DIEnumerator(name: "win10_20h1", value: 167772168)
!73 = !DICompositeType(tag: DW_TAG_enumeration_type, name: "std.builtin.Mode", scope: !6, file: !6, line: 142, baseType: !7, size: 8, align: 8, elements: !74)
!74 = !{!75, !76, !77, !78}
!75 = !DIEnumerator(name: "Debug", value: 0)
!76 = !DIEnumerator(name: "ReleaseSafe", value: 1)
!77 = !DIEnumerator(name: "ReleaseFast", value: 2)
!78 = !DIEnumerator(name: "ReleaseSmall", value: 3)
!79 = !DICompositeType(tag: DW_TAG_enumeration_type, name: "std.target.Arch", scope: !13, file: !13, line: 666, baseType: !14, size: 8, align: 8, elements: !80)
!80 = !{!81, !82, !83, !84, !85, !86, !87, !88, !89, !90, !91, !92, !93, !94, !95, !96, !97, !98, !99, !100, !101, !102, !103, !104, !105, !106, !107, !108, !109, !110, !111, !112, !113, !114, !115, !116, !117, !118, !119, !120, !121, !122, !123, !124, !125, !126, !127, !128, !129, !130, !131, !132}
!81 = !DIEnumerator(name: "arm", value: 0)
!82 = !DIEnumerator(name: "armeb", value: 1)
!83 = !DIEnumerator(name: "aarch64", value: 2)
!84 = !DIEnumerator(name: "aarch64_be", value: 3)
!85 = !DIEnumerator(name: "aarch64_32", value: 4)
!86 = !DIEnumerator(name: "arc", value: 5)
!87 = !DIEnumerator(name: "avr", value: 6)
!88 = !DIEnumerator(name: "bpfel", value: 7)
!89 = !DIEnumerator(name: "bpfeb", value: 8)
!90 = !DIEnumerator(name: "hexagon", value: 9)
!91 = !DIEnumerator(name: "mips", value: 10)
!92 = !DIEnumerator(name: "mipsel", value: 11)
!93 = !DIEnumerator(name: "mips64", value: 12)
!94 = !DIEnumerator(name: "mips64el", value: 13)
!95 = !DIEnumerator(name: "msp430", value: 14)
!96 = !DIEnumerator(name: "powerpc", value: 15)
!97 = !DIEnumerator(name: "powerpc64", value: 16)
!98 = !DIEnumerator(name: "powerpc64le", value: 17)
!99 = !DIEnumerator(name: "r600", value: 18)
!100 = !DIEnumerator(name: "amdgcn", value: 19)
!101 = !DIEnumerator(name: "riscv32", value: 20)
!102 = !DIEnumerator(name: "riscv64", value: 21)
!103 = !DIEnumerator(name: "sparc", value: 22)
!104 = !DIEnumerator(name: "sparcv9", value: 23)
!105 = !DIEnumerator(name: "sparcel", value: 24)
!106 = !DIEnumerator(name: "s390x", value: 25)
!107 = !DIEnumerator(name: "tce", value: 26)
!108 = !DIEnumerator(name: "tcele", value: 27)
!109 = !DIEnumerator(name: "thumb", value: 28)
!110 = !DIEnumerator(name: "thumbeb", value: 29)
!111 = !DIEnumerator(name: "i386", value: 30)
!112 = !DIEnumerator(name: "x86_64", value: 31)
!113 = !DIEnumerator(name: "xcore", value: 32)
!114 = !DIEnumerator(name: "nvptx", value: 33)
!115 = !DIEnumerator(name: "nvptx64", value: 34)
!116 = !DIEnumerator(name: "le32", value: 35)
!117 = !DIEnumerator(name: "le64", value: 36)
!118 = !DIEnumerator(name: "amdil", value: 37)
!119 = !DIEnumerator(name: "amdil64", value: 38)
!120 = !DIEnumerator(name: "hsail", value: 39)
!121 = !DIEnumerator(name: "hsail64", value: 40)
!122 = !DIEnumerator(name: "spir", value: 41)
!123 = !DIEnumerator(name: "spir64", value: 42)
!124 = !DIEnumerator(name: "kalimba", value: 43)
!125 = !DIEnumerator(name: "shave", value: 44)
!126 = !DIEnumerator(name: "lanai", value: 45)
!127 = !DIEnumerator(name: "wasm32", value: 46)
!128 = !DIEnumerator(name: "wasm64", value: 47)
!129 = !DIEnumerator(name: "renderscript32", value: 48)
!130 = !DIEnumerator(name: "renderscript64", value: 49)
!131 = !DIEnumerator(name: "ve", value: 50)
!132 = !DIEnumerator(name: "spu_2", value: 51)
!133 = !DICompositeType(tag: DW_TAG_enumeration_type, name: "std.target.Abi", scope: !13, file: !13, line: 402, baseType: !134, size: 8, align: 8, elements: !135)
!134 = !DIBasicType(name: "u5", size: 8, encoding: DW_ATE_unsigned)
!135 = !{!136, !137, !138, !139, !140, !141, !142, !143, !144, !145, !146, !147, !148, !149, !150, !151, !152, !153, !154, !155}
!136 = !DIEnumerator(name: "none", value: 0)
!137 = !DIEnumerator(name: "gnu", value: 1)
!138 = !DIEnumerator(name: "gnuabin32", value: 2)
!139 = !DIEnumerator(name: "gnuabi64", value: 3)
!140 = !DIEnumerator(name: "gnueabi", value: 4)
!141 = !DIEnumerator(name: "gnueabihf", value: 5)
!142 = !DIEnumerator(name: "gnux32", value: 6)
!143 = !DIEnumerator(name: "code16", value: 7)
!144 = !DIEnumerator(name: "eabi", value: 8)
!145 = !DIEnumerator(name: "eabihf", value: 9)
!146 = !DIEnumerator(name: "android", value: 10)
!147 = !DIEnumerator(name: "musl", value: 11)
!148 = !DIEnumerator(name: "musleabi", value: 12)
!149 = !DIEnumerator(name: "musleabihf", value: 13)
!150 = !DIEnumerator(name: "msvc", value: 14)
!151 = !DIEnumerator(name: "itanium", value: 15)
!152 = !DIEnumerator(name: "cygnus", value: 16)
!153 = !DIEnumerator(name: "coreclr", value: 17)
!154 = !DIEnumerator(name: "simulator", value: 18)
!155 = !DIEnumerator(name: "macabi", value: 19)
!156 = !DICompositeType(tag: DW_TAG_enumeration_type, name: "std.io.Mode", scope: !157, file: !157, line: 20, baseType: !158, size: 8, align: 8, elements: !159)
!157 = !DIFile(filename: "io.zig", directory: "/nix/store/7k1ipq7x4b61jfhc3kwvm7zqfm5jzj2y-zig-0.6.0/lib/zig/std")
!158 = !DIBasicType(name: "u1", size: 8, encoding: DW_ATE_unsigned)
!159 = !{!160, !161}
!160 = !DIEnumerator(name: "blocking", value: 0)
!161 = !DIEnumerator(name: "evented", value: 1)
!162 = !DICompositeType(tag: DW_TAG_enumeration_type, name: "anyerror", baseType: !163, size: 16, align: 16, elements: !164)
!163 = !DIBasicType(name: "u16", size: 16, encoding: DW_ATE_unsigned)
!164 = !{!165, !166, !167, !168, !169, !170, !171, !172, !173, !174, !175, !176, !177, !178, !179, !180}
!165 = !DIEnumerator(name: "(none)", value: 0)
!166 = !DIEnumerator(name: "DiskQuota", value: 1)
!167 = !DIEnumerator(name: "FileTooBig", value: 2)
!168 = !DIEnumerator(name: "InputOutput", value: 3)
!169 = !DIEnumerator(name: "NoSpaceLeft", value: 4)
!170 = !DIEnumerator(name: "AccessDenied", value: 5)
!171 = !DIEnumerator(name: "BrokenPipe", value: 6)
!172 = !DIEnumerator(name: "SystemResources", value: 7)
!173 = !DIEnumerator(name: "OperationAborted", value: 8)
!174 = !DIEnumerator(name: "NotOpenForWriting", value: 9)
!175 = !DIEnumerator(name: "WouldBlock", value: 10)
!176 = !DIEnumerator(name: "Unexpected", value: 11)
!177 = !DIEnumerator(name: "Overflow", value: 12)
!178 = !DIEnumerator(name: "Underflow", value: 13)
!179 = !DIEnumerator(name: "TimedOut", value: 14)
!180 = !DIEnumerator(name: "SystemCannotYield", value: 15)
!181 = !DICompositeType(tag: DW_TAG_enumeration_type, name: "std.target.x86.Feature", scope: !182, file: !182, line: 10, baseType: !183, size: 8, align: 8, elements: !184)
!182 = !DIFile(filename: "x86.zig", directory: "/nix/store/7k1ipq7x4b61jfhc3kwvm7zqfm5jzj2y-zig-0.6.0/lib/zig/std/target")
!183 = !DIBasicType(name: "u8", size: 8, encoding: DW_ATE_unsigned_char)
!184 = !{!185, !186, !187, !188, !189, !190, !191, !192, !193, !194, !195, !196, !197, !198, !199, !200, !201, !202, !203, !204, !205, !206, !207, !208, !209, !210, !211, !212, !213, !214, !215, !216, !217, !218, !219, !220, !221, !222, !223, !224, !225, !226, !227, !228, !229, !230, !231, !232, !233, !234, !235, !236, !237, !238, !239, !240, !241, !242, !243, !244, !245, !246, !247, !248, !249, !250, !251, !252, !253, !254, !255, !256, !257, !258, !259, !260, !261, !262, !263, !264, !265, !266, !267, !268, !269, !270, !271, !272, !273, !274, !275, !276, !277, !278, !279, !280, !281, !282, !283, !284, !285, !286, !287, !288, !289, !290, !291, !292, !293, !294, !295, !296, !297, !298, !299, !300, !301, !302, !303, !304, !305, !306, !307, !308, !309, !310, !311, !312, !313, !314, !315, !316, !317, !318}
!185 = !DIEnumerator(name: "3dnow", value: 0)
!186 = !DIEnumerator(name: "3dnowa", value: 1)
!187 = !DIEnumerator(name: "64bit", value: 2)
!188 = !DIEnumerator(name: "adx", value: 3)
!189 = !DIEnumerator(name: "aes", value: 4)
!190 = !DIEnumerator(name: "amx_bf16", value: 5)
!191 = !DIEnumerator(name: "amx_int8", value: 6)
!192 = !DIEnumerator(name: "amx_tile", value: 7)
!193 = !DIEnumerator(name: "avx", value: 8)
!194 = !DIEnumerator(name: "avx2", value: 9)
!195 = !DIEnumerator(name: "avx512bf16", value: 10)
!196 = !DIEnumerator(name: "avx512bitalg", value: 11)
!197 = !DIEnumerator(name: "avx512bw", value: 12)
!198 = !DIEnumerator(name: "avx512cd", value: 13)
!199 = !DIEnumerator(name: "avx512dq", value: 14)
!200 = !DIEnumerator(name: "avx512er", value: 15)
!201 = !DIEnumerator(name: "avx512f", value: 16)
!202 = !DIEnumerator(name: "avx512ifma", value: 17)
!203 = !DIEnumerator(name: "avx512pf", value: 18)
!204 = !DIEnumerator(name: "avx512vbmi", value: 19)
!205 = !DIEnumerator(name: "avx512vbmi2", value: 20)
!206 = !DIEnumerator(name: "avx512vl", value: 21)
!207 = !DIEnumerator(name: "avx512vnni", value: 22)
!208 = !DIEnumerator(name: "avx512vp2intersect", value: 23)
!209 = !DIEnumerator(name: "avx512vpopcntdq", value: 24)
!210 = !DIEnumerator(name: "bmi", value: 25)
!211 = !DIEnumerator(name: "bmi2", value: 26)
!212 = !DIEnumerator(name: "branchfusion", value: 27)
!213 = !DIEnumerator(name: "cldemote", value: 28)
!214 = !DIEnumerator(name: "clflushopt", value: 29)
!215 = !DIEnumerator(name: "clwb", value: 30)
!216 = !DIEnumerator(name: "clzero", value: 31)
!217 = !DIEnumerator(name: "cmov", value: 32)
!218 = !DIEnumerator(name: "cx16", value: 33)
!219 = !DIEnumerator(name: "cx8", value: 34)
!220 = !DIEnumerator(name: "enqcmd", value: 35)
!221 = !DIEnumerator(name: "ermsb", value: 36)
!222 = !DIEnumerator(name: "f16c", value: 37)
!223 = !DIEnumerator(name: "false_deps_lzcnt_tzcnt", value: 38)
!224 = !DIEnumerator(name: "false_deps_popcnt", value: 39)
!225 = !DIEnumerator(name: "fast_11bytenop", value: 40)
!226 = !DIEnumerator(name: "fast_15bytenop", value: 41)
!227 = !DIEnumerator(name: "fast_7bytenop", value: 42)
!228 = !DIEnumerator(name: "fast_bextr", value: 43)
!229 = !DIEnumerator(name: "fast_gather", value: 44)
!230 = !DIEnumerator(name: "fast_hops", value: 45)
!231 = !DIEnumerator(name: "fast_lzcnt", value: 46)
!232 = !DIEnumerator(name: "fast_scalar_fsqrt", value: 47)
!233 = !DIEnumerator(name: "fast_scalar_shift_masks", value: 48)
!234 = !DIEnumerator(name: "fast_shld_rotate", value: 49)
!235 = !DIEnumerator(name: "fast_variable_shuffle", value: 50)
!236 = !DIEnumerator(name: "fast_vector_fsqrt", value: 51)
!237 = !DIEnumerator(name: "fast_vector_shift_masks", value: 52)
!238 = !DIEnumerator(name: "fma", value: 53)
!239 = !DIEnumerator(name: "fma4", value: 54)
!240 = !DIEnumerator(name: "fsgsbase", value: 55)
!241 = !DIEnumerator(name: "fxsr", value: 56)
!242 = !DIEnumerator(name: "gfni", value: 57)
!243 = !DIEnumerator(name: "idivl_to_divb", value: 58)
!244 = !DIEnumerator(name: "idivq_to_divl", value: 59)
!245 = !DIEnumerator(name: "invpcid", value: 60)
!246 = !DIEnumerator(name: "lea_sp", value: 61)
!247 = !DIEnumerator(name: "lea_uses_ag", value: 62)
!248 = !DIEnumerator(name: "lvi_cfi", value: 63)
!249 = !DIEnumerator(name: "lvi_load_hardening", value: 64)
!250 = !DIEnumerator(name: "lwp", value: 65)
!251 = !DIEnumerator(name: "lzcnt", value: 66)
!252 = !DIEnumerator(name: "macrofusion", value: 67)
!253 = !DIEnumerator(name: "merge_to_threeway_branch", value: 68)
!254 = !DIEnumerator(name: "mmx", value: 69)
!255 = !DIEnumerator(name: "movbe", value: 70)
!256 = !DIEnumerator(name: "movdir64b", value: 71)
!257 = !DIEnumerator(name: "movdiri", value: 72)
!258 = !DIEnumerator(name: "mpx", value: 73)
!259 = !DIEnumerator(name: "mwaitx", value: 74)
!260 = !DIEnumerator(name: "nopl", value: 75)
!261 = !DIEnumerator(name: "pad_short_functions", value: 76)
!262 = !DIEnumerator(name: "pclmul", value: 77)
!263 = !DIEnumerator(name: "pconfig", value: 78)
!264 = !DIEnumerator(name: "pku", value: 79)
!265 = !DIEnumerator(name: "popcnt", value: 80)
!266 = !DIEnumerator(name: "prefer_128_bit", value: 81)
!267 = !DIEnumerator(name: "prefer_256_bit", value: 82)
!268 = !DIEnumerator(name: "prefer_mask_registers", value: 83)
!269 = !DIEnumerator(name: "prefetchwt1", value: 84)
!270 = !DIEnumerator(name: "prfchw", value: 85)
!271 = !DIEnumerator(name: "ptwrite", value: 86)
!272 = !DIEnumerator(name: "rdpid", value: 87)
!273 = !DIEnumerator(name: "rdrnd", value: 88)
!274 = !DIEnumerator(name: "rdseed", value: 89)
!275 = !DIEnumerator(name: "retpoline", value: 90)
!276 = !DIEnumerator(name: "retpoline_external_thunk", value: 91)
!277 = !DIEnumerator(name: "retpoline_indirect_branches", value: 92)
!278 = !DIEnumerator(name: "retpoline_indirect_calls", value: 93)
!279 = !DIEnumerator(name: "rtm", value: 94)
!280 = !DIEnumerator(name: "sahf", value: 95)
!281 = !DIEnumerator(name: "serialize", value: 96)
!282 = !DIEnumerator(name: "seses", value: 97)
!283 = !DIEnumerator(name: "sgx", value: 98)
!284 = !DIEnumerator(name: "sha", value: 99)
!285 = !DIEnumerator(name: "shstk", value: 100)
!286 = !DIEnumerator(name: "slow_3ops_lea", value: 101)
!287 = !DIEnumerator(name: "slow_incdec", value: 102)
!288 = !DIEnumerator(name: "slow_lea", value: 103)
!289 = !DIEnumerator(name: "slow_pmaddwd", value: 104)
!290 = !DIEnumerator(name: "slow_pmulld", value: 105)
!291 = !DIEnumerator(name: "slow_shld", value: 106)
!292 = !DIEnumerator(name: "slow_two_mem_ops", value: 107)
!293 = !DIEnumerator(name: "slow_unaligned_mem_16", value: 108)
!294 = !DIEnumerator(name: "slow_unaligned_mem_32", value: 109)
!295 = !DIEnumerator(name: "soft_float", value: 110)
!296 = !DIEnumerator(name: "sse", value: 111)
!297 = !DIEnumerator(name: "sse_unaligned_mem", value: 112)
!298 = !DIEnumerator(name: "sse2", value: 113)
!299 = !DIEnumerator(name: "sse3", value: 114)
!300 = !DIEnumerator(name: "sse4_1", value: 115)
!301 = !DIEnumerator(name: "sse4_2", value: 116)
!302 = !DIEnumerator(name: "sse4a", value: 117)
!303 = !DIEnumerator(name: "ssse3", value: 118)
!304 = !DIEnumerator(name: "tbm", value: 119)
!305 = !DIEnumerator(name: "tsxldtrk", value: 120)
!306 = !DIEnumerator(name: "use_aa", value: 121)
!307 = !DIEnumerator(name: "use_glm_div_sqrt_costs", value: 122)
!308 = !DIEnumerator(name: "vaes", value: 123)
!309 = !DIEnumerator(name: "vpclmulqdq", value: 124)
!310 = !DIEnumerator(name: "vzeroupper", value: 125)
!311 = !DIEnumerator(name: "waitpkg", value: 126)
!312 = !DIEnumerator(name: "wbnoinvd", value: 127)
!313 = !DIEnumerator(name: "x87", value: 128)
!314 = !DIEnumerator(name: "xop", value: 129)
!315 = !DIEnumerator(name: "xsave", value: 130)
!316 = !DIEnumerator(name: "xsavec", value: 131)
!317 = !DIEnumerator(name: "xsaveopt", value: 132)
!318 = !DIEnumerator(name: "xsaves", value: 133)
!319 = !DICompositeType(tag: DW_TAG_enumeration_type, name: "std.fmt.Alignment", scope: !320, file: !320, line: 16, baseType: !7, size: 8, align: 8, elements: !321)
!320 = !DIFile(filename: "fmt.zig", directory: "/nix/store/7k1ipq7x4b61jfhc3kwvm7zqfm5jzj2y-zig-0.6.0/lib/zig/std")
!321 = !{!322, !323, !324}
!322 = !DIEnumerator(name: "Left", value: 0)
!323 = !DIEnumerator(name: "Center", value: 1)
!324 = !DIEnumerator(name: "Right", value: 2)
!325 = !DICompositeType(tag: DW_TAG_enumeration_type, name: "std.spinlock.State", scope: !326, file: !326, line: 12, baseType: !183, size: 8, align: 8, elements: !327)
!326 = !DIFile(filename: "spinlock.zig", directory: "/nix/store/7k1ipq7x4b61jfhc3kwvm7zqfm5jzj2y-zig-0.6.0/lib/zig/std")
!327 = !{!328, !329}
!328 = !DIEnumerator(name: "Unlocked", value: 0)
!329 = !DIEnumerator(name: "Locked", value: 1)
