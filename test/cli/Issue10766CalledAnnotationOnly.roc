# Regression test for issue #10766: calling a declaration that has a type
# annotation and no implementation must report DECLARATION HAS NO VALUE and
# keep compiling, rather than crashing post-check lowering with an
# instantiation invariant.
Issue10766CalledAnnotationOnly := [].{}

missing_impl : I64 -> I64

call_missing : I64
call_missing = missing_impl(1)
