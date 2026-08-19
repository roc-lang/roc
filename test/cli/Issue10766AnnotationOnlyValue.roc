# Regression test for issue #10766: referencing a non-function declaration that
# has a type annotation and no implementation must report DECLARATION HAS NO
# VALUE, rather than crashing post-check lowering by reading a function
# interface off a non-function type.
Issue10766AnnotationOnlyValue := [].{}

missing_value : I64

use_missing : I64
use_missing = missing_value
