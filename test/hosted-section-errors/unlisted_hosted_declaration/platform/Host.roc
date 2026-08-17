## Hosted functions for the unlisted-hosted-declaration check fixture.
## `unlisted!` is deliberately absent from the platform header's hosted section.
Host := [].{
    double! : I64 => I64
    unlisted! : I64 => I64
}
