# Monotype lowering

This directory specializes checked Roc programs into fully concrete types and
lowers them into the program representation consumed by evaluation and code
generation.

Body lowering treats its instantiation graph as the authority for every
`TypeId` it inspects. This ownership boundary prevents body-local type state
from being confused with coordinator-owned program types, and allows completed
specialization work to cross into the final program only through ordered
commit and explicit type relocation.
