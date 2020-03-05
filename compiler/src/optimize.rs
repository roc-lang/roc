// PHILOSOPHY
//
// Focus on optimizations which are only safe in the absence of side effects, and leave the rest to LLVM.
//
// This focus may lead to some optimizations becoming transitively in scope. For example, some deforestation
// examples in the MSR paper benefit from multiple rounds of interleaved deforestation, beta-reduction, and inlining.
// To get those benefits, we'd have to do some inlining and beta-reduction that we could otherwise leave to LLVM's
// inlining and constant propagation/folding.
//
// Even if we're doing those things, it may still make sense to have LLVM do a pass for them as well, since
// early LLVM optimization passes may unlock later opportunities for inlining and constant propagation/folding.
//
// INLINING
//
// If a function is called exactly once (it's a helper function), presumably we always want to inline those.
// If a function is "small enough" it's probably worth inlining too.
//
// FUSION
//
// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/deforestation-short-cut.pdf
//
// Basic approach:
//
// Do list stuff using `build` passing Cons Nil (like a cons list) and then do foldr/build substitution/reduction.
// Afterwards, we can do a separate pass to flatten nested Cons structures into properly initialized RRBTs.
// This way we get both deforestation and efficient RRBT construction. Should work for the other collection types too.
//
// It looks like we need to do some amount of inlining and beta reductions on the Roc side, rather than
// leaving all of those to LLVM.
//
// Advanced approach:
//
// Express operations like map and filter in terms of toStream and fromStream, to unlock more deforestation.
// More info on here:
//
// https://wiki.haskell.org/GHC_optimisations#Fusion
