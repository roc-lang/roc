# Unannotated methods of a recursive nominal whose base tag has no payload.
# Compile-time evaluation runs each `expect`'s constants under every strategy.
RecursiveChain := [End, Link(RecursiveChain)].{
    label = |c| match c {
        End => "end"
        Link(inner) => inner.label()
    }

    zero = |c| match c {
        End => 0
        Link(inner) => inner.zero()
    }

    none = |c| match c {
        End => []
        Link(inner) => inner.none()
    }
}

chain = RecursiveChain.Link(RecursiveChain.Link(RecursiveChain.End))

labelled = chain.label()

expect labelled == "end"

expect {
    zero : U64
    zero = chain.zero()
    zero == 0
}

expect {
    none : List(U64)
    none = chain.none()
    none == []
}
