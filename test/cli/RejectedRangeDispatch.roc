expect [Low] == (RejectedRangeDispatch.Low..=High).iter().take_first(1).collect()
expect [Low] == (RejectedRangeDispatch.Low..=Low).iter().take_first(1).collect()
expect [Low] == (alias_low..=alias_high).iter().take_first(1).collect()
expect [Low] == (alias_low..=alias_low).iter().take_first(1).collect()

RejectedRangeDispatch := [Low, High].{
    between_inclusive : RejectedRangeDispatch, RejectedRangeDispatch -> Iter(RejectedRangeDispatch)
    between_inclusive = |lower, _| (0..=0).iter().map(|_| lower)

    range_inclusive_to : RejectedRangeDispatch, RejectedRangeDispatch -> Iter(RejectedRangeDispatch)
    range_inclusive_to = |lower, upper| lower.between_inclusive(upper)
}

RejectedAliasRange : RejectedRangeDispatch

alias_low : RejectedAliasRange
alias_low = RejectedRangeDispatch.Low

alias_high : RejectedAliasRange
alias_high = RejectedRangeDispatch.High
