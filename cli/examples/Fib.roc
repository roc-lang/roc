fibonacci = \num =>
    if num < 2 then
        num
    else
        fibonacci (num - 1) + fibonacci (num - 2)

fibonacci 9