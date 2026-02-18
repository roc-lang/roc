# This file intentionally has a parse error for testing stderr reporting
main! =
    # Missing closing quote to trigger tokenization error
    x = "unclosed string
    x
