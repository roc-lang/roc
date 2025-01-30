module [plain_text, em_text]

import Symbol exposing [Ident]

plain_text = \str -> PlainText(str)

em_text = \str -> EmText(str)
