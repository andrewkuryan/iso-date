module Date.Lexer.LexicalError

public export
formatValidTokensList: List String -> String
formatValidTokensList [] = "∅"
formatValidTokensList [token] = "'\{token}'"
formatValidTokensList [token1, token2] = "'\{token1}' or '\{token2}'"
formatValidTokensList (token :: tokens) = "'\{token}', \{formatValidTokensList tokens}"

public export
data LexicalError = 
      UnexpectedSymbol (List String) Int Char
    | UnexpectedEndOfInput (List String)

public export
Show LexicalError where
    show (UnexpectedSymbol tokens index char) =
        "[Lexical Error] Unexpected symbol on position \{show index}: \{formatValidTokensList tokens} expected, but received \{show char}"
    show (UnexpectedEndOfInput tokens) = 
        "[Lexical Error] Unexpected end of input: \{formatValidTokensList tokens} expected"