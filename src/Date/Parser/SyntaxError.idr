module Date.Parser.SyntaxError

import Date.Lexer.LexicalError

public export
data SyntaxError = 
      UnexpectedEndOfInput
    | UnexpectedToken String Int
    | SymbolDoesNotMatchType String String

public export
Show SyntaxError where
    show UnexpectedEndOfInput = 
        "[Syntax Error] Unexpected end of input"
    show (UnexpectedToken token position) = 
        "[Syntax Error] Unexpected token on position \{show position}: \"\{token}\""
    show (SymbolDoesNotMatchType type symbol) = 
        "[Syntax Error] Symbol does not match the type: '\{symbol}' is not a value of type \"\{type}\""