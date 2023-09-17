module Date.Parser.ISOString

import Data.List

import Date.ISOUtils
import public Date.Lexer.ISOString
import public Date.Parser.SyntaxError

public export
data Symbol: Type where
    MkYear: (value: Int) -> ISOYear value -> Symbol
    MkMonth: (value: Int) -> ISOMonth value -> Symbol    
    MkDayOfMonth: (year: ISOYear _) -> (month: ISOMonth _) -> (value: Int) -> ISODayOfMonth year month value -> Symbol
    MkHours: (value: Int) -> ISOHours value -> Symbol
    MkMinutes: (value: Int) -> ISOMinutes value -> Symbol
    MkSeconds: (value: Int) -> ISOSeconds value -> Symbol
    𝛆: Symbol

public export
buildNumber: Int -> List Nat -> Int
buildNumber acc (digit :: digits) = buildNumber (acc * 10 + (cast digit)) digits
buildNumber acc [] = acc

public export
withConstraint: {a: Int -> Type} -> List Nat -> ((v: Int) -> Dec (a v)) -> ((v: Int) -> a v -> Symbol) -> String -> Either SyntaxError Symbol
withConstraint digits constraint construct displayName = let value = buildNumber 0 digits in
    case constraint value of
        Yes prf => Right (construct value prf)
        _ => Left (SymbolDoesNotMatchType displayName (show value))

public export
matchYear: List Nat -> Either SyntaxError Symbol
matchYear digits = 
    withConstraint (case digits of [y1, y2] => [y1, y2, 0, 0]; other => other) isISOYear MkYear "ISOYear"

public export
matchMonth: List Nat -> Either SyntaxError Symbol
matchMonth digits = withConstraint digits isISOMonth MkMonth "ISOMonth"

public export
matchDayOfMonth: (ISOYear _) -> (ISOMonth _) -> List Nat -> Either SyntaxError Symbol
matchDayOfMonth year month digits =
    withConstraint digits (isISODayOfMonth year month) (MkDayOfMonth year month) "ISODayOfMonth"

public export
matchHours: List Nat -> Either SyntaxError Symbol
matchHours digits = withConstraint digits isISOHours MkHours "ISOHours"

public export
matchMinutes: List Nat -> Either SyntaxError Symbol
matchMinutes digits = withConstraint digits isISOMinutes MkMinutes "ISOMinutes"

public export
matchSeconds: List Nat -> Either SyntaxError Symbol
matchSeconds digits = withConstraint digits isISOSeconds MkSeconds "ISOSeconds"

public export
matchTokens: Int -> List Symbol -> List Token -> Either SyntaxError (List Token, Symbol)
matchTokens _ [] 
              (Digit y1 :: Digit y2 :: []) 
            = matchYear [y1, y2] <&> (MkPair [])
matchTokens _ []
              (Digit y1 :: Digit y2 :: Digit y3 :: Digit y4 :: rest) 
            = matchYear [y1, y2, y3, y4] <&> (MkPair rest)
matchTokens _ [MkYear _ _]
              (Dash :: Digit m1 :: Digit m2 :: rest) 
            = matchMonth [m1, m2] <&> (MkPair rest)
matchTokens _ [MkYear _ year, MkMonth _ month] 
              (Dash :: Digit d1 :: Digit d2 :: rest) 
            = matchDayOfMonth year month [d1, d2] <&> (MkPair rest)
matchTokens _ [MkYear _ _, MkMonth _ _, MkDayOfMonth _ _ _ _] 
              (TimeSeparator :: Digit h1 :: Digit h2 :: rest) 
            = matchHours [h1, h2] <&> (MkPair rest)
matchTokens _ [MkYear _ _, MkMonth _ _, MkDayOfMonth _ _ _ _, MkHours _ _] 
              (Colon :: Digit m1 :: Digit m2 :: rest) 
            = matchMinutes [m1, m2] <&> (MkPair rest)
matchTokens _ [MkYear _ _, MkMonth _ _, MkDayOfMonth _ _ _ _, MkHours _ _, MkMinutes _ _] 
              (Colon :: Digit s1 :: Digit s2 :: []) 
            = matchSeconds [s1, s2] <&> (MkPair [])
matchTokens _ [MkYear _ _] [] = Right ([], 𝛆)
matchTokens _ [MkYear _ _, MkMonth _ _] [] = Right ([], 𝛆)
matchTokens _ [MkYear _ _, MkMonth _ _, MkDayOfMonth _ _ _ _] [] = Right ([], 𝛆)
matchTokens _ [MkYear _ _, MkMonth _ _, MkDayOfMonth _ _ _ _, MkHours _ _] [] = Right ([], 𝛆)
matchTokens _ [MkYear _ _, MkMonth _ _, MkDayOfMonth _ _ _ _, MkHours _ _, MkMinutes _ _] [] = Right ([], 𝛆)
matchTokens _ [MkYear _ _, MkMonth _ _, MkDayOfMonth _ _ _ _, MkHours _ _, MkMinutes _ _, MkSeconds _ _] [] = Right ([], 𝛆)
matchTokens position symbols (token :: tokens) = Left (UnexpectedToken (show token) position)
matchTokens _ symbols [] = Left UnexpectedEndOfInput

public export
parseTokens: Int -> List Token -> List Symbol -> Either SyntaxError (List Symbol)
parseTokens position tokens symbols = 
    matchTokens position symbols tokens >>= 
    \(restTokens, newSymbol) => case newSymbol of
        𝛆 => Right symbols
        _ => parseTokens 
                (position + (cast (length tokens)) - (cast (length restTokens))) 
                restTokens
                (snoc symbols newSymbol)

public export
parseISOString: List Token -> Either SyntaxError (List Symbol)
parseISOString tokens = parseTokens 0 tokens []