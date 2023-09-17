module Date.Lexer.ISOString

import Data.String

import public Date.Lexer.DFA

public export
data Token = Digit Nat | Dash | Colon | TimeSeparator

public export
Show Token where
    show (Digit digit) = show digit
    show Dash = "DASH"
    show Colon = "COLON"
    show TimeSeparator = "TIME_SEPARATOR"

public export
data State = 
      Y0  | Y1  | Y2  | Y3  | Y4
    | M0  | M1  | M2
    | D0  | D1  | D2
    | H0  | H1  | H2
    | MT0 | MT1 | MT2
    | S0  | S1  | S2

finalStates = [Y2, Y4, M2, D2, H2, MT2, S2]

public export
Eq State where
    (==) Y0 Y0 = True
    (==) Y1 Y1 = True
    (==) Y2 Y2 = True
    (==) Y3 Y3 = True
    (==) Y4 Y4 = True
    (==) M0 M0 = True
    (==) M1 M1 = True
    (==) M2 M2 = True
    (==) D0 D0 = True
    (==) D1 D1 = True
    (==) D2 D2 = True
    (==) H0 H0 = True
    (==) H1 H1 = True
    (==) H2 H2 = True
    (==) MT0 MT0 = True
    (==) MT1 MT1 = True
    (==) MT2 MT2 = True
    (==) S0 S0 = True
    (==) S1 S1 = True
    (==) S2 S2 = True
    (==) _ _ = False   

public export
parseToken: Char -> Maybe (Token)
parseToken '-' = Just Dash
parseToken ':' = Just Colon
parseToken 'T' = Just TimeSeparator
parseToken char = parsePositive (singleton char) <&> Digit

public export
isDigit: Nat -> Nat -> TokenValidator Token
isDigit min max = MkTokenValidator 
    (\token => case token of 
        (Digit digit) => if digit >= min && digit <= max then True else False 
        _ => False)
    "\{show min}..\{show max}"

isDash = MkTokenValidator (\token => case token of Dash => True; _ => False) "-"
isColon = MkTokenValidator (\token => case token of Colon => True; _ => False) ":"
isTimeSeparator = MkTokenValidator (\token => case token of TimeSeparator => True; _ => False) "T"

transitions = [
    Y0 ~> isDigit 1 9 ?> Y1, Y1 ~> isDigit 0 9 ?> Y2, Y2 ~> isDigit 0 9 ?> Y3, Y3 ~> isDigit 0 9 ?> Y4,
    Y4 ~> isDash ?> M0,
    M0 ~> isDigit 0 1 ?> M1, M1 ~> isDigit 0 9 ?> M2,
    M2 ~> isDash ?> D0,
    D0 ~> isDigit 0 3 ?> D1, D1 ~> isDigit 0 9 ?> D2,
    D2 ~> isTimeSeparator ?> H0,
    H0 ~> isDigit 0 2 ?> H1, H1 ~> isDigit 0 9 ?> H2,
    H2 ~> isColon ?> MT0,
    MT0 ~> isDigit 0 6 ?> MT1, MT1 ~> isDigit 0 9 ?> MT2,
    MT2 ~> isColon ?> S0,
    S0 ~> isDigit 0 6 ?> S1, S1 ~> isDigit 0 9 ?> S2
]

public export
processISOString: String -> Either LexicalError (List Token)
processISOString str = processChars (MkDFA transitions finalStates parseToken) Y0 0 [] (unpack str)