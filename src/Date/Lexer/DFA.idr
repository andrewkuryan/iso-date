module Date.Lexer.DFA

import Data.List

import public Date.Lexer.LexicalError

public export
record TokenValidator t where
    constructor MkTokenValidator
    validate: (t -> Bool)
    displayName: String

public export
record Transition s t where
    constructor MkTransition
    from: s
    validator: TokenValidator t
    to: s

public export
(?>): Eq s => Show t => (s, TokenValidator t) -> s -> Transition s t
(?>) (from, validator) to = MkTransition from validator to

public export
(~>): Eq s => Show t => s -> TokenValidator t -> (s, TokenValidator t)
(~>) from validator = (from, validator)

infixl 10 ~>
infixl 10 ?>

public export
data DFA: Type -> Type -> Type where
    MkDFA: Eq s => Show t => List (Transition s t) -> List s -> (Char -> Maybe t) -> DFA s t

public export
getDFATransitions: DFA s t -> List (Transition s t)
getDFATransitions (MkDFA transitions _ _) = transitions

public export
getDFAFinalStates: DFA s _ -> List s
getDFAFinalStates (MkDFA _ finalStates _) = finalStates

public export
getDFAParseToken: DFA _ t -> (Char -> Maybe t)
getDFAParseToken (MkDFA _ _ parseToken) = parseToken

public export
processChar: Eq s => Show t => DFA s t -> s -> Char -> Maybe (s, t)
processChar dfa state char = (getDFAParseToken dfa) char >>=
    \token => find (\transition => transition.validator.validate token && transition.from == state) (getDFATransitions dfa) <&> 
        \transition => (transition.to, token)

public export
getTransitionDisplayNames: Eq s => Show t => DFA s t -> s -> List String
getTransitionDisplayNames dfa state = 
    let displayNames = map (.validator.displayName) (filter (((==) state) . .from) (getDFATransitions dfa)) in
        case find ((==) state) (getDFAFinalStates dfa) of
            (Just _) => snoc displayNames "End of input"
            Nothing => displayNames

public export
processChars: Eq s => Show t => DFA s t -> s -> Int -> List t -> List Char -> Either LexicalError (List t)
processChars dfa state index tokens (char :: rest) = 
    case processChar dfa state char of
        Just (nextState, newToken) => processChars dfa nextState (index + 1) (snoc tokens newToken) rest
        Nothing => Left (UnexpectedSymbol (getTransitionDisplayNames dfa state) index char)
processChars dfa state _ tokens [] = 
    case find ((==) state) (getDFAFinalStates dfa) of
        Just _ => Right tokens
        Nothing => Left (UnexpectedEndOfInput (getTransitionDisplayNames dfa state))