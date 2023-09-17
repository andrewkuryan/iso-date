module Date.ISODate

import Data.String
import Data.Either

import Date.ISOUtils
import Date.Lexer.ISOString
import Date.Parser.ISOString

public export
data ISODate: Type where
    MkISODate: 
        (year: Int) ->
        {auto 0 yearPrf: (ISOYear year)} ->
        (month: Int) ->
        {auto 0 monthPrf: (ISOMonth month)} ->
        (dayOfMonth: Int) -> 
        {auto 0 _: (ISODayOfMonth yearPrf monthPrf dayOfMonth)} ->
        (hours: Int) -> 
        {auto 0 _: (ISOHours hours)} ->
        (minutes: Int) -> 
        {auto 0 _: (ISOMinutes minutes)} ->
        (seconds: Int) -> 
        {auto 0 _: (ISOSeconds seconds)} ->
        ISODate

public export 
Show ISODate where
    show (MkISODate year month dayOfMonth hours minutes seconds) = 
        show year ++ "-" ++
        padLeft 2 '0' (show month) ++ "-" ++
        padLeft 2 '0' (show dayOfMonth) ++
        "T" ++
        padLeft 2 '0' (show hours) ++ ":" ++
        padLeft 2 '0' (show minutes) ++ ":" ++
        padLeft 2 '0' (show seconds)

public export
fromISOString: String -> Maybe ISODate
fromISOString str = (eitherToMaybe . processISOString) str >>= (eitherToMaybe . parseISOString) >>= 
    \symbols => case symbols of
        [MkYear year _] => 
            Just (MkISODate year 1 1 0 0 0)
        [MkYear year yearPrf, MkMonth month monthPrf] => 
            Just (MkISODate year month 1 0 0 0)
        [MkYear _ _, MkMonth _ _, MkDayOfMonth (MkISOYear year _ _) (MkISOMonth month _ _) dayOfMonth _] =>
            Just (MkISODate year month dayOfMonth 0 0 0)
        [
            MkYear _ _, MkMonth _ _, MkDayOfMonth (MkISOYear year _ _) (MkISOMonth month _ _) dayOfMonth _, 
            MkHours hours _
        ] =>
            Just (MkISODate year month dayOfMonth hours 0 0)
        [  
            MkYear _ _, MkMonth _ _, MkDayOfMonth (MkISOYear year _ _) (MkISOMonth month _ _) dayOfMonth _, 
            MkHours hours _, MkMinutes minutes _
        ] =>            
            Just (MkISODate year month dayOfMonth hours minutes 0)
        [
            MkYear _ _, MkMonth _ _, MkDayOfMonth (MkISOYear year _ _) (MkISOMonth month _ _) dayOfMonth _, 
            MkHours hours _, MkMinutes minutes _, MkSeconds seconds _
        ] =>            
            Just (MkISODate year month dayOfMonth hours minutes seconds)            
        _ => Nothing