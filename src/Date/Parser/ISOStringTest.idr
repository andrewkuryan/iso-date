module Date.Parser.ISOStringTest

import Data.List

import Date.ISOUtils
import Date.Parser.ISOString

getISOYear: (value: Int) -> {auto minPrf: (value >= 1583) === True} -> {auto maxPrf: (value <= 9999) === True} -> ISOYear value
getISOYear value = MkISOYear value minPrf maxPrf

getYear: (value: Int) -> {auto minPrf: (value >= 1583) === True} -> {auto maxPrf: (value <= 9999) === True} -> Symbol
getYear value = MkYear value (getISOYear value)

getISOMonth: (value: Int) -> {auto minPrf: (value >= 1) === True} -> {auto maxPrf: (value <= 12) === True} -> ISOMonth value
getISOMonth value = MkISOMonth value minPrf maxPrf

getMonth: (value: Int) -> {auto minPrf: (value >= 1) === True} -> {auto maxPrf: (value <= 12) === True} -> Symbol
getMonth value = MkMonth value (getISOMonth value)

getDayOfMonth: 
    (year: ISOYear _) -> (month: ISOMonth _) -> (value: Int) -> 
    {auto minPrf: (value >= 1) === True} -> {auto maxPrf: (value <= (daysInMonthToInt (daysInMonth year month))) === True} -> 
    Symbol
getDayOfMonth year month value = MkDayOfMonth year month value (MkISODayOfMonth year month value minPrf maxPrf)

getHours: (value: Int) -> {auto prf: (value <= 24) === True} -> Symbol
getHours value = MkHours value (MkISOHours value prf)

getMinutes: (value: Int) -> {auto prf: (value <= 60) === True} -> Symbol
getMinutes value = MkMinutes value (MkISOMinutes value prf)

getSeconds: (value: Int) -> {auto prf: (value <= 60) === True} -> Symbol
getSeconds value = MkSeconds value (MkISOSeconds value prf)

shouldParseCompleteDayExtendedFormat:
    parseISOString [Digit 1, Digit 9, Digit 8, Digit 5, Dash, Digit 0, Digit 4, Dash, Digit 1, Digit 2] = 
    Right [getYear 1985, getMonth 4, getDayOfMonth (getISOYear 1985) (getISOMonth 4) 12]
shouldParseCompleteDayExtendedFormat = Refl

shouldParseDateInLeapYear:
    parseISOString [Digit 2, Digit 0, Digit 2, Digit 0, Dash, Digit 0, Digit 2, Dash, Digit 2, Digit 9] =
    Right [getYear 2020, getMonth 2, getDayOfMonth (getISOYear 2020) (getISOMonth 2) 29]
shouldParseDateInLeapYear = Refl

shouldParseSpecificYearBaseFormat: 
    parseISOString [Digit 1, Digit 9, Digit 8, Digit 5] = 
    Right [getYear 1985]
shouldParseSpecificYearBaseFormat = Refl

shouldParseSpecificCenturyBaseFormat: 
    parseISOString [Digit 1, Digit 9] = 
    Right [getYear 1900]
shouldParseSpecificCenturyBaseFormat = Refl

shouldParseCompleteDateTimeExtendedFormat: 
    parseISOString [Digit 1, Digit 9, Digit 8, Digit 5, Dash, Digit 0, Digit 4, Dash, Digit 1, Digit 2, TimeSeparator, Digit 2, Digit 3, Colon, Digit 2, Digit 0, Colon, Digit 5, Digit 0] = 
    Right [getYear 1985, getMonth 4, getDayOfMonth (getISOYear 1985) (getISOMonth 4) 12, getHours 23, getMinutes 20, getSeconds 50]
shouldParseCompleteDateTimeExtendedFormat = Refl

shouldReturnUnexpectedEndOfInputError:
    parseISOString [] = 
    Left UnexpectedEndOfInput
shouldReturnUnexpectedEndOfInputError = Refl

shouldReturnUnexpectedTokenError: 
    parseISOString [Digit 1, Digit 9, Digit 8, Digit 5, Dash, Digit 0, Digit 4, Dash, Digit 1] =
    Left (UnexpectedToken (show Dash) 7)
shouldReturnUnexpectedTokenError = Refl

shouldReturnSymbolDoesNotMatchTypeError:
    parseISOString [Digit 2, Digit 0, Digit 2, Digit 1, Dash, Digit 0, Digit 2, Dash, Digit 2, Digit 9] =
    Left (SymbolDoesNotMatchType "ISODayOfMonth" (show (buildNumber 0 [2, 9])))
shouldReturnSymbolDoesNotMatchTypeError = Refl