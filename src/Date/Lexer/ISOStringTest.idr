module Date.Lexer.ISOStringTest

import Data.List
import Data.String

import Date.Lexer.ISOString

shouldProcessCompleteDayExtendedFormat: 
    processISOString "1985-04-12" = 
    Right [Digit 1, Digit 9, Digit 8, Digit 5, Dash, Digit 0, Digit 4, Dash, Digit 1, Digit 2]
shouldProcessCompleteDayExtendedFormat = Refl

shouldProcessSpecificMonthBaseFormat: 
    processISOString "1985-04" = 
    Right [Digit 1, Digit 9, Digit 8, Digit 5, Dash, Digit 0, Digit 4]
shouldProcessSpecificMonthBaseFormat = Refl

shouldProcessSpecificYearBaseFormat: 
    processISOString "1985" = 
    Right [Digit 1, Digit 9, Digit 8, Digit 5]
shouldProcessSpecificYearBaseFormat = Refl

shouldProcessSpecificCenturyBaseFormat: 
    processISOString "19" = 
    Right [Digit 1, Digit 9]
shouldProcessSpecificCenturyBaseFormat = Refl

shouldProcessCompleteDateTimeExtendedFormat: 
    processISOString "1985-04-12T23:20:50" = 
    Right [Digit 1, Digit 9, Digit 8, Digit 5, Dash, Digit 0, Digit 4, Dash, Digit 1, Digit 2, TimeSeparator, Digit 2, Digit 3, Colon, Digit 2, Digit 0, Colon, Digit 5, Digit 0]
shouldProcessCompleteDateTimeExtendedFormat = Refl

shouldProcessSpecificHourAndMinuteTimeExtendedFormat: 
    processISOString "1985-04-12T23:20" = 
    Right [Digit 1, Digit 9, Digit 8, Digit 5, Dash, Digit 0, Digit 4, Dash, Digit 1, Digit 2, TimeSeparator, Digit 2, Digit 3, Colon, Digit 2, Digit 0]
shouldProcessSpecificHourAndMinuteTimeExtendedFormat = Refl

shouldProcessSpecificHourBasicFormat: 
    processISOString "1985-04-12T23" = 
    Right [Digit 1, Digit 9, Digit 8, Digit 5, Dash, Digit 0, Digit 4, Dash, Digit 1, Digit 2, TimeSeparator, Digit 2, Digit 3]
shouldProcessSpecificHourBasicFormat = Refl

shouldReturnUnexpectedEndOfInputError: 
    processISOString "1985-04-1" = 
    Left (UnexpectedEndOfInput ["\{show 0}..\{show 9}"])
shouldReturnUnexpectedEndOfInputError = Refl

shouldReturnUnexpectedSymbolErrorIfDashWasMissed: 
    processISOString "1985-0412" = 
    Left (UnexpectedSymbol ["-", "End of input"] 7 '1')
shouldReturnUnexpectedSymbolErrorIfDashWasMissed = Refl

shouldReturnUnexpectedSymbolErrorIfDigitOutOfBoundsWasPassed: 
    processISOString "1985-24-12" = 
    Left (UnexpectedSymbol ["\{show 0}..\{show 1}"] 5 '2')
shouldReturnUnexpectedSymbolErrorIfDigitOutOfBoundsWasPassed = Refl