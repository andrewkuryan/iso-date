module Date.Locale

import Data.Vect
import Data.String
import Data.List1

import Date.ISOUtils

monthNames: String -> Maybe (Vect 12 String)
monthNames "en" = Just ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
monthNames _ = Nothing

monthToFin: Int -> Fin 12
monthToFin 1 = FZ
monthToFin 2 = shift 1 FZ
monthToFin 3 = shift 2 FZ
monthToFin 4 = shift 3 FZ
monthToFin 5 = shift 4 FZ
monthToFin 6 = shift 5 FZ
monthToFin 7 = shift 6 FZ
monthToFin 8 = shift 7 FZ
monthToFin 9 = shift 8 FZ
monthToFin 10 = shift 9 FZ
monthToFin 11 = shift 10 FZ
monthToFin 12 = shift 11 FZ
monthToFin _ = FZ

public export
getLocalMonthName: String -> Int -> Maybe String
getLocalMonthName locale month =  
    (monthNames (head (split (== '-') locale))) <&>
    (\names => index (monthToFin month) names) 