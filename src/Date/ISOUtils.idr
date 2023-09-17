module Date.ISOUtils

import Data.Nat

public export
data ISOYear: Int -> Type where
    MkISOYear: (value: Int) -> (value >= 1583) === True -> (value <= 9999) === True -> ISOYear value

public export
isLeap : (ISOYear _) -> Bool
isLeap (MkISOYear year _ _) = mod year 4 == 0 && (mod year 100 /= 0 || mod year 400 == 0)

public export
data ISOMonth: Int -> Type where
    MkISOMonth: (value: Int) -> (value >= 1) === True -> (value <= 12) === True -> ISOMonth value

public export
data DaysInMonth = Mk28 | Mk29 | Mk30 | Mk31

public export
daysInMonthToInt: DaysInMonth -> Int
daysInMonthToInt Mk28 = 28
daysInMonthToInt Mk29 = 29
daysInMonthToInt Mk30 = 30
daysInMonthToInt Mk31 = 31

public export
daysInMonth: (ISOYear _) -> (ISOMonth _) -> DaysInMonth
daysInMonth year (MkISOMonth 2 _ _) = if isLeap year then Mk29 else Mk28
daysInMonth _ (MkISOMonth 4 _ _) = Mk30
daysInMonth _ (MkISOMonth 6 _ _) = Mk30
daysInMonth _ (MkISOMonth 9 _ _) = Mk30
daysInMonth _ (MkISOMonth 11 _ _) = Mk30
daysInMonth _ _ = Mk31

public export
data ISODayOfMonth: (ISOYear _) -> (ISOMonth _) -> Int -> Type where
    MkISODayOfMonth: 
        (year: (ISOYear _)) -> (month: (ISOMonth _)) -> (value: Int) -> 
        (value >= 1) === True -> (value <= (daysInMonthToInt (daysInMonth year month))) === True ->
        ISODayOfMonth year month value
    MkDefaultISODayOfMonth: ISODayOfMonth _ _ 1

public export
data ISOHours: Int -> Type where
    MkISOHours: (value: Int) -> (value <= 24) === True -> ISOHours value

public export
data ISOMinutes: Int -> Type where
    MkISOMinutes: (value: Int) -> (value <= 60) === True -> ISOMinutes value

public export
data ISOSeconds: Int -> Type where
    MkISOSeconds: (value: Int) -> (value <= 60) === True -> ISOSeconds value                         

public export
isLTE: (value: Int) -> (constraint: Int) -> Dec ((value <= constraint) === True)
isLTE value constraint with (the (test : Bool ** (value <= constraint) === test) (value <= constraint ** Refl))
    isLTE value constraint | (True ** p) = Yes p
    isLTE value constraint | (False ** p)  = No (\q => absurd (trans (sym p) q))

public export
isGTE: (value: Int) -> (constraint: Int) -> Dec ((value >= constraint) === True)
isGTE value constraint with (the (test : Bool ** (value >= constraint) === test) (value >= constraint ** Refl))
    isGTE value constraint | (True ** p) = Yes p
    isGTE value constraint | (False ** p)  = No (\q => absurd (trans (sym p) q))

public export
isISOYear: (value: Int) -> Dec (ISOYear value)
isISOYear value = case (isGTE value 1583, isLTE value 9999) of
    (Yes minPrf, Yes maxPrf) => Yes (MkISOYear value minPrf maxPrf)
    (No disPrf, _) => No (\(MkISOYear _ q _) => disPrf q)
    (_, No disPrf) => No (\(MkISOYear _ _ q) => disPrf q)    

public export
isISOMonth: (value: Int) -> Dec (ISOMonth value)
isISOMonth value = case (isGTE value 1, isLTE value 12) of
    (Yes minPrf, Yes maxPrf) => Yes (MkISOMonth value minPrf maxPrf)
    (No disPrf, _) => No (\(MkISOMonth _ q _) => disPrf q)
    (_, No disPrf) => No (\(MkISOMonth _ _ q) => disPrf q)

is1LTEDaysInMonth: {days: DaysInMonth} -> (1 <= (daysInMonthToInt days)) === True
is1LTEDaysInMonth = case days of
    Mk28 => Refl
    Mk29 => Refl
    Mk30 => Refl
    Mk31 => Refl

public export
isISODayOfMonth: (year: (ISOYear _)) -> (month: (ISOMonth _)) -> (value: Int) -> Dec (ISODayOfMonth year month value)
isISODayOfMonth year month value = case (isGTE value 1, isLTE value (daysInMonthToInt (daysInMonth year month))) of
    (Yes minPrf, Yes maxPrf) => Yes (MkISODayOfMonth year month value minPrf maxPrf)
    (No disPrf, _) => No (\dayOfMonth => case dayOfMonth of 
        (MkISODayOfMonth _ _ _ q _) => disPrf q
        MkDefaultISODayOfMonth => disPrf Refl)
    (_, No disPrf) => No (\dayOfMonth => case dayOfMonth of 
        (MkISODayOfMonth _ _ _ _ q) => disPrf q
        MkDefaultISODayOfMonth => disPrf is1LTEDaysInMonth)

public export
isISOHours: (value: Int) -> Dec (ISOHours value)
isISOHours value = case isLTE value 24 of
    Yes prf => Yes (MkISOHours value prf)
    No disPrf => No (\(MkISOHours _ q) => disPrf q)    

public export
isISOMinutes: (value: Int) -> Dec (ISOMinutes value)
isISOMinutes value = case isLTE value 60 of
    Yes prf => Yes (MkISOMinutes value prf)
    No disPrf => No (\(MkISOMinutes _ q) => disPrf q)    

public export
isISOSeconds: (value: Int) -> Dec (ISOSeconds value)
isISOSeconds value = case isLTE value 60 of
    Yes prf => Yes (MkISOSeconds value prf)
    No disPrf => No (\(MkISOSeconds _ q) => disPrf q)       