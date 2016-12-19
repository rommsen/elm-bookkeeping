module Sum exposing (sumAmount)

import Types exposing (HasAmount)


{-| This function can sum any List of records that have an amount property.
(a -> List (HasAmount b)) means need a function that returns a List of HasAmounts from a type)

 sumAmount .payments member
-}
sumAmount : (a -> List (HasAmount b)) -> a -> Float
sumAmount property record =
    List.foldl (\payment amount -> amount + payment.amount) 0 (property record)
