module Sum exposing (..)

import Types exposing (..)
import Members.Types exposing (Member)


withSummaries : Model -> Model
withSummaries model =
    let
        memberPaymentsTotal =
            model.members
                |> List.map sumMemberPayment
                |> List.sum

        memberDebitTotal =
            model.members
                |> List.map sumMemberDebit
                |> List.sum

        lineItemTotal =
            sumAmount .lineItems model
    in
        { model
            | totalBalance = memberPaymentsTotal + lineItemTotal
            , totalMemberDebit = memberPaymentsTotal - memberDebitTotal
        }


{-| This function can sum any List of records that have an amount property.
(a -> List (HasAmount b)) means need a function that returns a List of HasAmounts from a type)

 sumAmount .payments member
-}
sumAmount : (a -> List (HasAmount b)) -> a -> Float
sumAmount property record =
    List.foldl (\payment amount -> amount + payment.amount) 0 (property record)


sumMemberPayment : Member -> Float
sumMemberPayment member =
    sumAmount .payments member


sumMemberDebit : Member -> Float
sumMemberDebit member =
    sumAmount .months member


memberBalance : Member -> Float
memberBalance member =
    sumMemberPayment member - sumMemberDebit member
