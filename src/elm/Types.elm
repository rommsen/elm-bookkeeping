module Types exposing (..)

import Date
import Json.Decode as JD


type alias Member =
    { id : String
    , name : String
    , active : Bool
    , months : List Month
    , payments : List Payment
    }


type MemberPane
    = MemberPaneShowNone
    | MemberPaneShowDetails Member
    | MemberPaneAddMonth
    | MemberPaneAddMember


type alias Month =
    { month : Date.Month
    , year : Int
    , amount : Float
    }


type alias Payment =
    { amount : Float
    , added : Date.Date
    }


type alias LineItem =
    { id : String
    , name : String
    , amount : Float
    }


type alias Model =
    { members : List Member
    , member : Maybe Member
    , memberName : String
    , memberPayment : Float
    , month : Month
    , lineItems : List LineItem
    , lineItem : Maybe LineItem
    , lineItemName : String
    , lineItemAmount : Float
    , totalBalance : Float
    , totalMemberDebit : Float
    , memberPane : MemberPane
    , selectedTab : Int
    }


type Msg
    = SaveMemberName
    | MemberAdded JD.Value
    | MemberUpdated JD.Value
    | InputMemberName String
    | ToggleMemberIsActive Member
    | InputMemberPaymentAmount String
    | CreateMemberPayment
    | SaveMemberPayment Member Payment
    | SelectMonth Date.Month
    | InputMonthYear String
    | InputMonthAmount String
    | AddMonthToActiveMembers
    | DeleteMonthFromMember Month Member
    | SelectLineItem LineItem
    | CancelLineItem
    | InputLineItemName String
    | InputLineItemAmount String
    | SaveLineItem
    | LineItemAdded JD.Value
    | LineItemUpdated JD.Value
    | LineItemDeleted JD.Value
    | DeleteLineItem LineItem
    | SelectTab Int
    | ChangeMemberPane MemberPane


months : List Date.Month
months =
    [ Date.Jan, Date.Feb, Date.Mar, Date.Apr, Date.May, Date.Jun, Date.Jul, Date.Aug, Date.Sep, Date.Oct, Date.Nov, Date.Dec ]


newLineItem : String -> Float -> LineItem
newLineItem name amount =
    LineItem "" name amount


memberWithName : String -> Member
memberWithName name =
    Member "" name True [] []



-- this function can sum any List of records that have an amount property


sumAmount : (a -> List { b | amount : Float }) -> a -> Float
sumAmount property record =
    List.foldl (\payment amount -> amount + payment.amount) 0 (property record)


memberPayment : Member -> Float
memberPayment member =
    sumAmount .payments member


memberDebit : Member -> Float
memberDebit member =
    sumAmount .months member


memberBalance : Member -> Float
memberBalance member =
    memberPayment member - memberDebit member
