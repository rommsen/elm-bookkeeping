module Types exposing (..)

import Date
import Dict
import Json.Decode as JD


type alias Member =
    { id : String
    , name : String
    , active : Bool
    , months : List Month
    , payments : List Payment
    }


type alias MemberNameForm =
    { name : String
    , errors : FormErrors
    , result : Maybe String
    }


type MemberPane
    = MemberPaneShowNone
    | MemberPaneShowDetails Member
    | MemberPaneAddMonth
    | MemberPaneAddMember


type MemberFilter
    = MemberFilterAll
    | MemberFilterActive
    | MemberFilterInactive


type alias Month =
    { month : Date.Month
    , year : Int
    , amount : Float
    }


type alias MonthForm =
    { month : Date.Month
    , year : String
    , amount : String
    , errors : FormErrors
    , result : Maybe Month
    }


type alias FormErrors =
    Dict.Dict String (Maybe String)


type alias FormError =
    ( String, Maybe String )


type alias Payment =
    { amount : Float
    , added : Date.Date
    }


type alias PaymentForm =
    { amount : String
    , errors : FormErrors
    , result : Maybe Float
    }


type alias LineItem =
    { id : String
    , name : String
    , amount : Float
    }


type alias LineItemForm =
    { name : String
    , amount : String
    , errors : FormErrors
    , result : Maybe LineItem
    }


type alias HasAmount a =
    { a | amount : Float }


type alias Model =
    { members : List Member
    , member : Maybe Member
    , memberNameForm : MemberNameForm
    , monthForm : MonthForm
    , paymentForm : PaymentForm
    , lineItems : List LineItem
    , lineItem : Maybe LineItem
    , lineItemForm : LineItemForm
    , totalBalance : Float
    , totalMemberDebit : Float
    , memberPane : MemberPane
    , memberFilter : MemberFilter
    , selectedTab : Tab
    }


type Tab
    = MemberTab
    | LineItemTab


type Msg
    = SaveMemberName
    | MemberAdded JD.Value
    | MemberUpdated JD.Value
    | InputMemberName String
    | ToggleMemberIsActive Member
    | InputMemberPaymentAmount String
    | CreateMemberPayment
    | SaveMemberPayment Member Payment
    | DeleteMemberPayment Member Payment
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
    | SelectTab Tab
    | ChangeMemberPane MemberPane
    | FilterMembers MemberFilter


months : List Date.Month
months =
    [ Date.Jan, Date.Feb, Date.Mar, Date.Apr, Date.May, Date.Jun, Date.Jul, Date.Aug, Date.Sep, Date.Oct, Date.Nov, Date.Dec ]


newLineItem : String -> Float -> LineItem
newLineItem name amount =
    LineItem "" name amount


emptyLineItemForm : LineItemForm
emptyLineItemForm =
    LineItemForm "" "" Dict.empty Nothing


emptyMemberNameForm : MemberNameForm
emptyMemberNameForm =
    MemberNameForm "" Dict.empty Nothing


emptyPaymentForm : PaymentForm
emptyPaymentForm =
    PaymentForm "" Dict.empty Nothing


memberNameFormFromMember : Member -> MemberNameForm
memberNameFormFromMember member =
    MemberNameForm member.name Dict.empty <| Just member.name


emptyMonthForm : MonthForm
emptyMonthForm =
    MonthForm Date.Jan "2016" "7.5" Dict.empty Nothing


lineItemFormFromLineItem : LineItem -> LineItemForm
lineItemFormFromLineItem lineItem =
    LineItemForm lineItem.name (toString lineItem.amount) Dict.empty Nothing


memberWithName : String -> Member
memberWithName name =
    Member "" name True [] []


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
