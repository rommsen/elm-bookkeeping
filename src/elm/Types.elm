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


type alias Model =
    { members : List Member
    , member : Maybe Member
    , memberName : String
    , memberPayment : Float
    , monthForm : MonthForm
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


lineItemFormFromLineItem : LineItem -> LineItemForm
lineItemFormFromLineItem lineItem =
    LineItemForm lineItem.name (toString lineItem.amount) Dict.empty Nothing


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
