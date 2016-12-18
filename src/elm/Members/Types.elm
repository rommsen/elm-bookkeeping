module Members.Types exposing (..)

import Date
import Dict
import FormValidation
import Form.Validation exposing (..)
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
    , errors : FormValidation.FormErrors
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
    , errors : List Error
    }


type alias Payment =
    { amount : Float
    , added : Date.Date
    }


type alias PaymentForm =
    { amount : String
    , errors : FormValidation.FormErrors
    , result : Maybe Float
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
    | DeleteMemberPayment Member Payment
    | SelectMonth Date.Month
    | InputMonthYear String
    | InputMonthAmount String
    | AddMonthToActiveMembers
    | DeleteMonthFromMember Month Member
    | ChangeMemberPane MemberPane
    | FilterMembers MemberFilter


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
    MonthForm Date.Jan "2016" "7.5" []


memberWithName : String -> Member
memberWithName name =
    Member "" name True [] []


months : List Date.Month
months =
    [ Date.Jan, Date.Feb, Date.Mar, Date.Apr, Date.May, Date.Jun, Date.Jul, Date.Aug, Date.Sep, Date.Oct, Date.Nov, Date.Dec ]
