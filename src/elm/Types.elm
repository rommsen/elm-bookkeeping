module Types exposing (..)

import App.Types exposing (..)
import LineItems.Types exposing (..)
import Members.Types exposing (..)


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


type Msg
    = AppMsg App.Types.Msg
    | MemberMsg Members.Types.Msg
    | LineItemMsg LineItems.Types.Msg
