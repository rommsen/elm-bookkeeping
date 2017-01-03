module Types exposing (..)

import App.Types exposing (..)
import LineItems.Types exposing (..)
import Members.Types exposing (..)


type alias HasAmount a =
    { a | amount : Float }


type alias Model =
    { app : App.Types.Model
    , members : Members.Types.Model
    , lineItems : LineItems.Types.Model
    , totalBalance : Float
    }


type Msg
    = AppMsg App.Types.Msg
    | MemberMsg Members.Types.Msg
    | LineItemMsg LineItems.Types.Msg
