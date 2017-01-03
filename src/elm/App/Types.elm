module App.Types exposing (..)


type alias Model =
    { selectedTab : Tab
    }


type Tab
    = MemberTab
    | LineItemTab


type Msg
    = SelectTab Tab



-- | Login
