module App.Types exposing (..)


type Tab
    = MemberTab
    | LineItemTab


type Msg
    = SelectTab Tab
