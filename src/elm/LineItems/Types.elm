module LineItems.Types exposing (..)

import Dict
import Json.Decode as JD
import FormValidation


type alias LineItem =
    { id : String
    , name : String
    , amount : Float
    }


type alias LineItemForm =
    { name : String
    , amount : String
    , errors : FormValidation.FormErrors
    , result : Maybe LineItem
    }


type Msg
    = CancelLineItem
    | SelectLineItem LineItem
    | InputLineItemName String
    | InputLineItemAmount String
    | SaveLineItem
    | LineItemAdded JD.Value
    | LineItemUpdated JD.Value
    | LineItemDeleted JD.Value
    | DeleteLineItem LineItem


newLineItem : String -> Float -> LineItem
newLineItem name amount =
    LineItem "" name amount


emptyLineItemForm : LineItemForm
emptyLineItemForm =
    LineItemForm "" "" Dict.empty Nothing


lineItemFormFromLineItem : LineItem -> LineItemForm
lineItemFormFromLineItem lineItem =
    LineItemForm lineItem.name (toString lineItem.amount) Dict.empty Nothing
