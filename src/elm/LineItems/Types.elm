module LineItems.Types exposing (..)

import Json.Decode as JD
import Form.Validation exposing (..)


type alias Model =
    { lineItems : List LineItem
    , lineItem : Maybe LineItem
    , lineItemForm : LineItemForm
    , lineItemTotal : Float
    }


type alias LineItem =
    { id : String
    , name : String
    , amount : Float
    }


type alias LineItemForm =
    { name : String
    , amount : String
    , errors : List Error
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
    | LineItemsRetrieved (List JD.Value)
    | DeleteLineItem LineItem


newLineItem : String -> Float -> LineItem
newLineItem name amount =
    LineItem "" name amount


emptyLineItemForm : LineItemForm
emptyLineItemForm =
    LineItemForm "" "" []


lineItemFormFromLineItem : LineItem -> LineItemForm
lineItemFormFromLineItem lineItem =
    LineItemForm lineItem.name (toString lineItem.amount) []
