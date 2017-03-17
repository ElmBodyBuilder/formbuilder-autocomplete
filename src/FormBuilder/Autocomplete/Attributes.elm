module FormBuilder.Autocomplete.Attributes
    exposing
        ( AutocompleteAttributes
        , defaultAttributes
        , selection
        , selectedElement
        , searchQuery
        )

{-| Attributes for the autocomplete field. Extends the common attributes of FormBuilder.

# Type
@docs AutocompleteAttributes

# Attributes
@docs defaultAttributes
@docs selection
@docs selectedElement
@docs searchQuery
-}

import FormBuilder.FieldBuilder.Attributes as Attributes exposing (FieldAttributes)
import FormBuilder.Autocomplete.Action as Autocomplete


{-| Attributes of the autocomplete.
-}
type alias AutocompleteAttributes a msg =
    { choices : Maybe (List ( String, List a ))
    , choiceView : Maybe (a -> String)
    , onSelect : Maybe (a -> msg)
    , searchQuery : Maybe String
    , focused : Bool
    , selectedElement : Maybe a
    }


{-| Default attributes of the autocomplete field.
-}
defaultAttributes : FieldAttributes (AutocompleteAttributes a msg) msg
defaultAttributes =
    { common = Attributes.commonAttributes
    , choices = Nothing
    , choiceView = Nothing
    , onSelect = Nothing
    , focused = False
    , selectedElement = Nothing
    , searchQuery = Nothing
    }


{-| Set the required data of the autocomplete.
- choices are labelled list of the elements which have to be displayed.
- choiceView is a function, turning and element to a string.
- onSelect is the message which will be send after a selection in the autocomplete.
- searchQuery is the query entered by the user.
- focused is a bool, representing if the autocomplete is focused or not.
- formAttributes are the attributes of the field.
-}
selection :
    Maybe (List ( String, List a ))
    -> (a -> String)
    -> (a -> msg)
    -> String
    -> Bool
    -> FieldAttributes (AutocompleteAttributes a msg) msg
    -> FieldAttributes (AutocompleteAttributes a msg) msg
selection choices choiceView onSelect searchQuery focused formAttributes =
    { formAttributes
        | choices = choices
        , choiceView = Just choiceView
        , onSelect = Just onSelect
        , focused = focused
        , searchQuery = Just searchQuery
    }


{-| Set the selected element of the attributes to the value of the autocomplete model.
-}
selectedElement :
    { b | selectedElement : Maybe Int, elements : List a }
    -> FieldAttributes (AutocompleteAttributes a msg) msg
    -> FieldAttributes (AutocompleteAttributes a msg) msg
selectedElement autocompleteModel fieldAttributes =
    { fieldAttributes
        | selectedElement = Autocomplete.selectedElement autocompleteModel
    }


{-| Set the searchQuery of the autocomplete.
-}
searchQuery :
    String
    -> FieldAttributes (AutocompleteAttributes a msg) msg
    -> FieldAttributes (AutocompleteAttributes a msg) msg
searchQuery query fieldAttributes =
    { fieldAttributes | searchQuery = Just query }
