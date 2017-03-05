module FormBuilder.Fields.Autocomplete.Attributes exposing (..)

{-| Autocomplete custom field attributes.

    # Custom attributes
    @docs defaultFieldAttributes

    # Attributes modifiers
    @docs selection, selectedElement
-}

import FormBuilder.FieldBuilder exposing (FormAttributes)
import FormBuilder.Fields.Autocomplete
import FormBuilder.Fields.Autocomplete.Type exposing (AutocompleteAttributes)


{-| À mettre à jour, puis documenter.
-}
defaultFieldAttributes : FormAttributes (AutocompleteAttributes a msg) msg
defaultFieldAttributes =
    { value = Nothing
    , id = Nothing
    , type_ = Nothing
    , label = Nothing
    , placeholder = Nothing
    , mandatory = Nothing
    , hidden = Nothing
    , options = Nothing
    , event = Nothing
    , noBottomPadding = Nothing
    , choices = Nothing
    , choiceView = Nothing
    , onSelect = Nothing
    , searchQuery = Nothing
    , onFocus = Nothing
    , onBlur = Nothing
    , onChange = Nothing
    , focused = False
    , selectedElement = Nothing
    }


{-| Set the selection. It includes following attributes:
        - choices
        - choicesView
        - onSelect
        - searchQuery
        - focused
-}
selection : Maybe (List ( String, List a )) -> (a -> String) -> (a -> msg) -> String -> Bool -> FormAttributes (AutocompleteAttributes a msg) msg -> FormAttributes (AutocompleteAttributes a msg) msg
selection choices choiceView onSelect searchQuery focused formAttributes =
    { formAttributes
        | choices = choices
        , choiceView = Just choiceView
        , onSelect = Just onSelect
        , searchQuery = Just searchQuery
        , focused = focused
    }


{-| Set the selected element.
-}
selectedElement : { b | selectedElement : Maybe Int, elements : List a } -> FormAttributes (AutocompleteAttributes a msg) msg -> FormAttributes (AutocompleteAttributes a msg) msg
selectedElement autocompleteModel formAttributes =
    { formAttributes | selectedElement = FormBuilder.Fields.Autocomplete.selectedElement autocompleteModel }
