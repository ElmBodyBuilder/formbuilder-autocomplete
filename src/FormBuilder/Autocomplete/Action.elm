module FormBuilder.Autocomplete.Action
    exposing
        ( changeSelectedElement
        , selectElement
        , deselectElement
        , selectedElement
        )

{-| Create a new autocomplete form field.

# Model Modifier
@docs changeSelectedElement
@docs selectElement
@docs deselectElement
@docs selectedElement
-}

import List.Extra


{-| Changes the selected element in the autocomplete.
-}
changeSelectedElement : { b | selectedElement : Maybe Int, elements : List a } -> Int -> { b | selectedElement : Maybe Int, elements : List a }
changeSelectedElement autocompleteModel offset =
    let
        elementsSize =
            autocompleteModel.elements |> List.length
    in
        { autocompleteModel
            | selectedElement =
                if elementsSize == 0 then
                    Nothing
                else
                    Just
                        (case autocompleteModel.selectedElement of
                            Nothing ->
                                if offset == 1 then
                                    0
                                else
                                    elementsSize - 1

                            Just selection ->
                                if selection == 0 && offset == -1 then
                                    0
                                else if selection == (elementsSize - 1) && offset == 1 then
                                    elementsSize - 1
                                else
                                    selection + offset
                        )
        }


{-| Selects an element in the elements, and puts it as selectedElement in autocomplete.
-}
selectElement :
    a
    -> { b | selectedElement : Maybe Int, elements : List a }
    -> { b | selectedElement : Maybe Int, elements : List a }
selectElement element autocompleteModel =
    { autocompleteModel | selectedElement = List.Extra.elemIndex element autocompleteModel.elements }


{-| Removes the selected element from the autocomplete.
-}
deselectElement :
    { b | selectedElement : Maybe Int }
    -> { b | selectedElement : Maybe Int }
deselectElement autocompleteModel =
    { autocompleteModel | selectedElement = Nothing }


{-| Returns the selected element from the autocomplete model.
-}
selectedElement :
    { b | elements : List a, selectedElement : Maybe Int }
    -> Maybe a
selectedElement { selectedElement, elements } =
    case selectedElement of
        Nothing ->
            Nothing

        Just index ->
            elements |> List.Extra.getAt index
