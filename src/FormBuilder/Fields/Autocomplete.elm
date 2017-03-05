module FormBuilder.Fields.Autocomplete exposing (..)

{-| Getters and setter to navigate through the Autocomplation list.

    # Getter
    @docs selectedElement

    # Setters
    @docs changeSelectedElement, selectElement, deselectElement
-}

import List.Extra


{-| Used to move up or down in the autocompletion list.
    Offset shoulde be '-1' to move up or '1' to move down.
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


{-| Selects a specific element within the autocompletion list.
-}
selectElement : a -> { b | selectedElement : Maybe Int, elements : List a } -> { b | selectedElement : Maybe Int, elements : List a }
selectElement element autocompleteModel =
    { autocompleteModel | selectedElement = List.Extra.elemIndex element autocompleteModel.elements }


{-| Returns the selected element in the list.
-}
selectedElement : { b | selectedElement : Maybe Int, elements : List a } -> Maybe a
selectedElement autocompleteModel =
    case autocompleteModel.selectedElement of
        Nothing ->
            Nothing

        Just index ->
            autocompleteModel.elements |> List.Extra.getAt index


{-| Unselects the selected element. No more selected element.
-}
deselectElement : { b | selectedElement : Maybe Int } -> { b | selectedElement : Maybe Int }
deselectElement autocompleteModel =
    { autocompleteModel | selectedElement = Nothing }
