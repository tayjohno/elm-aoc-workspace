module List.Mine exposing (combinations)

import List.Extra as List


combinations : Int -> List a -> List (List a)
combinations int aList =
    if int == 1 then
        aList |> List.map List.singleton

    else
        aList
            |> List.selectSplit
            |> List.map
                (\( _, val, newList ) ->
                    combinations (int - 1) newList
                        |> List.map (\item -> val :: item)
                )
            |> List.concat
