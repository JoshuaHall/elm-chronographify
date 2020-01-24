module Tests exposing (suite)

import Expect
import Fuzz exposing (intRange)
import Main exposing (reverseRange)
import Test exposing (Test, describe, fuzz2)


simpleReverseRange : Int -> Int -> List Int
simpleReverseRange hi lo =
    List.reverse (List.range lo hi)


suite : Test
suite =
    describe "reverseRange"
        [ fuzz2 (intRange 0 5000) (intRange 0 5000) "reverseRange should be equal to simpleReverseRange" <|
            \first second ->
                if first >= second then
                    reverseRange first second
                        |> Expect.equalLists (simpleReverseRange first second)

                else
                    reverseRange first second
                        |> List.isEmpty
                        |> Expect.true "Should be empty if the hi is less than the low"
        ]
