module DealTests exposing (tests)

import Deal
import Expect
import Set
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "Deal"
        [ describe "expectedValue"
            [ test "2" <|
                \_ ->
                    within (Deal.expectedValue [ 1, 2, 3 ]) 2
            , test "1.5" <|
                \_ ->
                    within (Deal.expectedValue [ 1, 2 ]) 1.5
            ]
        , describe "drop"
            [ test "simple" <|
                \_ ->
                    Expect.equal (Deal.drop 2 (Set.fromList [ 1, 2, 3 ]))
                        [ Set.fromList [ 3 ], Set.fromList [ 2 ], Set.fromList [ 3 ], Set.fromList [ 1 ], Set.fromList [ 2 ], Set.fromList [ 1 ] ]
            ]
        , describe "pessimisticExpectation"
            [ test "simple" <|
                \_ ->
                    within (Deal.pessimisticExpectation 0.5 [ 1, 2, 3, 4 ]) 1.5
            ]
        , describe "choices"
            [ test "goodChoice" <|
                \_ ->
                    Expect.equal (Deal.choices 2 (Set.fromList [ 1, 2, 3 ]))
                        [ Set.fromList [ 1, 2 ], Set.fromList [ 1, 3 ], Set.fromList [ 1, 2 ], Set.fromList [ 2, 3 ], Set.fromList [ 1, 3 ], Set.fromList [ 2, 3 ] ]
            ]
        ]


within : Float -> Float -> Expect.Expectation
within =
    Expect.within (Expect.Absolute 0.001)
