module Deal exposing (..)

import Set exposing (Set)



-- 6, 5, 3, 2, 2, 1, 1


run : String
run =
    let
        cases =
            Set.fromList
                [ 0.5
                , 10000
                , 40000
                ]

        point =
            1 - toFloat (Set.size cases) / 21

        offer =
            bankOffer point cases
    in
    "Bank offer: " ++ String.fromFloat offer


bankOffer : Float -> Set Float -> Float
bankOffer proportion cases =
    pessimisticExpectation proportion <|
        expectations <|
            List.map setValue <|
                choices 2 cases


all : List Float
all =
    [ 0.5
    , 1
    , 5
    , 10
    , 25
    , 75
    , 100
    , 250
    , 500
    , 750
    , 1000
    , 2500
    , 5000
    , 7500
    , 10000
    , 20000
    , 30000
    , 40000
    , 50000
    , 75000
    , 100000
    ]


choices : Int -> Set comparable -> List (Set comparable)
choices n cases =
    if n < 1 then
        [ Set.empty ]

    else
        List.concatMap
            (\i ->
                List.map (Set.insert i) <|
                    choices (n - 1) (Set.remove i cases)
            )
        <|
            Set.toList cases


dropSingle : Set comparable -> List (Set comparable)
dropSingle cases =
    let
        items =
            Set.toList cases
    in
    List.map (\i -> Set.remove i cases) items


drop : Int -> Set comparable -> List (Set comparable)
drop n cases =
    case n of
        0 ->
            [ cases ]

        1 ->
            dropSingle cases

        _ ->
            List.concatMap (drop (n - 1)) (dropSingle cases)


pessimisticExpectation : Float -> List Float -> Float
pessimisticExpectation proportion values =
    let
        optionValues =
            List.sort values

        length =
            toFloat <| List.length optionValues

        lowIndex =
            floor (proportion * length)

        offset =
            proportion * length - toFloat lowIndex
    in
    case ( lowIndex, List.drop (lowIndex - 1) values ) of
        ( 0, lower :: _ ) ->
            lower

        ( _, lower :: upper :: _ ) ->
            lower + (upper - lower) * offset

        ( _, [ top ] ) ->
            top

        ( _, [] ) ->
            0


expectations : List Float -> List Float
expectations ll =
    let
        step x prev =
            case prev of
                ( n, xPrev ) :: _ ->
                    ( n + 1, xPrev + x ) :: prev

                [] ->
                    [ ( 1, x ) ]
    in
    List.reverse <| List.map (\( n, x ) -> x / toFloat n) <| List.foldl step [] <| List.sort ll


setValue : Set Float -> Float
setValue set =
    expectedValue (Set.toList set)


expectedValue : List Float -> Float
expectedValue list =
    let
        ( num, total ) =
            List.foldl (\x ( c, v ) -> ( c + 1, v + x )) ( 0, 0 ) list
    in
    total / num
