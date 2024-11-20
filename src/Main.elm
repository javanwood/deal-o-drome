port module Main exposing (..)

import Deal
import Platform


main : Program () () ()
main =
    Platform.worker
        { init = \() -> ( (), log Deal.run )
        , update = \() () -> ( (), Cmd.none )
        , subscriptions = \() -> Sub.none
        }


port log : String -> Cmd msg
