module Arduino exposing (..)


type Output
    = Output Bool


digitalPin : Bool -> Output
digitalPin b =
    Output b


type Arduino a
    = Arduino { init : a, output : a -> Output }


arduino : { init : a, output : a -> Output } -> Arduino a
arduino arg =
    Arduino arg
