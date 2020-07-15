module Test exposing (..)


test1 : Int
test1 =
    1 + 2


test2 : Int
test2 =
    1 * 2


test3 : Int
test3 =
    if False then
        1

    else
        0


test4 : Int
test4 =
    id 42


id : Int -> Int
id i =
    i


main : Int
main =
    test1 + test2 + test3 + test4
