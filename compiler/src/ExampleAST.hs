module ExampleAST
  ( elmModule
  ) where

import AST.Source
  ( Alias(..)
  , Docs(..)
  , Effects(..)
  , Exposed(..)
  , Exposing(..)
  , Expr
  , Expr_(..)
  , Import(..)
  , Module(..)
  , Pattern
  , Pattern_(..)
  , Privacy(..)
  , Type
  , Type_(..)
  , Union(..)
  , Value(..)
  , VarType(..)
  )

import Data.Name (Name)
import Data.Utf8 (fromChars)
import Reporting.Annotation (Located(..), Position(..), Region(..))

{-
-- Elm program

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

type alias Model = Int

initialModel : Model
initialModel = 0

type Msg = Increment | Decrement
update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1

view: Model -> Html Msg
view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (String.fromInt model) ]
    , button [ onClick Increment ] [ text "+" ]
    ]

main: Program () Model Msg
main =
  Browser.sandbox { init = initialModel, update = update, view = view }
-}
dummyReg :: Region
dummyReg = Region (Position 0 0) (Position 1 1)

at :: a -> Located a
at x = At dummyReg x

var :: Name -> Expr_
var name = Var LowVar name

initialModel :: Value
initialModel =
  Value
    (at (fromChars "initialModel"))
    []
    (at (Int 0))
    (Just (at (TType dummyReg (fromChars "Int") [])))

modelType :: Type
modelType = at (TType dummyReg (fromChars "Model") [])

msgType :: Type
msgType = at (TType dummyReg (fromChars "Msg") [])

update :: Value
update =
  Value
    (at (fromChars "update"))
    [at (PVar (fromChars "msg")), at (PVar (fromChars "model"))]
    (At
       dummyReg
       (Case
          (at (var (fromChars "msg")))
          [ ( at (PCtor dummyReg (fromChars "Increment") [])
            , At
                dummyReg
                (Call
                   (at (Op (fromChars "+")))
                   [at (var (fromChars "model")), at (Int 1)]))
          , ( at (PCtor dummyReg (fromChars "Decrement") [])
            , At
                dummyReg
                (Call
                   (at (Op (fromChars "-")))
                   [at (var (fromChars "model")), at (Int 1)]))
          ]))
    (Just (at (TLambda msgType (at (TLambda modelType modelType)))))

view :: Value
view =
  Value
    (at (fromChars "view"))
    [at (PVar (fromChars "model"))]
    (at
       (Call
          (at (var (fromChars "div")))
          [ at (List [])
          , at
              (List
                 [ at
                     (Call
                        (at (var (fromChars "button")))
                        [ at
                            (List
                               [ at
                                   (Call
                                      (at (var (fromChars "onclick")))
                                      [ at
                                          (Call
                                             (at (var (fromChars "Decrement")))
                                             [])
                                      ])
                               ])
                        , at
                            (Call
                               (at (var (fromChars "text")))
                               [at (Str (fromChars "-"))])
                        ])
                 , etc
                 ])
          ]))
    (Just
       (at
          (TLambda modelType (at (TType dummyReg (fromChars "HTML") [msgType])))))

etc :: a
etc = undefined

main :: Value
main =
  Value
    (at (fromChars "main"))
    []
    (at
       (Call
          (at (VarQual LowVar (fromChars "Browser") (fromChars "sandbox")))
          [ at
              (Record
                 [ (at (fromChars "init"), at (var (fromChars "initialModel")))
                 , (at (fromChars "update"), at (var (fromChars "update")))
                 , (at (fromChars "view"), at (var (fromChars "view")))
                 ])
          ]))
    (Just
       (at
          (TType
             dummyReg
             (fromChars "Program")
             [at (TType dummyReg (fromChars "()") []), modelType, msgType])))

elmModule :: Module
elmModule =
  Module
    { _name = Nothing
    , _exports = at Open
    , _docs = NoDocs dummyReg
    , _imports =
        [ Import (at (fromChars "Browser")) Nothing Open
        , Import
            (at (fromChars "Html"))
            Nothing
            (Explicit
               [ Upper (at (fromChars "Html")) Private
               , Lower (at (fromChars "button"))
               , Lower (at (fromChars "div"))
               , Lower (at (fromChars "text"))
               ])
        ]
    , _values = [at initialModel, at view, at update]
    , _unions =
        [ at
            (Union
               (at (fromChars "Msg"))
               []
               [ (at (fromChars "Increment"), [])
               , (at (fromChars "Decrement"), [])
               ])
        ]
    , _aliases =
        [ at
            (Alias
               (at (fromChars "Model"))
               []
               (at (TType dummyReg (fromChars "Int") [])))
        ]
    , _binops = []
    , _effects = NoEffects
    }
