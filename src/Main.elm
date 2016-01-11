import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue,onClick)
import Signal exposing (Address)
import StartApp.Simple as StartApp
import String
import List
import Http

main = StartApp.start { model = "", view = view, update = update }

type alias Model = 
    { animals : List Animal
    , searched : List Animal
    , viewedAnimals : List Animal 
    }
    
type alias Animal = 
    { name : String
    , desc : String
    , kind : String
    }
    
makeAnimal : String -> String -> String -> Animal
makeAnimal name desc kind = 
    { name = name
    , desc = desc
    , kind = kind
    }
    
emptyModel : Model
emptyModel =
    { animals = []
    , searched = []
    , viewedAnimals = []
    }

type Action 
    = NoOp
    | Search String
    | Click Animal


--update

update : Action -> Model -> Model
update action model =
    case action of
      NoOp -> model
      Search str -> {model | 
        searched = filter (\n -> .name |> (String.contains str)) model.animals} 
      Click animal -> {model | 
        model.viewedAnimals = List.append model.viewedAnimals [animal]}

--view

view : Address Action -> Model -> Html
view address model =
  div []
    [ input
        [ placeholder "Text to reverse"
        , value 
        , on "input" targetValue (Signal.message address)
        , myStyle
        ]
        []
    , ul [ myStyle ] 
         (List.map (\t ->  li [onClick (Signal.message address)] [text t]) <| List.filter (String.contains string) listdata)
    ]


myStyle : Attribute
myStyle =
  style
    [ ("width", "40%")
    , ("height", "2em")
    , ("padding", "5px 5px 5px 5px")
    , ("font-size", "1em")
    ]

myChangingStyle =
    case mode of
      

listdata : List String
listdata = [
    "chien",
    "chat",
    "canard",
    "ours"]
    
testFun : String -> String
testFun str = String.reverse str 