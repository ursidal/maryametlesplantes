import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue,onClick)
import Signal exposing (Address)
import StartApp.Simple as StartApp
import String
import List
import Http
import Markdown

main = StartApp.start { model = model, view = view, update = update }

type alias Model = 
    { animals : List Animal
    , searched : List Animal
    , viewedAnimals : List Animal 
    , field : String
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
    , field = ""
    }

addAnimal : Model -> String -> String -> String -> Model
addAnimal model name desc kind =
    let newAnimal = makeAnimal name desc kind
    in
      {model | animals = List.append model.animals [newAnimal]}

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
        searched = List.filter (\n -> n.name |> (String.contains str)) model.animals} 
      Click animal -> {model | 
        viewedAnimals = List.append model.viewedAnimals [animal]}

--view

view : Address Action -> Model -> Html
view address model =
  div 
    [ id "container"
    , style [("width","100%"),("height","100%")]
    ]
    [ div 
      [style [("width","50%"),("display","inline-block"),("background","#eee")]] 
      [ input
          [ placeholder "Search"
          , on "input" targetValue (Signal.message address << Search)
          , myStyle
          ]
          []
      , ul [ myStyle ] 
         (List.map (\t ->  li 
           [ onClick address (Click t)] [text t.name]) model.searched)
      ]
    , div 
      [style [("width","50%"),("display","inline-block")]]
      [ ul []
          ( List.map 
            (\t -> li [] 
              [Markdown.toHtml (String.join "" ["**nom:** ", t.name,", **genre:** ",t.kind,", **description:** ",t.desc])])
              model.viewedAnimals
          )
      ]
    ]
    

myStyle : Attribute
myStyle =
  style
    [ ("width", "80%")
    , ("height", "2em")
    , ("padding", "5px 5px 5px 5px")
    , ("font-size", "1em")
    ]
    
leftPaneStyle : Attribute
leftPaneStyle =
    style
      [ ("float","left")] 


model : Model
model = 
    List.foldl (\(a,b,c) acc -> addAnimal acc a b c) emptyModel 
      [("chien","il fait wouaf","canidé")
      ,("chat","il fait miaou","félin")
      ,("canard","il fait coin coin","oiseau")
      ,("ours","il fait grr","ursidé")
      ]
    

listdata : List String
listdata = [
    "chien",
    "chat",
    "canard",
    "ours"]
    
testFun : String -> String
testFun str = String.reverse str 
