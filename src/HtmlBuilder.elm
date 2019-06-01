module HtmlBuilder exposing
  (Nodes, newNode, addAttrs, addChild, completeNode)


import Html exposing (..)
import Html.Attributes exposing (..)


type Nodes msg =
  Nodes (List (Context msg))


type alias Context msg =
  { node: List (Attribute msg) -> List (Html msg) -> Html msg
  , attrs: List (Attribute msg)
  , children: List (Html msg)
  }


newNode: (List (Attribute msg) -> List (Html msg) -> Html msg) -> Nodes msg -> Nodes msg
newNode node (Nodes nodes) =
  Nodes <| Context node [] [] :: nodes


addAttrs: List (Attribute msg) -> Nodes msg -> Nodes msg
addAttrs attrs (Nodes nodes) =
  case nodes of
    ctx :: tail ->
      Nodes <| { ctx | attrs = attrs } :: tail

    _ -> Nodes nodes


addChild: Html msg -> Nodes msg -> Nodes msg
addChild node (Nodes nodes) =
  case nodes of
    ctx :: tail ->
      Nodes <| { ctx | children = node :: ctx.children } :: tail

    _ -> Nodes nodes


completeNode: Nodes msg -> (Maybe (Html msg), Nodes msg)
completeNode (Nodes nodes) =
  case nodes of
    { node, attrs, children } :: tail ->
      node attrs (List.reverse children) |>
      (\n -> (Just n, addChild n <| Nodes tail))

    [] -> (Nothing, Nodes [])
