module Route exposing
  (Route)


import JsonModel as JM


type alias Route page =
  { path: List String
  , params: JM.SearchParams
  , page: page
  }