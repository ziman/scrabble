{ name = "scrabble"
, dependencies =
  [ "console"
  , "effect"
  , "psci-support"
  , "affjax"
  , "argonaut-codecs"
  , "http-methods"
  , "websocket-moderate"
  , "react-basic"
  , "react-basic-classic"
  , "react-basic-dom"
  , "matrices"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
