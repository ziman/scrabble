{ name = "scrabble"
, dependencies =
  [ "console"
  , "effect"
  , "psci-support"
  , "argonaut"
  , "argonaut-codecs"
  , "websocket-moderate"
  , "react-basic"
  , "react-basic-classic"
  , "react-basic-dom"
  , "media-types"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
