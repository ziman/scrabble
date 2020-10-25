{ name = "scrabble"
, dependencies =
  [ "console"
  , "effect"
  , "psci-support"
  , "affjax"
  , "argonaut-codecs"
  , "http-methods"
  , "websocket-moderate"
  , "halogen"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
