{ name = "pixit"
, dependencies =
  [ "console"
  , "effect"
  , "psci-support"
  , "react-basic"
  , "affjax"
  , "argonaut-codecs"
  , "http-methods"
  , "websocket-moderate"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
