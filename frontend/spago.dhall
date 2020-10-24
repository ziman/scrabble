{ name = "pixit"
, dependencies =
  [ "console"
  , "effect"
  , "psci-support"
  , "react-basic"
  , "affjax"
  , "argonaut-codecs"
  , "http-methods"
  , "web-socket"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
