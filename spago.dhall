{ name = "interactive-data-core"
, dependencies =
  [ "data-mvc"
  , "either"
  , "heterogeneous"
  , "identity"
  , "maybe"
  , "newtype"
  , "prelude"
  , "record"
  , "record-extra"
  , "typelevel-prelude"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
