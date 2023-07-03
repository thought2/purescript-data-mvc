{ name = "interactive-data-core"
, dependencies =
  [ "data-mvc"
  , "effect"
  , "either"
  , "heterogeneous"
  , "identity"
  , "newtype"
  , "prelude"
  , "profunctor"
  , "record"
  , "record-extra"
  , "typelevel-prelude"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
