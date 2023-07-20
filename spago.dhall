{ name = "interactive-data-core"
, dependencies =
  [ "bifunctors"
  , "data-mvc"
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
