{ name = "interactive-data-core"
, dependencies =
  [ "arrays"
  , "bifunctors"
  , "data-mvc"
  , "effect"
  , "either"
  , "heterogeneous"
  , "identity"
  , "maybe"
  , "newtype"
  , "prelude"
  , "profunctor"
  , "record"
  , "typelevel-prelude"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
