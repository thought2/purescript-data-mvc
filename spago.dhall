{ name = "interactive-data-core"
, dependencies =
  [ "arrays"
  , "bifunctors"
  , "data-mvc"
  , "either"
  , "heterogeneous"
  , "maybe"
  , "newtype"
  , "prelude"
  , "profunctor"
  , "record"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
