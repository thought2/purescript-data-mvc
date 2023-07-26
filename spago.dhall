{ name = "interactive-data-core"
, dependencies =
  [ "arrays"
  , "bifunctors"
  , "mvc"
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
