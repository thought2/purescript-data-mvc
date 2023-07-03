{ name = "interactive-data-core"
, dependencies =
  [ "data-mvc"
  , "either"
  , "heterogeneous"
  , "identity"
  , "maybe"
  , "newtype"
  , "prelude"
  , "profunctor"
  , "record"
  , "record-extra"
  , "typelevel-prelude"
  , "unsafe-coerce"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
