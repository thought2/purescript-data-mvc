{ name = "interactive-data-core"
, dependencies =
  [ "data-mvc"
  , "effect"
  , "either"
  , "foreign-object"
  , "heterogeneous"
  , "identity"
  , "maybe"
  , "newtype"
  , "prelude"
  , "profunctor"
  , "record"
  , "record-extra"
  , "strings"
  , "tuples"
  , "typelevel-prelude"
  , "unsafe-coerce"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
