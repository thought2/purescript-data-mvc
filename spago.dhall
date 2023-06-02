{ name = "interactive-data-core"
, dependencies =
  [ "data-mvc", "maybe", "prelude", "record-extra", "typelevel-prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
