
let upstream =
  https://github.com/purescript/package-sets/releases/download/psc-0.15.8-20230517/packages.dhall
    sha256:8b94a0cd7f86589a6bd06d48cb9a61d69b66a94b668657b2f10c8b14c16e028c

in upstream


with mvc =
  { repo = "https://github.com/thought2/purescript-mvc.git"
  , version = "884cf062846de0cc11a505816d73ba63204cedcc"
  , dependencies = [ "heterogeneous", "maybe", "newtype", "prelude", "record", "variant" ] : List Text
  }

