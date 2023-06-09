let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.8-20230517/packages.dhall
        sha256:8b94a0cd7f86589a6bd06d48cb9a61d69b66a94b668657b2f10c8b14c16e028c

in  upstream

  with virtual-dom =
      { dependencies =
          [ "either"
          , "foreign"
          , "maybe"
          , "prelude"
          , "strings"
          , "transformers"
          , "tuples"
          , "variant"
          ]
      , repo =
          "https://github.com/thought2/purescript-virtual-dom.git"
      , version =
          "main"
      }

  with virtual-dom-halogen =
      { dependencies =
          [ "aff"
          , "arrays"
          , "bifunctors"
          , "effect"
          , "foreign"
          , "halogen"
          , "prelude"
          , "safe-coerce"
          , "strings"
          , "tuples"
          , "unsafe-coerce"
          , "virtual-dom"
          , "web-events"
          , "web-html"
          ]
      , repo =
          "https://github.com/thought2/purescript-virtual-dom-halogen.git"
      , version =
          "main"
      }