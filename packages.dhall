
let upstream =
  https://github.com/purescript/package-sets/releases/download/psc-0.15.8-20230517/packages.dhall
    sha256:8b94a0cd7f86589a6bd06d48cb9a61d69b66a94b668657b2f10c8b14c16e028c

in upstream


with marked =
  { repo = "ssh://git@github.com/thought2/purescript-marked.git"
  , version = "e9505e2438fdf5dd975fcac96a759189b9574563"
  , dependencies = [ "console", "dts", "effect", "either", "integers", "labeled-data", "maybe", "newtype", "nullable", "prelude", "ts-bridge", "unsafe-coerce", "untagged-union", "variant", "variant-encodings" ]
  }


with data-mvc =
  { repo = "https://github.com/thought2/purescript-data-mvc.git"
  , version = "1a8d7e1ab72feaa622e2ef0fd84facfb0bba9f44"
  , dependencies = [ "heterogeneous", "maybe", "newtype", "prelude", "record", "variant" ]
  }


with ts-bridge =
  { repo = "https://github.com/thought2/purescript-ts-bridge.git"
  , version = "ae0fc665132edcb5d15454906e4750f51c826ae2"
  , dependencies = [ "aff", "aff-promise", "arrays", "console", "dts", "effect", "either", "foldable-traversable", "foreign-object", "literals", "maybe", "newtype", "node-buffer", "node-fs", "node-fs-aff", "node-path", "node-process", "nullable", "optparse", "ordered-collections", "ordered-set", "partial", "prelude", "record", "safe-coerce", "strings", "transformers", "tuples", "typelevel-prelude", "unsafe-coerce", "untagged-union", "variant", "variant-encodings" ]
  }


with virtual-dom-react-basic =
  { repo = "https://github.com/thought2/purescript-virtual-dom-react-basic.git"
  , version = "2efb8a3bb000fe53cfc76ebb770f5ffed0565e1d"
  , dependencies = [ "console", "effect", "foreign", "foreign-object", "maybe", "prelude", "react-basic", "react-basic-dom", "strings", "tuples", "unsafe-coerce", "virtual-dom" ]
  }


with virtual-dom-halogen =
  { repo = "https://github.com/thought2/purescript-virtual-dom-halogen.git"
  , version = "e8ac7a6a67f4fd50ddb62c6c0bf37070803843a5"
  , dependencies = [ "aff", "arrays", "bifunctors", "effect", "foreign", "halogen", "prelude", "safe-coerce", "strings", "tuples", "unsafe-coerce", "virtual-dom", "web-events", "web-html" ]
  }


with virtual-dom =
  { repo = "https://github.com/thought2/purescript-virtual-dom.git"
  , version = "2e9a410718444e2adfb62b05f6144a1fca0d20f5"
  , dependencies = [ "either", "foreign", "maybe", "prelude", "strings", "transformers", "tuples", "variant" ]
  }


with labeled-data =
  { repo = "https://github.com/thought2/purescript-labeled-data.git"
  , version = "02647c4a175d73fad22d8ecba4e8c618744d0404"
  , dependencies = [ "aff", "effect", "either", "maybe", "prelude", "record", "tuples", "type-equality", "unsafe-coerce", "variant" ]
  }


with variant-encodings =
  { repo = "https://github.com/thought2/purescript-variant-encodings.git"
  , version = "ec064edfd885f4efd0eb924ae4e26752ccf975c2"
  , dependencies = [ "prelude", "unsafe-coerce", "variant" ]
  }


with dts =
  { repo = "https://github.com/thought2/purescript-dts.git"
  , version = "0f71dd4a8ea966835eee31684e7b004c683e4f72"
  , dependencies = [ "arrays", "console", "effect", "maybe", "newtype", "ordered-collections", "ordered-set", "prelude", "tuples" ]
  }


with interactive-data-core =
  { repo = "ssh://git@github.com/thought2/purescript-interactive-data-core.git"
  , version = "fa09621fa0e6becc3a0582eb27db4a2cd13f7d75"
  , dependencies = [ "maybe", "prelude" ]
  }


with data-functions =
  { repo = "ssh://git@github.com/thought2/purescript-data-functions.git"
  , version = "8707ec9f38faf43e5fbed190338580980be00557"
  , dependencies = [ "heterogeneous", "prelude" ]
  }

