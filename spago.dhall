let name = "zipperarray"

in  { name
    , license = "MIT"
    , repository = "https://github.com/jamieyung/purescript-${name}.git"
    , dependencies =
      [ "arrays"
      , "control"
      , "foldable-traversable"
      , "maybe"
      , "naturals"
      , "prelude"
      ]
    , packages = ./packages.dhall
    , sources = [ "src/**/*.purs" ]
    }
