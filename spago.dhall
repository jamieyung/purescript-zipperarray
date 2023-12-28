{ name = "zipperarray"
, dependencies =
  [ "arrays"
  , "control"
  , "foldable-traversable"
  , "maybe"
  , "naturals"
  , "prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
