{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "cmus-web"
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-web"
  , "console"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "halogen"
  , "integers"
  , "maybe"
  , "prelude"
  , "profunctor"
  , "tuples"
  , "web-html"
  ]
, packages = ./frontend/packages.dhall
, sources = [ "./frontend/src/**/*.purs", "./frontend/test/**/*.purs" ]
}
