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
{ name = "my-project"
, dependencies =
  [ "aff"
  , "aff-coroutines"
  , "affjax"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "b64"
  , "bifunctors"
  , "console"
  , "control"
  , "coroutines"
  , "datetime"
  , "effect"
  , "either"
  , "form-urlencoded"
  , "http-methods"
  , "integers"
  , "lists"
  , "maybe"
  , "media-types"
  , "prelude"
  , "refs"
  , "safely"
  , "spec"
  , "spec-discovery"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
