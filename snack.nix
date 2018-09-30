let
deps = [
    "GLFW-b"
    "JuicyPixels"
    "MissingH"
    "MonadRandom"
    "OpenGL"
    "OpenGLRaw"
    "QuickCheck"
    "aeson"
    "aeson-pretty"
    "base"
    "base-unicode-symbols"
    "binary"
    "bytestring"
    "cairo"
    "clock"
    "containers"
    "dependent-sum"
    "directory"
    "dlist"
    "exceptions"
    "extra"
    "filepath"
    "free"
    "freer-simple"
    "fsnotify"
    "ghc-prim"
    "gi-cairo"
    "gi-gobject"
    "gi-pango"
    "gi-pangocairo"
    "hashable"
    "haskell-gi-base"
    "hedgehog"
    "hspec"
    "hxt"
    "lambdacube-compiler"
    "lambdacube-gl"
    "lambdacube-ir"
    "lens"
    "linear"
    "lub"
    "metamorphic"
    "monadplus"
    "mono-traversable"
    "monoidal-containers"
    "mtl"
    "parsers"
    "pretty"
    "pretty-show"
    "primitive"
    "profunctors"
    "proteaaudio"
    "random"
    "ref-tf"
    "reflex"
    "semigroupoids"
    "semigroups"
    "singletons"
    "spool"
    "stm"
    "tasty"
    "tasty-expected-failure"
    "tasty-hedgehog"
    "tasty-hspec"
    "tasty-hunit"
    "tasty-quickcheck"
    "tasty-smallcheck"
    "template-haskell"
    "text"
    "text-format"
    "text-lens"
    "text-zipper"
    "these"
    "time"
    "transformers"
    "trifecta"
    "type-map"
    "unordered-containers"
    "vect"
    "vector"
    "wl-pprint-extras"
    "wl-pprint-text"
    ];
lib = {
      src = ./src;
      dependencies = deps;
      extensions = [ "OverloadedStrings"];
    };
in {
    main = "Main";
    src = ./.;
    packages = [ lib ];
    dependencies = [ "wreq" "lens" ];
}