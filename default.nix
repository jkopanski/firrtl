{ mkDerivation, base, formatting, hspec, hspec-megaparsec
, megaparsec, monads-tf, optparse-applicative, parsers
, prettyprinter, prettyprinter-ansi-terminal, recursion-schemes
, singletons, stdenv, tasty, tasty-hspec, tasty-hunit, text
, transformers, unordered-containers
}:
mkDerivation {
  pname = "firrtl";
  version = "0.0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base formatting megaparsec monads-tf parsers prettyprinter
    prettyprinter-ansi-terminal recursion-schemes singletons text
    transformers unordered-containers
  ];
  executableHaskellDepends = [
    base formatting megaparsec monads-tf optparse-applicative parsers
    prettyprinter prettyprinter-ansi-terminal recursion-schemes
    singletons text transformers unordered-containers
  ];
  testHaskellDepends = [
    base formatting hspec hspec-megaparsec megaparsec monads-tf parsers
    prettyprinter prettyprinter-ansi-terminal recursion-schemes
    singletons tasty tasty-hspec tasty-hunit text transformers
    unordered-containers
  ];
  homepage = "https://github.com/jkopanski/firrtl#readme";
  description = "FIRRTL haskell implementation";
  license = stdenv.lib.licenses.bsd3;
}
