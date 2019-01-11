{ mkDerivation, base, checkers, filepath, hpython, lens, QuickCheck
, stdenv, tasty, tasty-hunit, tasty-quickcheck, text
}:
mkDerivation {
  pname = "python-ident-rename";
  version = "0.1.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base filepath hpython lens text ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base checkers lens QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/tonymorris/python-ident-rename";
  description = "Python identifer rename";
  license = stdenv.lib.licenses.bsd3;
}
