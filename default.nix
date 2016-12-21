{ mkDerivation, async, base, containers, deepseq, hslogger, HUnit
, QuickCheck, random, stdenv, stm, test-framework
, test-framework-hunit, test-framework-quickcheck2, time
, transformers
}:
mkDerivation {
  pname = "longrun";
  version = "0.6.0.0";
  src = ./.;
  libraryHaskellDepends = [
    async base containers deepseq hslogger random stm time transformers
  ];
  testHaskellDepends = [
    base HUnit QuickCheck test-framework test-framework-hunit
    test-framework-quickcheck2
  ];
  homepage = "https://github.com/zoranbosnjak/longrun";
  description = "Long running process support routines";
  license = stdenv.lib.licenses.gpl3;
}
