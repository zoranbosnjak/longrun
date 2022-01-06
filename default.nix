{ mkDerivation, async, base, containers, deepseq, hslogger, HUnit
, lib, mtl, QuickCheck, random, stm, test-framework
, test-framework-hunit, test-framework-quickcheck2, time
, transformers
}:
mkDerivation {
  pname = "longrun";
  version = "0.12.1";
  src = ./.;
  libraryHaskellDepends = [
    async base containers deepseq hslogger mtl random stm time
    transformers
  ];
  testHaskellDepends = [
    base containers hslogger HUnit mtl QuickCheck random stm
    test-framework test-framework-hunit test-framework-quickcheck2
    transformers
  ];
  homepage = "https://github.com/zoranbosnjak/longrun";
  description = "Long running process support routines";
  license = lib.licenses.gpl3;
}
