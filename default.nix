{ mkDerivation, async, base, containers, deepseq, hslogger, random
, stdenv, stm, time, transformers
}:
mkDerivation {
  pname = "longrun";
  version = "0.6.0.0";
  src = ./.;
  libraryHaskellDepends = [
    async base containers deepseq hslogger random stm time transformers
  ];
  homepage = "https://github.com/zoranbosnjak/longrun";
  description = "Long running process support routines";
  license = stdenv.lib.licenses.gpl3;
}
