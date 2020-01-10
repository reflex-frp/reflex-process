{ mkDerivation, base, bytestring, containers, data-default, process
, reflex, reflex-vty, stdenv, unix, vty
, markdown-unlit
}:
mkDerivation {
  pname = "reflex-process";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring data-default process reflex unix
  ];
  executableHaskellDepends = [
    base containers data-default process reflex reflex-vty vty
  ];
  executableToolDepends = [
    markdown-unlit
  ];
  description = "reflex-frp interface for running shell commands";
  license = stdenv.lib.licenses.bsd3;
}
