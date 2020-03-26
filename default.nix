{ mkDerivation, base, bytestring, containers, directory, filepath
, stdenv, websockets
}:
mkDerivation {
  pname = "stupid-sync";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring containers directory filepath websockets
  ];
  description = "stupid-sync server for synchronization over WebSockets";
  license = stdenv.lib.licenses.gpl3;
}
