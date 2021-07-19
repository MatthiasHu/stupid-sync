{ lib, mkDerivation, base, bytestring, containers, directory, filepath
, websockets
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
  license = lib.licenses.gpl3;
}
