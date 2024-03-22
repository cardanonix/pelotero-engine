{ mkDerivation, aeson, async, base, bytestring, cassava, containers
, cryptohash-sha256, crypton, debug-trace-var, directory, filepath
, graphviz, http-conduit, lib, memory, mtl, process, random
, scientific, text, time, unordered-containers, vector
}:
mkDerivation {
  pname = "pelotero-engine";
  version = "0.0.8.2";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async base bytestring cassava containers cryptohash-sha256
    crypton debug-trace-var directory filepath graphviz http-conduit
    memory mtl process random scientific text time unordered-containers
    vector
  ];
  license = "unknown";
}
