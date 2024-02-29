{ mkDerivation, aeson, async, base, bytestring, cassava, containers
, cryptohash-sha256, crypton, debug-trace-var, directory, filepath
, http-conduit, lib, memory, random, scientific, text, time
, unordered-containers, vector
}:
mkDerivation {
  pname = "pelotero-engine";
  version = "0.0.7.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async base bytestring cassava containers cryptohash-sha256
    crypton debug-trace-var directory filepath http-conduit memory
    random scientific text time unordered-containers vector
  ];
  license = "unknown";
}
