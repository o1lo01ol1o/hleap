{
  mkDerivation, stdenv
, aeson, base, containers, data-default, mtl, text, unordered-containers, websockets
}:

mkDerivation {
  pname = "hleap";
  version = "0.2.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base containers data-default mtl text unordered-containers websockets
  ];
  executableHaskellDepends = [
  ];
  homepage = "https://bitbucket.org/functionally/hleap";
  description = "Web Socket interface to Leap Motion controller";
  license = stdenv.lib.licenses.mit;
}
