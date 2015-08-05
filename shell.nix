with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, attoparsec, base, bytestring, containers
             , directory, double-conversion, filepath, HDBC, HDBC-sqlite3
             , lhef-tools, optparse-applicative, process, statistics, stdenv
             , transformers, vector
             }:
             mkDerivation {
               pname = "GluinoStopPolarization";
               version = "0.1.0.0";
               src = ./.;
               isLibrary = true;
               isExecutable = true;
               buildDepends = [
                 attoparsec base bytestring containers directory double-conversion
                 filepath HDBC HDBC-sqlite3 lhef-tools optparse-applicative process
                 statistics transformers vector
               ];
               description = "Top polarization study in Gluino decays";
               license = stdenv.lib.licenses.gpl3;
             }) {};
in
  pkg.env
