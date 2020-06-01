{ mkDerivation, base, base58-bytestring, bytestring, conduit
, containers, cryptonite, exceptions, fetchgit, filepath, hspec
, hspec-discover, hspec-expectations, hspec-need-env, http-client
, http-client-tls, interpolatedstring-perl6, memory, minio-hs
, QuickCheck, safe, sqlite-simple, stdenv, store
, string-conversions, text, unliftio
}:
mkDerivation {
  pname = "micro-dal";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/ivanovs-4/micro-dal";
    sha256 = "1q2dyhf3kvksr3cxmq1iczg10na8rpac7ah4n1f1rys94j81z0pg";
    rev = "79b9e534fc93de351394b39935ca7d93cee1e63d";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base base58-bytestring bytestring conduit cryptonite exceptions
    filepath http-client http-client-tls interpolatedstring-perl6
    memory minio-hs safe sqlite-simple store string-conversions text
    unliftio
  ];
  testHaskellDepends = [
    base containers hspec hspec-discover hspec-expectations
    hspec-need-env QuickCheck store string-conversions text
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "http://github.com/hexresearch/micro-dal#readme.md";
  description = "Lightweight Data Access Layer";
  license = stdenv.lib.licenses.bsd3;
}
