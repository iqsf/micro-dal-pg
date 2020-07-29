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
    url = "https://github.com/iqsf/micro-dal";
    sha256 = "06ib3lf5f1pxrlw6cxm2hcqfpb13qarcnffx6jw8c4jhhy1y3chm";
    rev = "4e22900ba30944bf1c12e56afaed7978ac1713ba";
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
