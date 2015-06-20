# ChangeLog / ReleaseNotes


## Version 0.6.1.2

* Bumping up dependency on [MonadRandom][] to include `0.4`. (change, affects
  only benchmark and test-suite)
* Adding sensible upper bound to [deepseq][] package. (change, affects
  only benchmark)


## Version 0.6.1.1

* Cleanup of version dependencies, most importantly:
    * Bumped transformers dependency from `< 0.4` to `0.5`.
    * Bumped criterion minimal version to 1.0.0.0. This required update of
      benchmarking code.
* Documentation updates.
* Uploaded to [Hackage][]:
  <http://hackage.haskell.org/package/apache-md5-0.6.1.1>


## Version 0.6.1.0

* Function `mkSalt` rejects empty `ByteString` and returns `Nothing` for it.
* Using `NoImplicitPrelude` language extension.
* Example program is once again compilable.
* Minor documentation updates including `README.md`.
* Uploaded to [Hackage][]:
  <http://hackage.haskell.org/package/apache-md5-0.6.1.0>


## Version 0.6.0.0

* Introducing Salt newtype wrapper to guarantee that it consists of only
  characters that can be used in htpasswd entry.
* Algorithm implementation details and helper functions were all moved to
  `Data.Digest.ApacheMD5.Internal` module to make it explicit that they aren't
  part of stable API.
* Exposing `Data.Digest.ApacheMD5.Internal` module so that library authors
  still have the ability to get the most out of this package.
* Updated `README.md`, `example.hs`, tests and benchmarks.
* Uploaded to [Hackage][]:
  <http://hackage.haskell.org/package/apache-md5-0.6.0.0>


## Version 0.5.0.1

Release date: **2013-07-27 10:38 +0200**

* Minor release with mostly documentation updates.
* Introducing `example.hs` that creates htpasswd like entry and prints it to
  stdout. (new)
* Updated `README.md` with reference to [Hackage][] and example mentioned
  above.
* Introducing this ChangeLog / ReleaseNotes file. (new)
* Clean up of benchmark dependencies.
* Uploaded to [Hackage][]:
  <http://hackage.haskell.org/package/apache-md5-0.5.0.1>


## Version 0.5.0.0

Release date: **2013-07-26 20:29 +0200**

* First public release.
* Uploaded to [Hackage][]:
  <http://hackage.haskell.org/package/apache-md5-0.5.0.0>


[Hackage]:
  http://hackage.haskell.org/
  "HackageDB (or just Hackage) is a collection of releases of Haskell packages."
[MonadRandom]:
  http://hackage.haskell.org/package/MonadRandom
  "Hackage: MonadRandom package."
[deepseq]:
  http://hackage.haskell.org/package/deepseq
  "Hackage: deepseq package."
