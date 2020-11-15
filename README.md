# fp-testsuite
Repository with testcases for all homework assignments for 
Functional Programming, fall semester 2020 
([TU Wien](https://www.tuwien.at/en/))

## How to use

### Method 1
There is a [PDF](https://www.complang.tuwien.ac.at/knoop/lehre/ws20/fp185A03/Fprog.pdf)
on the [courses website](https://www.complang.tuwien.ac.at/knoop/fp185A03_ws2021.html)
in which Page 3 provides instruction on how to test with cabal.
Just follow those steps and later replace the files `TestSuiteXY.hs` 
with the same files from this repository.

### Method 2

Add a `.cabal` (e.g. fprog.cabal) file to your directory with the following contents:
```yaml
cabal-version: 2.2
name:          fprog
version:       0.2.0.0
build-type:    Simple

test-suite ffp
    type:                exitcode-stdio-1.0
    main-is:             TestSuite5.hs -- TestSuite file
    other-modules:       Angabe5 -- Angabe File
    build-depends:       base,
                         tasty,
                         tasty-hunit
    default-language:    Haskell2010
```
> All the files should be in the same directory

You have to change the TestSuite a bit, since we won't use the `Main.hs` file.
Change the first line of `TestSuiteX.hs` to:
```haskell
module Main where
```
and add the following lines:
```haskell
main :: IO ()
main = defaultMain spec
```
Now you should be able to run the test with `cabal test`

## Be decent!
This repository is managed by your [fellow students](https://github.com/fsbier/fp-testsuite/graphs/contributors)
, so don't expect full
testsuite on the day we get the assignment. However, feel free to contribute 
your own testcases with a Pull request.
