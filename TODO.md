# Upstream
- [ ] Failure on multithreaded test suite (haskell mailing list)
- [ ] tasty failure on ghc 8.10.2?? wtf
  ```
  Uses J for something Haskell would have a hard time with: j-test: internal error: index out of bounds
CallStack (from HasCallStack):
  error, called at ./Test/Tasty/Ingredients/ConsoleReporter.hs:178:20 in tasty-1.3.1-268a33715c1414c235611995fa9c09de6e68f7c1c3aa3455f08d0451da953e2d:Test.Tasty.Ingredients.ConsoleReporter
  ```
  ```
  J dl
    Performs calculation and has sensible output:             OK
    Reads back type in the environment:                       OK
    Reads a string:                                           OK
    Sends an array to J:                                      OK
    Uses J to perform a complex calculation:                  OK
    Writes strigns to J values:
  Test suite j-test: FAIL
  ```
