# j-hs

Call [J](https://www.jsoftware.com) from Haskell.

Features a [repa](https://hackage.haskell.org/package/repa) interface.

## Example

Suppose we wish to perform linear regression. In J we could do:

```
xs := 1 2 3
ys := 2 4 6

reg_result =: ys %. xs ^/ i.2
```
