# Revision history for group-theory

## 0.2.2.0

* Bump `groups` bound to `0.5.3` for `generated'` bugfix.
* Update documentation and doctests

## 0.2.1.0

* Support for GHC 9.0
* Fixed mempty instance for `Abelianizer`
* Fix warning for redundant semigroup import

## 0.2.0.0

* Depend on the `groups` package ([#19](https://github.com/emilypi/group-theory/pull/19) - thanks to @taneb for providing the package!)
* Added `FreeProduct` ([#13](https://github.com/emilypi/group-theory/pull/13))
* Removed unsound `Group` instances ([#14](https://github.com/emilypi/group-theory/pull/14))
* Add the `GroupOrder` typeclass for order calculations ([#20](https://github.com/emilypi/group-theory/pull/20))
* Fixed 'Permutation' instances ([#21](https://github.com/emilypi/group-theory/pull/21))
* Bugfixes for `FreeAbelianGroup`, allowing it to handle sparsity more robustly, as well as handling
  `mempty` values on construction with a pattern synonym.
* Typo fixes and documentation updates ([#16](https://github.com/emilypi/group-theory/pull/16), [#17](https://github.com/emilypi/group-theory/pull/17))

## 0.1.0.0

* First version. Released on an unsuspecting world.
