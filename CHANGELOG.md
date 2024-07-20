# CHANGELOG

## 0.1.1

### New Features
- added vector operations to `math/vec.lisp`
- added primitive draw cubic bezier curve in `primitives.lisp` using polynomial form of the cubic bezier
- primitives now have variants (e.g. `point*`) that takes simple-vectors instead of flat arguments

### Refactorings
- 2D render calls have been moved to file `primitives.lisp`
- transformation related code has been moved to `transformations.lisp`
- gl related code has been moved to `gl.lisp`
- a subpackage `math` has been created for better organization of code


### Breaking
These methods have been removed:
- `vec-mul-scalar` from `math.lisp`
- `half` from `math.lisp`
