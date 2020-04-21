# For Macros

## operator'..'
Is operator'..' inclusive or non-inclusive?

## block-for macros
Can block-for macros contain conditions to 'short-circuit' the loop?

For example:
```
for:
  x in xs
  x != 10
```

might `break` or `continue` when `x` is equal to 10.

We have also restricted a single range-variable per loop.
