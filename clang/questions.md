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

# Syntax questions

How do we express a global namespace for a qulified name?

Do we have a lambda representation?

Which operator is address of?

# Classes, Constructors, and destructor

Is the following syntax acceptable for constructors and destructors?
```
c : type = class:
  constructor() : void!
    # something
  destructor() : void!
    #body
```

# Semantic Questions

What do each of the following operators actually do?
```
a?
?a
a@
:=
a=>b
```

In the example given here 
```
[editable_slider(0.0f,1.0f)]
Health:float
```
What does the editable_slider attribute look like when it's not an attribute?
Is it a class that's defined in the language somewhere? Is it defined in C++?


