# Basics
Usyntax can be used as a language with significant whitespace, or C-style braces.
Semicolons are optional.

```
if(1):
  1                      # semicolon left off
else:
  0;                     # semicolon used
```

```
if(1) {
  1;
} else {
  0;
}
```

We can declare variables with an automatically deduced type or an explicit
type. If we do not initialize the deduced-type variable, it will be deduced
upon initialization.

Note that we explicitly type a variable with the : operator.
```
x                       # x has undeduced type
y : int                 # y has int type
x = 3.14                # x deduces to type double
```

We can apply the same logic to functions and function parameters. Functions
can have an automatically deduced return type, or an explicit return type.

Functions are defined using the ! operator.
```
f(x:int):int!            # f takes an int parameter and returns an int.
  returns x

g(z)!                    # g will have the return type that is the type of
  returns z              # of parameter z.

```


# Macros
Usyntax operates on chainable macros that follow the form [keyword] { block }.
There are some reserved macros such as if, else, for, and while, but macros
can have user-defined keywords as well. 

### If Macro
There are a variety of different ways to write a conditional macro:

```
if (x < 10) {
  do_something();
}
```

```
if (x < 10):
  do_something();
```

We can use the then operator to create a single-line conditional statement
with no braces.
```
if (x < 10) then do_something();
```

Or we can use a "block condition" followed by a "then block" to evaluate
multiple conditions followed by an executable block.
```
if:
      x < 10
      x > 5
then:
      do_something();
```

TODO: can we do block conditions with braces?
```
if {
      x < 10
      x > 5
} then {
      do_something();
}
```

### For Macro
For loops have a language-defined macro syntax. The variations on for macros
are quite similar to conditional macros.

Examples:

```
for (x : 0..99) do do_something()
```

```
for (x : 0..99) {
  do_something();
}

```
for:
      x : 0..99
      y = x * x
do:
      do_something(y)
```
