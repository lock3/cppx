# LLVM Guidelines

We use the LLVM styleguide whenever possible. Read through it here: <br />
https://llvm.org/docs/CodingStandards.html
<br />You are expected to be familiar with this styleguide.

We make some modifications in the name of consistency:
 - variable names have CapitalCasing
 - function names have camelCasing()

**We use spaces, not tabs.**

LLVM follows a strict 80-character limit. When it comes to splitting lines,
always try to split at the most readable point.

Important notes:
Parentheses touch function names, as in `f()`. Parentheses do _not_ touch
control structures like<br /> `if (condition)`. <br />They are not functions.

Braces come inline, except in front of `class` declarations.
```C++
class OutOfLineBrace
{
};

void InLineBrace() {
}
```

Do not use curly braces when you don't have to, such as in single-line control
structures.

```C++
if (true)
  ; // no need for braces
```

There is an exception: a one-line `else if` in a cascade of `if` statements
should be consistent.
```C++
if (condition) {
  1;
  2;
} else if (condition) {
  1;
  2;
} else {
  1;
} // put an unnecessary brace here for consistency and clarity
```

# General Guidelines

Use a single line of whitespace to group chunks of related code in a function.
```C++
  Expression LHS = elaborateExpr(LHSSyntax);
  if (LHS.is<clang::TypeSourceInfo *>() || LHS.isNull()) {
    SemaRef.Diags.Report(LHSSyntax->getLoc(),
                         clang::diag::err_expected_expression);
    return nullptr;
  }

  Expression RHS = elaborateExpr(RHSSyntax);
  if (RHS.is<clang::TypeSourceInfo *>() || RHS.isNull()) {
    SemaRef.Diags.Report(RHSSyntax->getLoc(),
                         clang::diag::err_expected_expression);
    return nullptr;
  }
```

This is considerably more readable than one wall of text.

We borrow some philosophical practices from the Linux styleguide:
https://www.kernel.org/doc/html/v4.10/process/coding-style.html

Namely:

 - Functions should fit on a standard 1080-pixel high screen. Due to the
   different nature of compilers and kernels, we cannot always follow this
   guideline, but use your best judgment. If a function is longer than a screen,
   break it up into static functions or private methods.
 - Avoid deep nesting. If you are nesting `if` statements 4 or 5 levels deep,
   you are probably doing something wrong. Do you need a null check, or could
   you just use `dyn_cast_or_null` ?

# Comments
Comments should be clear and concise. _They are not your diary._ Every other
programmer working on Gold, and every programmer who _might_ work on Gold
has to be able to understand them. If you are putting terms like "I think"
or "the issue I had before" in your comments, you are misusing them.

# Assertions and Unreachables
As the LLVM styleguide mentions, assert liberally. Keep in mind that assertions
are for things you _know_ are true, not things you _wish_ to be true.

Do not commit `assert(false)`. Use `llvm_unreachable` instead. Unreachables
should be clear and concise and _understandable to an end-user who will never
see this codebase._ **They should not be longer than one 80-character line.**

Replace multi-line unreachables with TODO or FIXME comments.

Do not carelessly add unreachables or assertions to code you did not write.