#include "clang/Green/Tokens.h"

char const* token::spelling() const
{
  if (kind == 0)
    return "<end of input>";
  return sym.data();
}

