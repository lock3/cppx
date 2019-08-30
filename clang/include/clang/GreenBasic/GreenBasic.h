#ifndef CLANG_GREEN_GREENBASIC_H
#define CLANG_GREEN_GREENBASIC_H

#define usyn_noinline __declspec(noinline)

namespace usyntax {

// Numeric types.
using intp = long long;
using nat32 = unsigned long;
using nat16 = unsigned short;
using nat8 = unsigned char;
using char8 = unsigned char;

// Basic functions.
template <class t, intp n>
constexpr intp array_size(t (&)[n]) noexcept {
  return n;
}

// Basic data structures.
struct snippet_t {
  intp start_line, start_ofs, end_line, end_ofs;
};

struct parse_err_t {
  snippet_t snip;
  const char *code, *message;
};

template <class t>
struct link_t {
  t value;
  link_t *next;
};

} // namespace usyntax

#endif
