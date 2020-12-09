// RUN: %clang_cc1 -std=c++2a -freflection -verify %s

namespace meta {
  using info = decltype(reflexpr(void));
}

class meta_type_class {
  meta::info var;
};

template<typename T>
class meta_type_templ_class_a {
  T var;
};

template<int>
class meta_type_templ_class_b {
  meta::info var;
};

int main() {
  {
    meta::info reflection; // expected-error {{meta type variables may only appear in manifestly constant evaluated contexts}}
  }
  {
    auto reflection = reflexpr(void); // expected-error {{meta type variables may only appear in manifestly constant evaluated contexts}}
  }
  {
    meta::info reflection = reflexpr(void); // expected-error {{meta type variables may only appear in manifestly constant evaluated contexts}}
  }
  {
    meta::info* reflection_ptr; // expected-error {{meta type variables may only appear in manifestly constant evaluated contexts}}
  }
  {
    constexpr meta::info reflection = reflexpr(void);
    const meta::info& reflection_ptr = reflection; // expected-error {{meta type variables may only appear in manifestly constant evaluated contexts}}
  }
  {
    meta::info reflection_ptr [1]; // expected-error {{meta type variables may only appear in manifestly constant evaluated contexts}}
  }
  {
    meta_type_class tc; // expected-error {{meta type variables may only appear in manifestly constant evaluated contexts}}
  }
  {
    meta_type_templ_class_a<meta::info> tc; // expected-error {{meta type variables may only appear in manifestly constant evaluated contexts}}
  }
  {
    meta_type_templ_class_b<1> tc; // expected-error {{meta type variables may only appear in manifestly constant evaluated contexts}}
  }
  return 0;
}
