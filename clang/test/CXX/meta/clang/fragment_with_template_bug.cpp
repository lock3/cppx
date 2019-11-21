// RUN: %clang_cc1 -freflection -std=c++2a %s

template<typename T>
struct struct_s {
  consteval {
    int a = 1;
    -> __fragment struct {
      template<typename Q>
      void unqualid("foo_", a)() {
      }
    };
  }
};

int main() {
  struct_s<int> s;
  s.foo_1<int>();
  return 0;
}
