// RUN: %clang_cc1 -fsyntax-only -verify -freflection -std=c++2a %s

class ClassWithArr {
  int a_arr[10];
};

class ClassWithPtr {
  int *a_ptr;
};

class ArrayDestructure {
  consteval {
    int a = 0;
    int &a_ref = a; // expected-note {{'a_ref' declared here}}
    int *a_ptr = &a; // expected-note {{'a_ptr' declared here}}

    ClassWithArr a_arr_class;
    ClassWithPtr a_ptr_class; // expected-note {{'a_class' declared here}}
    -> __fragment struct {
      constexpr auto val_a() {
        return a;
      }

      constexpr auto val_a_ref() {
        return a_ref; // expected-error {{reference to local variable 'a_ref' declared in enclosing function 'ArrayDestructure::__constexpr_decl'}}
      }

      constexpr auto val_a_ptr() {
        return a_ptr; // expected-error {{reference to local variable 'a_ptr' declared in enclosing function 'ArrayDestructure::__constexpr_decl'}}
      }

      constexpr auto val_a_arr_class() {
        return a_arr_class;
      }

      constexpr auto val_a_ptr_class() {
        return a_ptr_class; // expected-error {{reference to local variable 'a_class' declared in enclosing function 'ArrayDestructure::__constexpr_decl'}}
      }
    };
  }
};
