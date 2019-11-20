set(LLVM_LINK_COMPONENTS
  Support
  )

add_clang_library(greenBasic
  File.cpp
  Symbol.cpp
  Tokens.cpp

  LINK_LIBS
  clangBasic
)
